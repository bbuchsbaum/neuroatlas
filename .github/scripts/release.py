"""Publish the approved 0.2.0 slice only after checks on the default branch."""

import json
import os
from pathlib import Path
import re
import urllib.error
import urllib.parse
import urllib.request


REQUIRED_WORKFLOWS = {
    ".github/workflows/check_on_main.yml",
    ".github/workflows/check_on_different_r_os.yml",
    ".github/workflows/pkgdown.yaml",
}
APPROVED_VERSION = "0.2.0"


def release_gate(repository, sha, branch, default_branch, head_sha, runs):
    if branch != default_branch or sha != head_sha:
        return False
    for path in REQUIRED_WORKFLOWS:
        matching = [run for run in runs if (
            run.get("path") == path and run.get("head_sha") == sha
            and run.get("head_branch") == default_branch
            and run.get("event") == "push"
            and run.get("head_repository", {}).get("full_name") == repository
        )]
        if not matching:
            return False
        latest = max(matching, key=lambda run: run["id"])
        if latest.get("status") != "completed" or latest.get("conclusion") != "success":
            return False
    return True


def release_notes(description, news):
    version = re.search(r"^Version:\s*(\S+)\s*$", description, re.MULTILINE)
    if not version or version[1] != APPROVED_VERSION:
        return None
    sections = re.split(r"^# neuroatlas ", news, flags=re.MULTILINE)
    if len(sections) < 2 or sections[1].splitlines()[0] != APPROVED_VERSION:
        raise ValueError("NEWS does not describe the approved package version.")
    return sections[1].partition("\n")[2].strip()


def api_client(repository):
    def request(path, data=None, missing_ok=False):
        url = "https://api.github.com/repos/" + repository + "/" + path
        headers = {
            "Authorization": "Bearer " + os.environ["GITHUB_TOKEN"],
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
        }
        body = None if data is None else json.dumps(data).encode("utf-8")
        req = urllib.request.Request(url, data=body, headers=headers)
        try:
            with urllib.request.urlopen(req, timeout=60) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            if missing_ok and error.code == 404:
                return None
            raise RuntimeError(f"GitHub request failed: HTTP {error.code}") from None
    return request


def tag_commit(api, ref):
    obj = ref["object"]
    while obj["type"] == "tag":
        obj = api("git/tags/" + obj["sha"])["object"]
    if obj["type"] != "commit":
        raise ValueError("Release tag does not reference a commit.")
    return obj["sha"]


def prepare_tag(api, sha, notes):
    tag = "v" + APPROVED_VERSION
    ref = api("git/ref/tags/" + tag, missing_ok=True)
    if ref is not None:
        if tag_commit(api, ref) != sha:
            print("The approved version is already tagged elsewhere; skipping.")
            return None
    else:
        api("git/refs", {"ref": "refs/tags/" + tag, "sha": sha})
    Path("body.md").write_text(notes + "\n", encoding="utf-8")
    return tag


def main():
    event = json.loads(Path(os.environ["GITHUB_EVENT_PATH"]).read_text())
    repository = os.environ["GITHUB_REPOSITORY"]
    run = event.get("workflow_run", {})
    repo = event["repository"]
    if (os.environ["GITHUB_EVENT_NAME"] != "workflow_run"
            or run.get("conclusion") != "success"
            or run.get("head_repository", {}).get("full_name") != repository
            or run.get("head_branch") != repo["default_branch"]):
        print("Release event is outside the approved default-branch scope.")
        return
    sha = run["head_sha"]
    api = api_client(repository)
    branch = urllib.parse.quote(repo["default_branch"], safe="")
    head = api("branches/" + branch)["commit"]["sha"]
    runs = []
    page = 1
    while True:
        batch = api(f"actions/runs?head_sha={sha}&per_page=100&page={page}")["workflow_runs"]
        runs.extend(batch)
        if len(batch) < 100:
            break
        page += 1
    if not release_gate(repository, sha, run["head_branch"],
                        repo["default_branch"], head, runs):
        print("Required checks are pending, failed, or for a different commit.")
        return
    notes = release_notes(Path("DESCRIPTION").read_text(), Path("NEWS.md").read_text())
    if notes is None:
        print("Package version is outside this approved release.")
        return
    # Recheck the branch immediately before the only remote mutation.
    if api("branches/" + branch)["commit"]["sha"] != sha:
        print("Default branch moved during release preparation; skipping.")
        return
    tag = prepare_tag(api, sha, notes)
    if tag:
        with open(os.environ["GITHUB_OUTPUT"], "a", encoding="utf-8") as output:
            output.write("tag=" + tag + "\n")


if __name__ == "__main__":
    main()

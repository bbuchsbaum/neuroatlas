#!/usr/bin/env python3
"""Fetch only locked surface assets; --offline verifies without network access."""

import argparse
import hashlib
import json
from pathlib import Path
import tarfile
import urllib.request


def verify(data, entry):
    if len(data) != entry["size_bytes"]:
        raise ValueError("Size mismatch: " + entry.get("path", entry.get("id", "")))
    if hashlib.sha256(data).hexdigest() != entry["sha256"]:
        raise ValueError("SHA-256 mismatch: " + entry.get("path", entry.get("id", "")))


def destination(root, name):
    path = root / name
    if Path(name).name != name or path.is_symlink():
        raise ValueError("Expected a plain filename: " + name)
    return path


def download(entry):
    with urllib.request.urlopen(entry["url"], timeout=120) as response:
        data = response.read()
    verify(data, entry)
    return data


def fetch(lock, root, offline=False):
    root.mkdir(parents=True, exist_ok=True)
    archives = {a["id"]: a for a in lock["archives"]}
    archive_cache = root.parent / "archives"
    for entry in lock["assets"]:
        path = destination(root, entry["path"])
        if path.exists():
            verify(path.read_bytes(), entry)
            continue
        if offline:
            raise FileNotFoundError(path)
        if "archive" in entry:
            archive = archives[entry["archive"]]
            archive_cache.mkdir(exist_ok=True)
            archive_path = destination(archive_cache, archive["id"] + ".tar.gz")
            if archive_path.exists():
                verify(archive_path.read_bytes(), archive)
            else:
                with archive_path.open("xb") as handle:
                    handle.write(download(archive))
            with tarfile.open(archive_path) as handle:
                member = handle.getmember(entry["member"])
                if not member.isfile():
                    raise ValueError("Archive member must be a regular file")
                # Read the named member only; never extract archive paths.
                data = handle.extractfile(member).read()
        else:
            data = download(entry)
        verify(data, entry)
        with path.open("xb") as handle:
            handle.write(data)
    return len(lock["assets"])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("destination", type=Path)
    parser.add_argument("--lock", type=Path,
                        default=Path(__file__).with_name("inputs.lock.json"))
    parser.add_argument("--offline", action="store_true")
    args = parser.parse_args()
    count = fetch(json.loads(args.lock.read_text()), args.destination, args.offline)
    print(json.dumps({"verified_assets": count, "offline": args.offline}))

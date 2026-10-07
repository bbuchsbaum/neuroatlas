import copy
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("release", Path(__file__).with_name("release.py"))
release = importlib.util.module_from_spec(spec)
spec.loader.exec_module(release)


class ReleaseGateTests(unittest.TestCase):
    def setUp(self):
        self.runs = [{
            "id": i, "path": path, "head_sha": "a" * 40,
            "head_branch": "master", "event": "push",
            "head_repository": {"full_name": "bbuchsbaum/neuroatlas"},
            "status": "completed", "conclusion": "success",
        } for i, path in enumerate(sorted(release.REQUIRED_WORKFLOWS), 1)]

    def gate(self, runs=None, **changes):
        args = dict(repository="bbuchsbaum/neuroatlas", sha="a" * 40,
                    branch="master", default_branch="master", head_sha="a" * 40,
                    runs=self.runs if runs is None else runs)
        args.update(changes)
        return release.release_gate(**args)

    def test_all_exact_commit_checks_pass(self):
        self.assertTrue(self.gate())

    def test_wrong_branch_or_moved_head(self):
        self.assertFalse(self.gate(branch="feature"))
        self.assertFalse(self.gate(head_sha="b" * 40))

    def test_missing_or_untrusted_checks(self):
        for field, value in [
            ("head_sha", "b" * 40), ("head_branch", "feature"),
            ("event", "pull_request"), ("status", "in_progress"),
            ("conclusion", "failure"), ("conclusion", "skipped"),
            ("head_repository", {"full_name": "fork/neuroatlas"}),
        ]:
            with self.subTest(field=field, value=value):
                runs = copy.deepcopy(self.runs)
                runs[0][field] = value
                self.assertFalse(self.gate(runs))
        self.assertFalse(self.gate(self.runs[:-1]))

    def test_latest_failed_attempt_overrides_old_success(self):
        newer = dict(self.runs[0], id=99, conclusion="failure")
        self.assertFalse(self.gate(self.runs + [newer]))

    def test_only_approved_version_and_matching_news(self):
        self.assertEqual(release.release_notes("Version: 0.2.0\n",
                         "# neuroatlas 0.2.0\n\nNew feature.\n\n# neuroatlas 0.1.0\nOld."),
                         "New feature.")
        for version in ["0.2.0.9001", "0.3.0", "0.1.0"]:
            self.assertIsNone(release.release_notes("Version: " + version, ""))
        with self.assertRaises(ValueError):
            release.release_notes("Version: 0.2.0", "# neuroatlas 0.1.0\nOld.")

    def test_tag_is_never_overwritten(self):
        calls = []
        def api(path, data=None, missing_ok=False):
            calls.append((path, data))
            return {"object": {"type": "commit", "sha": "b" * 40}}
        self.assertIsNone(release.prepare_tag(api, "a" * 40, "Notes"))
        self.assertTrue(all(data is None for _, data in calls))

    def test_create_or_replay_tag_at_verified_commit(self):
        for existing in [None, {"object": {"type": "commit", "sha": "a" * 40}}]:
            calls = []
            def api(path, data=None, missing_ok=False):
                calls.append((path, data))
                return existing
            with tempfile.TemporaryDirectory() as folder:
                with patch.object(release, "Path", lambda path: Path(folder) / path):
                    self.assertEqual(release.prepare_tag(api, "a" * 40, "Notes"), "v0.2.0")
                self.assertEqual((Path(folder) / "body.md").read_text(), "Notes\n")
            writes = [data for _, data in calls if data is not None]
            self.assertEqual(writes, [] if existing else [{"ref": "refs/tags/v0.2.0", "sha": "a" * 40}])


if __name__ == "__main__":
    unittest.main()

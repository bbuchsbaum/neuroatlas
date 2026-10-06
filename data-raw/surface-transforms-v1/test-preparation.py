#!/usr/bin/env python3
"""Offline regression checks for integrity failures and smoke acceptance."""

import base64
import hashlib
import importlib.util
import io
from pathlib import Path
import struct
import tarfile
import tempfile
import unittest
import zlib


def load(name):
    spec = importlib.util.spec_from_file_location(
        name, Path(__file__).with_name(name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


fetcher = load("fetch-inputs")
smoke = load("workbench-smoke")


class IntegrityTests(unittest.TestCase):
    def test_named_archive_member_and_hash(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            archive = root / "upstream.tar.gz"
            content = b"mask fixture"
            with tarfile.open(archive, "w:gz") as handle:
                member = tarfile.TarInfo("atlases/mask.gii")
                member.size = len(content)
                handle.addfile(member, io.BytesIO(content))
                link = tarfile.TarInfo("atlases/unsafe.gii")
                link.type = tarfile.SYMTYPE
                link.linkname = "/outside"
                handle.addfile(link)
            archive_entry = {"id": "test", "url": archive.as_uri(),
                             "size_bytes": archive.stat().st_size,
                             "sha256": hashlib.sha256(archive.read_bytes()).hexdigest()}
            entry = {"path": "mask.gii", "archive": "test",
                     "member": "atlases/mask.gii", "size_bytes": len(content),
                     "sha256": hashlib.sha256(content).hexdigest()}
            lock = {"archives": [archive_entry], "assets": [entry]}
            self.assertEqual(fetcher.fetch(lock, root / "inputs"), 1)
            self.assertEqual((root / "inputs/mask.gii").read_bytes(), content)
            entry.update(path="unsafe.gii", member="atlases/unsafe.gii")
            with self.assertRaisesRegex(ValueError, "regular file"):
                fetcher.fetch(lock, root / "inputs")

    def test_offline_corruption_and_missing_fail(self):
        content = b"pinned input"
        entry = {"path": "input.gii", "size_bytes": len(content),
                 "sha256": hashlib.sha256(content).hexdigest()}
        lock = {"archives": [], "assets": [entry]}
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            with self.assertRaises(FileNotFoundError):
                fetcher.fetch(lock, root, offline=True)
            (root / "input.gii").write_bytes(content)
            self.assertEqual(fetcher.fetch(lock, root, offline=True), 1)
            (root / "input.gii").write_bytes(b"corrupted!!!")
            with self.assertRaisesRegex(ValueError, "SHA-256"):
                fetcher.fetch(lock, root, offline=True)

    def test_unsafe_paths_fail(self):
        with tempfile.TemporaryDirectory() as tmp:
            for name in ("../outside", "/absolute", "nested/file"):
                with self.assertRaises(ValueError):
                    fetcher.destination(Path(tmp), name)
            link = Path(tmp) / "link"
            link.symlink_to("absent")
            with self.assertRaises(ValueError):
                fetcher.destination(Path(tmp), "link")


class SmokeTests(unittest.TestCase):
    def test_explicit_workbench_unassigned_label(self):
        xml = ('<GIFTI><LabelTable><Label Key="0" Alpha="0">NAME</Label>'
               '<Label Key="11" Alpha="1">cortex</Label></LabelTable>'
               '<DataArray Dimensionality="1" Dim0="2" Encoding="ASCII">'
               '<Data>0 11</Data></DataArray></GIFTI>')
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "labels.gii"
            path.write_text(xml.replace("NAME", "label_0"))
            with self.assertRaisesRegex(ValueError, "unassigned key 0"):
                smoke.validate_label_fixture(path, [0, 11])
            path.write_text(xml.replace("NAME", "???"))
            smoke.validate_label_fixture(path, [0, 11])
            path.write_text(xml.replace("NAME", "???").replace("0 11", "0 12"))
            with self.assertRaisesRegex(ValueError, "absent from label table"):
                smoke.validate_label_fixture(path, [0, 11])

    def test_compressed_gifti_reader(self):
        values = [0.0, 7.0, 7.0]
        data = base64.b64encode(zlib.compress(struct.pack("<3f", *values))).decode()
        xml = ('<GIFTI><DataArray Dimensionality="1" Dim0="3" '
               'Encoding="GZipBase64Binary" DataType="NIFTI_TYPE_FLOAT32" '
               'Endian="LittleEndian"><Data>' + data +
               '</Data></DataArray></GIFTI>')
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "metric.gii"
            path.write_text(xml)
            self.assertEqual(smoke.metric_values(path), values)

    def test_acceptance_rejects_wrong_hemi_and_mask_leak(self):
        valid = [0, 1, 1]
        result = smoke.check_case([0, 7, 7], [0, 11, 12], valid, valid,
                                  3, 7, [0, 11, 12])
        self.assertEqual(result["covered_cortical_vertices"], 2)
        failures = [([0, 13, 13], [0, 11, 12]),
                    ([7, 7, 7], [0, 11, 12]),
                    ([0, 7, 7], [0, 21, 22]),
                    ([0, 7, 7], [0, 11.5, 12]),
                    ([0, 7, 7], [0, 0, 0])]
        for metric, labels in failures:
            with self.assertRaises(ValueError):
                smoke.check_case(metric, labels, valid, valid, 3, 7, [0, 11, 12])
        with self.assertRaisesRegex(ValueError, "No cortical"):
            smoke.check_case([0, 0, 0], [0, 11, 12], [0, 0, 0], valid,
                             3, 7, [0, 11, 12])


if __name__ == "__main__":
    unittest.main()

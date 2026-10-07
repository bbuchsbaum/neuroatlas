#!/usr/bin/env python3
"""Reject changes to projection manifests, coordinates, masks and sample data."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile

source = Path(sys.argv[1]).resolve()
receipt = json.loads((source / "consumer-receipt.json").read_text())
oracle = Path(__file__).with_name("oracle.py").resolve()
first = receipt["cases"][0]
manifest = json.loads((source / first / "case.json").read_text())
paths = [Path(first) / name for name in
         ("case.json", "points-1.bin", "cortex.bin", "values.bin")]
paths.append(Path(manifest["volume"]))
with tempfile.TemporaryDirectory(prefix="projection-evidence-") as tmp:
    target = Path(tmp)
    for path in source.iterdir():
        if path.name in receipt["cases"]:
            (target / path.name).mkdir()
            for item in path.iterdir():
                if item.is_file():
                    (target / path.name / item.name).symlink_to(item)
        elif path.is_file():
            (target / path.name).symlink_to(path)
    for relative in paths:
        path = target / relative
        path.unlink()
        path.write_bytes(b"altered projection evidence")
        result = subprocess.run([sys.executable, str(oracle), str(target)],
                                capture_output=True, text=True)
        assert result.returncode != 0 and "AssertionError" in result.stderr, (
            relative, result.stderr)
        path.unlink()
        path.symlink_to(source / relative)
        print(f"PASS: changed {relative} rejected", flush=True)

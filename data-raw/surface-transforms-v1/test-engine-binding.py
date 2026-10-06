#!/usr/bin/env python3
"""Reject a mismatched version or installed artifact before numerical work."""
import copy
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

receipt = json.loads(Path(sys.argv[1]).read_text())
script = Path(__file__).with_name("qualify-native.R").resolve()
with tempfile.TemporaryDirectory(prefix="neuroatlas-binding-") as directory:
    root = Path(directory)
    for name in ("version", "artifact"):
        changed = copy.deepcopy(receipt)
        expected = "binding$version"
        if name == "version":
            changed["version"] = "0.0.0"
        else:
            first = next(iter(changed["installed_artifacts_sha256"]))
            changed["installed_artifacts_sha256"][first] = "0" * 64
            expected = "Installed engine artifact differs from build receipt"
        binding = root / f"{name}.json"
        binding.write_text(json.dumps(changed))
        environment = dict(os.environ, NEUROATLAS_ENGINE_BINDING=str(binding))
        run = subprocess.run(
            ["Rscript", str(script), str(root / name)],
            env=environment, capture_output=True, text=True,
        )
        assert run.returncode != 0 and expected in run.stderr, run.stderr
        assert not (root / name / "consumer-receipt.json").exists()
print("PASS: mismatched version and altered installed bytes rejected")

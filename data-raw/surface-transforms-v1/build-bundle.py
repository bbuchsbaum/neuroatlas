#!/usr/bin/env python3
"""Assemble a non-overwriting, hash-manifested offline smoke bundle."""

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys


def build(inputs, fixtures, output, cluster="trillium"):
    source = Path(__file__).resolve().parent
    subprocess.run([sys.executable, str(source / "fetch-inputs.py"),
                    str(inputs), "--offline"], check=True)
    lock = source / "inputs.lock.json"
    domains = json.loads((fixtures / "domains.json").read_text())
    if domains["input_lock_sha256"] != hashlib.sha256(lock.read_bytes()).hexdigest():
        raise ValueError("Fixtures were prepared against a different input lock")
    output.mkdir(parents=True, exist_ok=False)
    (output / "inputs").mkdir()
    for entry in json.loads(lock.read_text())["assets"]:
        shutil.copyfile(inputs / entry["path"], output / "inputs" / entry["path"])
    shutil.copytree(fixtures, output / "fixtures")
    for name in ("inputs.lock.json", "fetch-inputs.py", "workbench-smoke.py",
                 cluster + "-smoke.sbatch", "prepare-fixtures.R"):
        shutil.copyfile(source / name, output / name)
    files = sorted(p for p in output.rglob("*") if p.is_file())
    lines = [hashlib.sha256(p.read_bytes()).hexdigest() + "  " +
             p.relative_to(output).as_posix() for p in files]
    manifest = "\n".join(lines) + "\n"
    (output / "SHA256SUMS").write_text(manifest)
    digest = hashlib.sha256(manifest.encode()).hexdigest()
    print(json.dumps({"bundle": str(output), "manifest_sha256": digest,
                      "files": len(files)}))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", type=Path)
    parser.add_argument("fixtures", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--cluster", choices=("trillium", "nibi"),
                        default="trillium")
    args = parser.parse_args()
    build(args.inputs, args.fixtures, args.output, args.cluster)

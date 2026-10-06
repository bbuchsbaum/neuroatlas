#!/usr/bin/env python3
"""Prove that altered sample selection and policy bytes fail before acceptance."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile

source = Path(sys.argv[1]).resolve()
receipt = json.loads((source/'consumer-receipt.json').read_text())
oracle = Path(__file__).with_name('qualify-native-oracle.py').resolve()
with tempfile.TemporaryDirectory(prefix='native-evidence-') as tmp:
    target = Path(tmp)
    for name in ['consumer-receipt.json'] + [x['file'] for x in receipt['upstream_bindings']]:
        (target/name).symlink_to(source/name)
    for name in receipt['cases']:
        folder = target/name
        folder.mkdir()
        for p in (source/name).iterdir():
            if p.is_file():
                (folder/p.name).symlink_to(p)
    first = receipt['cases'][0]
    manifest_path = target/first/'case.json'
    manifest = json.loads(manifest_path.read_text())
    manifest['sampled_targets_zero_based'][0] += 1
    manifest_path.unlink()
    manifest_path.write_text(json.dumps(manifest))
    run = subprocess.run([sys.executable,str(oracle),str(target)],capture_output=True,text=True)
    assert run.returncode != 0 and "case_manifest_sha256" in run.stderr, run.stderr
    manifest_path.unlink()
    manifest_path.symlink_to(source/first/'case.json')
    real = next(x for x in receipt['cases'] if (source/x/'values.bin').exists())
    values_path = target/real/'values.bin'
    values_path.unlink()
    values_path.write_bytes(b'altered policy input')
    run = subprocess.run([sys.executable,str(oracle),str(target)],capture_output=True,text=True)
    assert run.returncode != 0 and "item['sha256']" in run.stderr, run.stderr
    print('PASS: altered sample index and altered policy input both rejected')

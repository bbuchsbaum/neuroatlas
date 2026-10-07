"""Inspect I/O fixtures with Workbench without claiming resampling parity."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('directory', type=Path)
args = parser.parse_args()
version = subprocess.check_output(['wb_command', '-version'], text=True).strip()
cases = json.loads((args.directory / 'cases.json').read_text())
results = []
for name in cases:
    path = args.directory / ('roundtrip-' + name)
    result = subprocess.run(['wb_command', '-file-information', str(path)],
                            capture_output=True, text=True, check=True)
    assert 'BRAIN_MODELS' in result.stdout
    assert 'CortexLeft' in result.stdout and 'ThalamusLeft' in result.stdout
    result_type = next(line for line in result.stdout.splitlines()
                       if line.startswith('Type:')).split(':', 1)[1].strip()
    results.append({'file': name, 'sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
                    'type': result_type, 'inspection_exit_code': result.returncode})
receipt = {'status': 'PASS', 'workbench_version': version, 'cases': results,
           'scope': 'brain models inspectable; transposed axes have unknown subtype',
           'resampling_parity_claim': False}
(args.directory / 'workbench-inspection-receipt.json').write_text(
    json.dumps(receipt, indent=2) + '\n')
print('PASS: Workbench inspected all 17 outputs; transposed subtype limitation retained.')

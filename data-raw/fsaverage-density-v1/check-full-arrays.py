#!/usr/bin/env python3
"""Independent full-array mass/masking checks on exported candidate operators."""
import hashlib
import json
import sys
from pathlib import Path
import numpy as np

base = Path(sys.argv[1])
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
contract_path = Path(__file__).with_name('contract-v1.json')
contract = json.loads(contract_path.read_text())
receipt = json.loads((base / 'consumer-receipt.json').read_text())
assert receipt['contract_sha256'] == sha(contract_path)
reports = []
for name in receipt['routes']:
    folder = base / name
    assert sha(folder / 'case.json') == receipt['case_manifest_sha256'][name]
    meta = json.loads((folder / 'case.json').read_text())
    for item in meta['files']:
        assert sha(folder / item['file']) == item['sha256']
    weights = np.fromfile(folder / 'weights.bin', '<f8').reshape(-1, 3)
    rows, cols = weights[:, 0].astype(int), weights[:, 1].astype(int)
    source = np.fromfile(folder / 'source.bin', '<f8').reshape(-1, 3)
    target = np.fromfile(folder / 'target.bin', '<f8').reshape(-1, 3)
    src_mask = np.fromfile(folder / 'source-mask.bin', '<i4').astype(bool)
    dst_mask = np.fromfile(folder / 'target-mask.bin', '<i4').astype(bool)
    output = np.fromfile(folder / 'output.bin', '<f8').reshape(-1, 5)
    assert np.isfinite(weights).all() and (weights[:, 2] > 0).all()
    mass = np.bincount(rows, weights[:, 2], minlength=len(target))
    error = float(np.max(np.abs(mass-1)))
    assert error <= contract['absolute_weight_tolerance']
    active = src_mask[cols]
    masked_mass = np.bincount(rows[active], weights[active, 2], minlength=len(target))
    available = (masked_mass > 0) & dst_mask
    assert np.array_equal(np.isfinite(output).all(axis=1), available)
    assert np.isnan(output[~available]).all()
    values = np.fromfile(folder / 'values.bin', '<f8').reshape(-1, 5)
    expected = np.full_like(output, np.nan)
    for channel in range(5):
        total = np.bincount(rows[active], weights[active, 2]*values[cols[active], channel],
                            minlength=len(target))
        expected[available, channel] = total[available]/masked_mass[available]
    value_error = float(np.max(np.abs(expected[available]-output[available])))
    assert value_error <= contract['bounded_value_tolerance']
    labels = np.fromfile(folder / 'labels.bin', '<f8')
    keys = np.unique(labels)
    totals = np.column_stack([np.bincount(rows[active & (labels[cols] == key)],
        weights[active & (labels[cols] == key), 2], minlength=len(target)) for key in keys])
    aggregate = keys[np.argmax(totals, axis=1)]
    largest_weight = np.zeros(len(target))
    np.maximum.at(largest_weight, rows[active], weights[active, 2])
    winners = active & (weights[:, 2] == largest_weight[rows])
    winner_column = np.full(len(target), len(source), dtype=int)
    np.minimum.at(winner_column, rows[winners], cols[winners])
    largest = labels[np.minimum(winner_column, len(source)-1)]
    for method, prediction in [('aggregate', aggregate), ('largest', largest)]:
        actual = np.fromfile(folder / (method+'.bin'), '<f8')
        assert np.isnan(actual[~available]).all()
        assert np.array_equal(actual[available], prediction[available])
    # Downsampling nested ico meshes must have exactly one unit contributor:
    # excluded exact vertices cannot acquire nearby support through rounding.
    if len(source) > len(target):
        assert np.array_equal(source[:len(target)], target)
        assert len(rows) == len(target)
        assert np.array_equal(rows, np.arange(len(target)))
        assert np.array_equal(cols, np.arange(len(target)))
        assert np.array_equal(weights[:, 2], np.ones(len(target)))
        assert np.array_equal(available, src_mask[:len(target)] & dst_mask)
    reports.append(dict(route=name, target_vertices=len(target),
        positive_coefficients=len(rows), maximum_row_mass_error=error,
        full_masked_value_error=value_error, label_mismatches=0,
        available=int(available.sum()), masked_or_unsupported=int((~available).sum()),
        nested_exact_selection=(len(source) > len(target))))
result = dict(status='PASS', contract_sha256=sha(contract_path),
    script_sha256=sha(__file__), consumer_receipt_sha256=sha(base/'consumer-receipt.json'),
    numpy_version=np.__version__, routes=reports)
(base/'full-array-receipt.json').write_text(json.dumps(result, indent=2)+'\n')
print(json.dumps(result, indent=2))

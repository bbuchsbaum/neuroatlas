#!/usr/bin/env python3
"""Independent SciPy interpolation, categorical and sparse-composition oracle."""
from pathlib import Path
import hashlib
import itertools
import json
import sys

import numpy as np
from scipy.interpolate import interpn
from scipy.sparse import coo_matrix


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def nodes(volume, affine, points, cortex, kind, policy):
    dims = volume.shape[:3]
    maps = volume.shape[3]
    voxel = (np.c_[points, np.ones(len(points))] @ np.linalg.inv(affine).T)[:, :3]
    inside = cortex & ((voxel >= 0) & (voxel <= np.array(dims) - 1)).all(axis=1)
    voxel[~inside] = 0
    axes = tuple(np.arange(d) for d in dims)
    values = np.zeros((len(points), maps))
    mass = np.zeros_like(values)
    missing = np.zeros_like(values, dtype=bool)
    finite = np.isfinite(volume)
    method = "nearest" if kind == "label" else "linear"
    for k in range(maps):
        filled = np.where(finite[..., k], volume[..., k], 0)
        values[:, k] = interpn(axes, filled, voxel, method=method,
                              bounds_error=False, fill_value=0)
        mass[:, k] = interpn(axes, finite[..., k].astype(float), voxel,
                            method=method, bounds_error=False, fill_value=0)
    if kind == "label":
        nearest = np.ceil(voxel - .5).astype(int)
        missing = ~finite[nearest[:, 0], nearest[:, 1], nearest[:, 2], :]
    else:
        lower = np.floor(voxel).astype(int)
        fraction = voxel - lower
        for offset in itertools.product((0, 1), repeat=3):
            offset = np.array(offset)
            positive = np.where(offset, fraction > 0, fraction < 1).all(axis=1)
            index = np.minimum(lower + offset, np.array(dims) - 1)
            missing |= positive[:, None] & ~finite[index[:, 0], index[:, 1],
                                                   index[:, 2], :]
    missing &= inside[:, None]
    mass[~inside] = 0
    with np.errstate(invalid="ignore", divide="ignore"):
        values /= mass
    available = mass > 0
    if policy != "omit":
        available &= ~missing
    values[~available] = np.nan
    return values, mass, available


def project(volume, affine, points, cortex, kind, policy):
    samples = [nodes(volume, affine, p, cortex, kind, policy) for p in points]
    mass = np.mean([s[1] for s in samples], axis=0)
    available = mass > 0 if policy == "omit" else np.all([s[2] for s in samples], axis=0)
    if kind == "label":
        values = np.full_like(mass, np.nan)
        votes = np.stack([s[0] for s in samples], axis=2)
        for row, col in np.argwhere(available):
            keys, counts = np.unique(votes[row, col, np.isfinite(votes[row, col])],
                                     return_counts=True)
            values[row, col] = keys[counts == counts.max()].min()
    else:
        numerator = np.mean([np.nan_to_num(s[0]) * s[1] for s in samples], axis=0)
        with np.errstate(invalid="ignore", divide="ignore"):
            values = numerator / mass
        values[~available] = np.nan
    return values, mass, available


def surface_compose(folder, values, kind, policy, n_target):
    weights = np.fromfile(folder / "surface-weights.bin", "<f8").reshape(-1, 3)
    rows, cols = weights[:, :2].astype(int).T
    source_mask = np.fromfile(folder / "source-mask.bin", "<i4").astype(bool)
    target_mask = np.fromfile(folder / "target-mask.bin", "<i4").astype(bool)
    raw = weights[:, 2] * source_mask[cols]
    mass = np.bincount(rows, weights=raw, minlength=n_target)
    normalized = np.divide(raw, mass[rows], out=np.zeros_like(raw), where=mass[rows] > 0)
    available = np.tile((mass > 0)[:, None], (1, values.shape[1]))
    result = np.full((n_target, values.shape[1]), np.nan)
    for k in range(values.shape[1]):
        finite = np.isfinite(values[cols, k])
        missing = np.bincount(rows, weights=normalized * ~finite,
                              minlength=n_target) > 0
        if policy != "omit":
            available[:, k] &= ~missing
        available[:, k] &= target_mask
        used = normalized * finite
        if policy == "omit":
            finite_mass = np.bincount(rows, weights=used, minlength=n_target)
            used = np.divide(used, finite_mass[rows], out=np.zeros_like(used),
                             where=finite_mass[rows] > 0)
            available[:, k] &= finite_mass > 0
        if kind != "label":
            matrix = coo_matrix((used, (rows, cols)),
                                shape=(n_target, values.shape[0])).tocsr()
            result[:, k] = matrix @ np.nan_to_num(values[:, k])
        else:
            # At most three native contributors. Accumulate each key's weight
            # independently and use the explicitly declared smallest-key tie.
            keys = np.unique(values[np.isfinite(values[:, k]), k])
            votes = np.stack([np.bincount(rows, weights=used *
                (np.nan_to_num(values[cols, k], nan=-np.inf) == key),
                minlength=n_target) for key in keys], axis=1)
            result[:, k] = keys[np.argmax(votes, axis=1)]
        result[~available[:, k], k] = np.nan
    return result, mass, available


def evaluate(root):
    contract_path = Path(__file__).with_name("contract-v1.json")
    contract = json.loads(contract_path.read_text())
    receipt_path = root / "consumer-receipt.json"
    receipt = json.loads(receipt_path.read_text())
    assert sha(contract_path) == receipt["contract_sha256"]
    cache = {}
    reports = []
    for name in receipt["cases"]:
        folder = root / name
        manifest_path = folder / "case.json"
        assert sha(manifest_path) == receipt["case_sha256"][name]
        m = json.loads(manifest_path.read_text())
        for file, expected in m["files"].items():
            assert sha(folder / file) == expected, (name, file, "changed evidence")
        path = root / m["volume"]
        if m["volume"] not in cache:
            assert sha(path) == m["volume_sha256"]
            dims = tuple(m["dimensions"])
            data = np.fromfile(path, "<f8").reshape(dims, order="F")
            if data.ndim == 3:
                data = data[..., None]
            # Retain only one large grid/type in memory.
            cache = {m["volume"]: data}
        data = cache[m["volume"]]
        points = [np.fromfile(folder / f"points-{i}.bin", "<f8").reshape(-1, 3)
                  for i in range(1, m["nodes"] + 1)]
        cortex = np.fromfile(folder / "cortex.bin", "<i4").astype(bool)
        expected, mass, available = project(data, np.array(m["affine"]), points,
                                             cortex, m["data_type"], m["na_policy"])
        if (folder / "surface-weights.bin").exists():
            expected, mass, available = surface_compose(folder, expected,
                m["data_type"], m["na_policy"], m["target_vertices"])
        actual = np.fromfile(folder / "values.bin", "<f8").reshape(-1, m["maps"])
        actual_available = np.fromfile(folder / "available.bin", "<i4").reshape(-1, m["maps"]).astype(bool)
        actual_mass = np.fromfile(folder / "mass.bin", "<f8")
        actual_mass = actual_mass.reshape(mass.shape)
        availability_mismatches = int(np.count_nonzero(actual_available != available))
        finite = available & np.isfinite(actual)
        maximum_error = float(np.max(np.abs(actual[finite] - expected[finite]), initial=0))
        label_mismatches = int(np.count_nonzero(actual[finite] != expected[finite])) if m["data_type"] == "label" else 0
        maximum_mass_error = float(np.max(np.abs(actual_mass - mass), initial=0))
        threshold = contract["acceptance"]["probability_absolute_error"] if m["data_type"] == "probability" else contract["acceptance"]["linear_absolute_error"]
        report = dict(name=name, vertices=m["target_vertices"], maps=m["maps"],
            maximum_error=maximum_error, maximum_mass_error=maximum_mass_error,
            label_mismatches=label_mismatches,
            availability_mismatches=availability_mismatches)
        reports.append(report)
        assert availability_mismatches == 0, report
        assert np.array_equal(np.isfinite(actual), available), report
        assert maximum_error <= threshold and maximum_mass_error <= 1e-12, report
        assert label_mismatches == 0, report
        print(json.dumps(report), flush=True)
    result = dict(status="PASS", contract_sha256=sha(contract_path),
        consumer_receipt_sha256=sha(receipt_path), oracle_sha256=sha(Path(__file__)),
        numpy_version=np.__version__, cases=reports)
    (root / "oracle-receipt.json").write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    evaluate(Path(sys.argv[1]))

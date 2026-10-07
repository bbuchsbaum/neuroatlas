#!/usr/bin/env python3
"""Independent exhaustive triangle oracle; no spatial tree or candidate pruning.

Each face contributes its unconstrained least-squares projection if interior,
plus all three closed segments (including endpoints). The global minimum is
selected by Euclidean distance. SVD pseudoinverses are computed independently
of neurotransform's closest-point region tests. All arrays are float64.
"""
import hashlib
import json
import sys
from pathlib import Path
import numpy as np
import mpmath as mp
refinement_path = Path(__file__).with_name("oracle-refinement-v1.json")
refinement = json.loads(refinement_path.read_text())

def precise_minimum(vertices, faces, query, candidates, digits):
    with mp.workdps(digits):
        Q = mp.matrix([mp.mpf(float(x)) for x in query])
        best = None
        for fi in sorted(candidates):
            T = [mp.matrix([mp.mpf(float(x)) for x in v]) for v in vertices[faces[fi]]]
            for j in range(3):
                if T[j] == Q:
                    return {int(faces[fi, j]): mp.mpf(1)}, mp.mpf(0)
            E = mp.matrix([[T[1][j]-T[0][j], T[2][j]-T[0][j]] for j in range(3)])
            uv = mp.lu_solve(E.T*E, E.T*(Q-T[0]))
            bary = [1-uv[0]-uv[1], uv[0], uv[1]]
            choices = []
            if min(bary) >= 0:
                choices.append(bary)
            for a, b in ((0, 1), (1, 2), (2, 0)):
                D = T[b]-T[a]
                u = max(mp.mpf(0), min(mp.mpf(1), mp.fdot(Q-T[a], D)/mp.fdot(D, D)))
                weights = [mp.mpf(0)]*3
                weights[a], weights[b] = 1-u, u
                choices.append(weights)
            for weights in choices:
                point = sum((T[j]*weights[j] for j in range(3)), mp.zeros(3, 1))
                distance = mp.fdot(Q-point, Q-point)
                if best is None or distance < best[0]:
                    best = (distance, {int(c): w for c, w in zip(faces[fi], weights) if w > 0})
        return best[1], best[0]

base = Path(sys.argv[1])
contract_path = Path(__file__).with_name('contract-v1.json')
contract = json.loads(contract_path.read_text())
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
receipt = json.loads((base / 'consumer-receipt.json').read_text())
assert receipt['contract_sha256'] == sha(contract_path)
assert receipt['engine_revision'] == contract['engine_revision']
for item in receipt['upstream_bindings']:
    assert sha(base / item['file']) == item['sha256']
for path, expected in receipt['consumer_source_sha256'].items():
    assert sha(path) == expected
tolerance = contract['absolute_weight_tolerance']
reports = []
for name in receipt['cases']:
    folder = base / name
    assert sha(folder / 'case.json') == receipt['case_manifest_sha256'][name]
    meta = json.loads((folder / 'case.json').read_text())
    for item in meta['files']:
        assert sha(folder / item['file']) == item['sha256']
    vertices = np.fromfile(folder / 'source.bin', '<f8').reshape(-1, 3)
    faces = np.fromfile(folder / 'faces.bin', '<i4').reshape(-1, 3)
    target = np.fromfile(folder / 'target.bin', '<f8').reshape(-1, 3)
    weights = np.fromfile(folder / 'weights.bin', '<f8').reshape(-1, 3)
    vertices = 100 * vertices / np.linalg.norm(vertices, axis=1)[:, None]
    target = 100 * target / np.linalg.norm(target, axis=1)[:, None]
    triangles = vertices[faces]
    origin = triangles[:, 0]
    edges = np.stack((triangles[:, 1]-origin, triangles[:, 2]-origin), axis=2)
    singular = np.linalg.svd(edges, compute_uv=False)
    assert np.max(singular[:, 0]/singular[:, 1]) <= refinement["maximum_triangle_condition_number"]
    inverse = np.linalg.pinv(edges)
    segment_edges = [(triangles[:, b]-triangles[:, a], a, b)
                     for a, b in ((0, 1), (1, 2), (2, 0))]
    max_weight = max_point = max_value = 0.0
    label_mismatches = available_mismatches = 0
    real = (folder / 'values.bin').exists()
    if real:
        values = np.fromfile(folder / 'values.bin', '<f8').reshape(-1, 5)
        output = np.fromfile(folder / 'output.bin', '<f8').reshape(-1, 5)
        source_mask = np.fromfile(folder / 'source-mask.bin', '<i4').astype(bool)
        target_mask = np.fromfile(folder / 'target-mask.bin', '<i4').astype(bool)
        labels = np.fromfile(folder / 'labels.bin', '<f8')
        label_outputs = {p: np.fromfile(folder / (p+'.bin'), '<f8')
                         for p in ('aggregate', 'largest')}
    for row in meta['sampled_targets_zero_based']:
        q = target[row]
        uv = np.einsum('nij,nj->ni', inverse, q-origin)
        bary = np.column_stack((1-uv.sum(axis=1), uv))
        projection = np.einsum('ni,nij->nj', bary, triangles)
        distance = np.sum((q-projection)**2, axis=1)
        distance[np.min(bary, axis=1) < 0] = np.inf
        primitive_distances = [distance.copy()]
        fi = int(np.argmin(distance))
        best_distance, best_face, best_weights = distance[fi], fi, bary[fi].copy()
        for edge, a, b in segment_edges:
            u = np.clip(np.einsum('ni,ni->n', q-triangles[:, a], edge) /
                        np.einsum('ni,ni->n', edge, edge), 0, 1)
            projection = triangles[:, a] + u[:, None]*edge
            distance = np.sum((q-projection)**2, axis=1)
            primitive_distances.append(distance.copy())
            fi = int(np.argmin(distance))
            if distance[fi] < best_distance:
                best_distance, best_face = distance[fi], fi
                best_weights = np.zeros(3)
                best_weights[a], best_weights[b] = 1-u[fi], u[fi]
        envelope = 4096*np.finfo(float).eps*(np.linalg.norm(q)+np.max(np.linalg.norm(vertices, axis=1)))**2
        face_distance = np.minimum.reduce(primitive_distances)
        candidates = np.flatnonzero(face_distance <= best_distance+envelope)
        low, _ = precise_minimum(vertices, faces, q, candidates, refinement['decimal_digits'][0])
        high, high_distance = precise_minimum(vertices, faces, q, candidates, refinement['decimal_digits'][1])
        stable = max(abs(float(low.get(c, 0)-high.get(c, 0))) for c in set(low) | set(high))
        assert stable <= refinement['precision_stability_tolerance'], (name, row, stable)
        oracle = {c: float(w) for c, w in high.items()}
        oracle_point = sum(vertices[c]*w for c, w in oracle.items())
        native_rows = weights[weights[:, 0] == row]
        native = {int(c): float(w) for _, c, w in native_rows}
        error = max(abs(native.get(c, 0)-oracle.get(c, 0)) for c in set(native) | set(oracle))
        native_point = sum(vertices[c]*w for c, w in native.items())
        point_error = float(np.linalg.norm(native_point-oracle_point)/100)
        max_weight, max_point = max(max_weight, error), max(max_point, point_error)
        assert error <= tolerance, (name, row, 'weight', error, native, oracle)
        assert point_error <= contract['relative_projection_tolerance'], (name, row, point_error)
        if real:
            active = {c: w for c, w in oracle.items() if source_mask[c]}
            available = bool(active) and bool(target_mask[row])
            available_mismatches += int(available != bool(np.isfinite(output[row]).all()))
            if available:
                mass = sum(active.values())
                expected = sum(values[c]*w for c, w in active.items())/mass
                max_value = max(max_value, float(np.max(np.abs(expected-output[row]))))
                totals = {}
                for c, w in active.items():
                    key = labels[c]
                    totals[key] = totals.get(key, 0)+w
                aggregate = min(totals, key=lambda k: (-totals[k], k))
                largest = labels[min(active, key=lambda c: (-active[c], c))]
                label_mismatches += int(label_outputs['aggregate'][row] != aggregate)
                label_mismatches += int(label_outputs['largest'][row] != largest)
            else:
                label_mismatches += sum(not np.isnan(x[row]) for x in label_outputs.values())
    report = dict(name=name, queries=len(meta['sampled_targets_zero_based']),
                  triangles_per_query=len(faces), maximum_weight_error=max_weight,
                  maximum_relative_point_error=max_point, maximum_masked_value_error=max_value,
                  label_mismatches=label_mismatches, availability_mismatches=available_mismatches)
    reports.append(report)
    (base / 'oracle-progress.json').write_text(json.dumps(reports, indent=2)+'\n')
    print(json.dumps(report), flush=True)
    assert max_value <= contract['bounded_value_tolerance'], report
    assert label_mismatches == available_mismatches == 0, report
result = dict(status='PASS', contract_sha256=sha(contract_path),
              consumer_receipt_sha256=sha(base/'consumer-receipt.json'),
              oracle_sha256=sha(__file__), refinement_sha256=sha(refinement_path),
              mpmath_version=mp.__version__, numpy_version=np.__version__, cases=reports,
              scope='Fresh exhaustive geometry and sampled policy comparison; no Workbench equivalence claim')
(base/'oracle-receipt.json').write_text(json.dumps(result, indent=2)+'\n')

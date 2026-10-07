"""Full-mesh practical comparison with Workbench and published parcel maps."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import subprocess
import nibabel as nb
import numpy as np
from scipy.spatial import cKDTree


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def values(path):
    return nb.load(path).darrays[0].data


def metric(path, x):
    nb.save(nb.gifti.GiftiImage(darrays=[nb.gifti.GiftiDataArray(
        np.asarray(x, dtype=np.float32), intent="NIFTI_INTENT_SHAPE")]), path)


def boundary(labels, triangles):
    edges = np.vstack([triangles[:, [0, 1]], triangles[:, [1, 2]], triangles[:, [2, 0]]])
    unequal = labels[edges[:, 0]] != labels[edges[:, 1]]
    result = np.zeros(len(labels), dtype=bool)
    result[edges[unequal].ravel()] = True
    return result


def compare(pred, ref, mask, area, coords, triangles, name, label_names):
    pred = np.where(np.isfinite(pred), pred, -1).astype(int)
    ref = np.where(np.isfinite(ref), ref, -1).astype(int)
    mismatch = mask & (pred != ref)
    info = {"comparison": name, "evaluated": int(mask.sum()),
            "disagreements": int(mismatch.sum()),
            "disagreement_percent": float(100 * mismatch.sum() / mask.sum()),
            "area_disagreement_percent": float(100 * area[mismatch].sum() / area[mask].sum())}
    edges = boundary(ref, triangles) & mask
    info["disagreement_boundary_distance_degrees_p95"] = None
    if mismatch.any() and edges.any():
        xyz = coords / np.linalg.norm(coords, axis=1)[:, None]
        chord = cKDTree(xyz[edges]).query(xyz[mismatch])[0]
        degrees = np.degrees(2 * np.arcsin(np.minimum(1, chord / 2)))
        info["disagreement_boundary_distance_degrees_p95"] = float(np.quantile(degrees, .95))
    rows = []
    for key in sorted((set(ref[mask]) | set(pred[mask])) - {0, -1}):
        a, b = mask & (ref == key), mask & (pred == key)
        na, nb_ = int(a.sum()), int(b.sum())
        ra, pa = area[a].sum(), area[b].sum()
        rows.append({"comparison": name, "key": int(key), "label": label_names.get(str(key), str(key)),
                     "reference_vertices": na, "predicted_vertices": nb_,
                     "dice": float(2 * (a & b).sum() / (na + nb_)),
                     "area_dice": float(2 * area[a & b].sum() / (ra + pa)),
                     "reference_area_mm2": float(ra), "predicted_area_mm2": float(pa),
                     "area_bias_percent": float(100 * (pa - ra) / ra) if ra else None})
    info["lost_labels"] = [r["key"] for r in rows if r["reference_vertices"] and not r["predicted_vertices"]]
    info["minimum_parcel_dice"] = min(r["dice"] for r in rows)
    info["median_parcel_dice"] = float(np.median([r["dice"] for r in rows]))
    return info, rows


def run(work, public, pinned, output):
    output.mkdir(parents=True, exist_ok=False)
    lock = json.loads(Path("inst/extdata/surface-inputs-v1.json").read_text())
    domains = {}
    for hemi in ("L", "R"):
        for template, density in (("fsaverage", "164k"), ("fsLR", "32k")):
            assets = {}
            for role in ("sphere", "area"):
                asset_role = "registered_sphere" if template == "fsLR" and role == "sphere" else role
                a = next(a for a in lock["assets"] if a.get("template") == template and
                         a.get("hemisphere") == hemi and a.get("role") == asset_role and a.get("density") == density)
                path = pinned / a["path"]
                assert sha(path) == a["sha256"]
                assets[role] = path
            roi = np.fromfile(public / f"{template}-{hemi}-cortex.bin", "<i4").astype(bool)
            roi_path = output / f"{template}-{hemi}-roi.shape.gii"
            metric(roi_path, roi)
            mesh = nb.load(assets["sphere"])
            assets.update(roi=roi, roi_path=roi_path,
                          coords=mesh.get_arrays_from_intent("NIFTI_INTENT_POINTSET")[0].data,
                          triangles=mesh.get_arrays_from_intent("NIFTI_INTENT_TRIANGLE")[0].data)
            domains[template, hemi] = assets

    def wb(*args):
        cmd = ["wb_command", *map(str, args)]
        p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        with (output / "commands.jsonl").open("a") as stream:
            stream.write(json.dumps({"argv": cmd, "returncode": p.returncode, "output": p.stdout}) + "\n")
        if p.returncode:
            raise RuntimeError(p.stdout)
        return p.stdout

    version = wb("-version")
    public_receipt = json.loads((public / "receipt.json").read_text())
    for name, digest in public_receipt["files"].items():
        assert sha(public / name) == digest
    cases, parcels = [], []
    for name, config in public_receipt["cases"].items():
        source, target = config["from"], config["to"]
        hemi, kind = config["hemi"], config["data_type"]
        dst = domains[target, hemi]
        if kind == "label":
            # Bind WB inputs to the exact public operator geometry, not merely
            # a mesh with the same vertex count or template name.
            for side, template in (("from", source), ("to", target)):
                domain = config["provenance"]["specification"][side]
                geometry = domains[template, hemi]
                for field, array, dtype in (
                    ("coordinates_sha256", geometry["coords"], "<f8"),
                    ("topology_sha256", geometry["triangles"], "<i4"),
                    ("cortex_sha256", geometry["roi"], "<i4"),
                    ("area_sha256", values(geometry["area"]), "<f8")):
                    assert hashlib.sha256(np.asarray(array, dtype=dtype).tobytes()).hexdigest() == domain[field], (name, side, field)
        native = np.fromfile(public / f"{name}.bin", "<f8")
        available = np.fromfile(public / f"{name}-available.bin", "<i4").astype(bool)
        assert len(native) == len(dst["roi"]) and np.array_equal(np.isfinite(native), available)
        area = values(dst["area"])
        if kind in ("label", "projection"):
            scale = config["scale"]
            names = json.loads((work / "surface-inputs" / f"labels-{scale}.json").read_text())
            ref = values(work / "surface-inputs" / f"{target}-{hemi}-{scale}.label.gii")
            if kind == "projection":
                source_table = list(csv.DictReader((public / f"{name}-labels.csv").open()))
                canonical = {v: int(k) for k, v in names.items()}
                translated = np.full(native.shape, np.nan)
                for item in source_table:
                    key = int(item["key"])
                    translated[native == key] = canonical[item["name"]] if key else 0
                native = translated
            info, rows = compare(native, ref, dst["roi"], area, dst["coords"], dst["triangles"], name + ":published", names)
            info["unavailable_cortex"] = int((dst["roi"] & ~available).sum())
            cases.append(info)
            parcels.extend(rows)
            np.savez_compressed(output / (name + "-comparison.npz"), native=native, reference=ref,
                                cortex=dst["roi"], coords=dst["coords"], triangles=dst["triangles"])
            if kind == "projection":
                print(name, "measured", flush=True)
                continue
        src = domains[source, hemi]
        for method in ("BARYCENTRIC", "ADAP_BARY_AREA"):
            suffix = "label.gii" if kind == "label" else "shape.gii"
            path = (work / "surface-inputs" / f"{source}-{hemi}-{scale}.label.gii" if kind == "label" else
                    work / "inputs" / f"tpl-fsaverage_hemi-{hemi}_den-164k_{config['kind']}.shape.gii")
            out = output / f"{name}-{method}.{suffix}"
            valid = output / f"{name}-{method}-valid.shape.gii"
            options = ["-current-roi", src["roi_path"], "-valid-roi-out", valid]
            if method == "ADAP_BARY_AREA":
                options += ["-area-metrics", src["area"], dst["area"]]
            wb("-label-resample" if kind == "label" else "-metric-resample",
               path, src["sphere"], dst["sphere"], method, out, *options)
            pred = values(out).astype(float)
            wb_available = (values(valid) > 0) & dst["roi"]
            joint = available & wb_available & dst["roi"]
            coverage_difference = int(((available != wb_available) & dst["roi"]).sum())
            comparison = name + ":native-vs-" + method
            if kind == "label":
                assert set(pred) <= set(values(path)) | {0}
                info, rows = compare(native, pred, joint, area, dst["coords"], dst["triangles"], comparison, names)
                parcels.extend(rows)
                published, rows = compare(np.where(wb_available, pred, np.nan), ref, dst["roi"], area,
                    dst["coords"], dst["triangles"], name + ":" + method + "-vs-published", names)
                cases.append(published)
                parcels.extend(rows)
            else:
                error = native[joint] - pred[joint]
                info = {"comparison": comparison, "evaluated": int(joint.sum()),
                        "max_absolute_error": float(np.max(np.abs(error))),
                        "rms_error": float(np.sqrt(np.mean(error ** 2))),
                        "correlation": float(np.corrcoef(native[joint], pred[joint])[0, 1]),
                        "strict_5e_minus5_pass": bool(np.max(np.abs(error)) <= 5e-5) if method == "BARYCENTRIC" else None}
            info["availability_disagreements"] = coverage_difference
            cases.append(info)
        print(name, "measured", flush=True)
    with (output / "parcels.csv").open("w") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(parcels[0]))
        writer.writeheader()
        writer.writerows(parcels)
    (output / "receipt.json").write_text(json.dumps({"cases": cases,
        "protocol_sha256": sha(Path(__file__).with_name("protocol.md")),
        "script_sha256": sha(__file__), "public_receipt_sha256": sha(public / "receipt.json"),
        "workbench": version, "workbench_sha256": sha("/usr/bin/wb_command"),
        "files": {p.name: sha(p) for p in output.iterdir() if p.is_file()}}, indent=2, allow_nan=False) + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("work", "public", "pinned", "output"):
        parser.add_argument(name, type=Path)
    a = parser.parse_args()
    run(a.work, a.public, a.pinned, a.output)

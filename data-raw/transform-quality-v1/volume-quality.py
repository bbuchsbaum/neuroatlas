"""Independent regional and deformation diagnostics for released image warps."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import platform

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from scipy import ndimage as ndi
import SimpleITK as sitk

FRAMES = ["MNI152NLin6Asym", "MNI152NLin2009cAsym"]


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def summary(values):
    v = np.asarray(values)
    finite = v[np.isfinite(v)]
    return dict(zip(["min", "p01", "p05", "median", "p95", "p99", "max"],
                    map(float, np.quantile(finite, [0, .01, .05, .5, .95, .99, 1]))),
                n=int(v.size), nonfinite=int(np.count_nonzero(~np.isfinite(v))))


def overlap(a, b, spacing):
    na, nb = int(a.sum()), int(b.sum())
    result = {"reference_voxels": na, "moving_voxels": nb,
              "dice": float(2 * np.count_nonzero(a & b) / (na + nb)) if na + nb else None,
              "volume_bias_percent": float(100 * (nb - na) / na) if na else None,
              "mean_boundary_mm": None, "hd95_mm": None, "centroid_mm": None}
    if not na or not nb:
        return result
    ia, ib = np.argwhere(a), np.argwhere(b)
    result["centroid_mm"] = float(np.linalg.norm((ia.mean(0) - ib.mean(0)) * spacing))
    lo = np.maximum(np.minimum(ia.min(0), ib.min(0)) - 1, 0)
    hi = np.minimum(np.maximum(ia.max(0), ib.max(0)) + 2, a.shape)
    box = tuple(slice(l, h) for l, h in zip(lo, hi))
    a, b = a[box], b[box]
    ea = a & ~ndi.binary_erosion(a)
    eb = b & ~ndi.binary_erosion(b)
    distances = np.r_[ndi.distance_transform_edt(~ea, sampling=spacing)[eb],
                      ndi.distance_transform_edt(~eb, sampling=spacing)[ea]]
    result["mean_boundary_mm"] = float(distances.mean())
    result["hd95_mm"] = float(np.quantile(distances, .95))
    return result


def correlation(a, b, mask):
    x, y = a[mask], b[mask]
    if len(x) < 3 or np.std(x) == 0 or np.std(y) == 0:
        return None
    return float(np.corrcoef(x, y)[0, 1])


def resample(image, fixed, transform, labels=False):
    return sitk.Resample(image, fixed, transform,
                         sitk.sitkNearestNeighbor if labels else sitk.sitkLinear,
                         0, sitk.sitkFloat32)


def run(work, output):
    output.mkdir(parents=True, exist_ok=False)
    inputs = work / "inputs"
    registry = list(csv.DictReader(Path("inst/extdata/transform_registry.csv").open()))
    transforms, bindings = {}, []
    for source in FRAMES:
        target = next(f for f in FRAMES if f != source)
        row = next(r for r in registry if r["from_space"] == source and
                   r["to_space"] == target and r["provider"] == "neuroatlas")
        path = work / "cache" / row["artifact_version"] / (row["artifact_id"] + ".h5")
        assert sha(path) == row["sha256"]
        transforms[source] = sitk.ReadTransform(str(path))
        bindings.append({"source": source, "target": target, "path": str(path), "sha256": sha(path)})
    regions, cases = [], []
    names = {atlas: {int(row["index"]): row["name"].strip() for row in csv.DictReader(
        (inputs / f"tpl-{FRAMES[0]}_atlas-{atlas}_dseg.tsv").open(), delimiter="\t")}
        for atlas in ("HOCPA", "HOSPA")}
    identity = sitk.Transform(3, sitk.sitkIdentity)
    for source in FRAMES:
        target = next(f for f in FRAMES if f != source)
        transform = transforms[source]
        for res in ("01", "02"):
            name = f"{source}_to_{target}_res-{res}"
            def read(frame, suffix):
                return sitk.ReadImage(str(inputs / f"tpl-{frame}_res-{res}_{suffix}"))
            fixed = read(target, "desc-brain_T1w.nii.gz")
            moving = read(source, "desc-brain_T1w.nii.gz")
            spacing = np.array(fixed.GetSpacing()[::-1])
            fixed_values = sitk.GetArrayFromImage(fixed).astype(float)
            mask = sitk.GetArrayFromImage(read(target, "desc-brain_mask.nii.gz")) > 0
            outputs = {}
            for method, tr in (("identity", identity), ("warp", transform)):
                image = resample(moving, fixed, tr)
                values = sitk.GetArrayFromImage(image)
                outputs[method] = values
                sitk.WriteImage(image, str(output / f"{name}-{method}-T1w.nii.gz"))
                moved_mask = sitk.GetArrayFromImage(resample(
                    read(source, "desc-brain_mask.nii.gz"), fixed, tr, True)) > 0
                case = {"case": name, "method": method, "source": source,
                        "target": target, "resolution_mm": int(res),
                        "correlation": correlation(fixed_values, values, mask),
                        "mask": overlap(mask, moved_mask, spacing)}
                cases.append(case)
                for atlas in ("HOCPA", "HOSPA"):
                    ref = sitk.GetArrayFromImage(read(target, f"atlas-{atlas}_desc-th25_dseg.nii.gz"))
                    src = read(source, f"atlas-{atlas}_desc-th25_dseg.nii.gz")
                    moved_img = resample(src, fixed, tr, True)
                    moved = sitk.GetArrayFromImage(moved_img)
                    assert set(np.unique(moved)) <= set(np.unique(sitk.GetArrayViewFromImage(src))) | {0}
                    sitk.WriteImage(moved_img, str(output / f"{name}-{method}-{atlas}.nii.gz"))
                    keys = sorted((set(np.unique(ref)) | set(np.unique(sitk.GetArrayViewFromImage(src)))) - {0})
                    for key in keys:
                        roi = ref == key
                        regions.append({"case": name, "method": method, "atlas": atlas,
                                        "key": int(key), "label": names[atlas].get(int(key), str(key)),
                                        "correlation": correlation(fixed_values, values, roi),
                                        **overlap(roi, moved == key, spacing)})
            field = sitk.TransformToDisplacementField(transform, sitk.sitkVectorFloat64,
                fixed.GetSize(), fixed.GetOrigin(), fixed.GetSpacing(), fixed.GetDirection())
            disp = sitk.GetArrayFromImage(field)
            # Derivatives in physical LPS coordinates, including image direction.
            index_to_lps = np.array(fixed.GetDirection()).reshape(3, 3) @ np.diag(fixed.GetSpacing())
            grad = np.stack([np.stack(np.gradient(disp[..., component]), axis=-1)[..., ::-1]
                             for component in range(3)], axis=-2)
            jac = np.linalg.det(grad @ np.linalg.inv(index_to_lps) + np.eye(3))
            del grad
            composition = sitk.CompositeTransform([transforms[target], transform])
            residual = sitk.GetArrayFromImage(sitk.TransformToDisplacementField(
                composition, sitk.sitkVectorFloat64, fixed.GetSize(), fixed.GetOrigin(),
                fixed.GetSpacing(), fixed.GetDirection()))
            shell = (~mask) & (ndi.distance_transform_edt(~mask, sampling=spacing) <= 3)
            deform = {}
            for region, m in (("brain", mask), ("exterior_shell_3mm", shell)):
                deform[region] = {"jacobian": summary(jac[m]),
                    "nonpositive_jacobians": int(np.count_nonzero(jac[m] <= 0)),
                    "displacement_mm": summary(np.linalg.norm(disp[m], axis=-1)),
                    "inverse_consistency_mm": summary(np.linalg.norm(residual[m], axis=-1))}
            cases[-1]["deformation"] = deform
            del disp, residual, field
            # Fixed anatomical positions and shared intensity scale per template.
            fig, axes = plt.subplots(4, 5, figsize=(15, 12))
            z_positions = [-30, -10, 10, 30, 50]
            def normalize(v):
                return v / np.percentile(v[mask], 99)
            def display(v):
                # All inputs have axis-aligned grids. Show RAS -x (left
                # hemisphere) on the left consistently across both templates.
                return v[:, ::-1] if fixed.GetDirection()[0] > 0 else v
            for col, z in enumerate(z_positions):
                iz = fixed.TransformPhysicalPointToIndex((0., 0., float(z)))[2]
                ref = normalize(fixed_values)[iz]
                for row, method in enumerate(("identity", "warp")):
                    mov = normalize(outputs[method])[iz]
                    yy, xx = np.indices(ref.shape)
                    tiles = ((xx // max(1, round(12 / spacing[2])) + yy // max(1, round(12 / spacing[1]))) % 2) == 0
                    axes[row, col].imshow(display(np.where(tiles, ref, mov)), origin="lower", cmap="gray", vmin=0, vmax=1)
                    axes[row + 2, col].imshow(display(np.abs(ref - mov)), origin="lower", cmap="magma", vmin=0, vmax=.3)
                    axes[row, col].set_title(f"{method}: z={z} mm")
                    axes[row + 2, col].set_title(f"{method}: normalized |difference|")
            for ax in axes.flat:
                ax.axis("off")
            fig.suptitle(name + "\nT1-dependent diagnostics; checkerboards 12 mm; difference scale 0-0.3; L on left", fontsize=12)
            fig.tight_layout()
            fig.savefig(output / (name + ".png"), dpi=120)
            plt.close(fig)
            print(name, "measured", flush=True)
    with (output / "regions.csv").open("w") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(regions[0]))
        writer.writeheader()
        writer.writerows(regions)
    receipt = {"protocol_sha256": sha(Path(__file__).with_name("protocol.md")),
               "script_sha256": sha(__file__), "python": platform.python_version(),
               "SimpleITK": sitk.Version_VersionString(), "numpy": np.__version__,
               "inputs_manifest_sha256": sha(inputs / "inputs.json"),
               "transforms": bindings, "cases": cases,
               "interpretation": "Dependent anatomical diagnostics; no ground-truth accuracy claim",
               "files": {p.name: sha(p) for p in output.iterdir() if p.is_file()}}
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2, allow_nan=False) + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("work", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    run(args.work, args.output)

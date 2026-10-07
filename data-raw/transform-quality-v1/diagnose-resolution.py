"""Post hoc audit of the poor posterior STG 2 mm diagnostic; retain all cases."""
import csv
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import SimpleITK as sitk
import numpy as np
import nibabel as nb
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

spec = importlib.util.spec_from_file_location("volume", Path(__file__).with_name("volume-quality.py"))
volume = importlib.util.module_from_spec(spec)
spec.loader.exec_module(volume)
work, output = map(Path, sys.argv[1:])
output.mkdir(exist_ok=False)
inputs = work / "inputs"
rows, cross = [], []
for template in volume.FRAMES:
    for atlas in ("HOCPA", "HOSPA"):
        table = {int(r["index"]): r["name"].strip() for r in csv.DictReader(
            (inputs / f"tpl-MNI152NLin6Asym_atlas-{atlas}_dseg.tsv").open(), delimiter="\t")}
        a = sitk.ReadImage(str(inputs / f"tpl-{template}_res-01_atlas-{atlas}_desc-th25_dseg.nii.gz"))
        b = sitk.ReadImage(str(inputs / f"tpl-{template}_res-02_atlas-{atlas}_desc-th25_dseg.nii.gz"))
        native = sitk.GetArrayFromImage(b)
        downsampled = sitk.GetArrayFromImage(volume.resample(a, b, sitk.Transform(3, sitk.sitkIdentity), True))
        for key in sorted(set(np.unique(native)) | set(np.unique(downsampled))):
            if key == 0:
                continue
            rows.append({"template": template, "atlas": atlas, "key": int(key), "label": table[int(key)],
                         **volume.overlap(native == key, downsampled == key, np.array(b.GetSpacing()[::-1]))})
        if atlas == "HOCPA":
            vals, counts = np.unique(downsampled[native == 10], return_counts=True)
            cross.append({"template": template, "native_2mm_key": 10,
                          "downsampled_1mm_keys": {str(int(k)): int(n) for k, n in zip(vals, counts)}})
            t1 = sitk.GetArrayFromImage(sitk.ReadImage(str(inputs / f"tpl-{template}_res-02_desc-brain_T1w.nii.gz")))
            fig, axes = plt.subplots(2, 4, figsize=(13, 7))
            for col, z in enumerate((-10, 0, 10, 20)):
                iz = b.TransformPhysicalPointToIndex((0., 0., float(z)))[2]
                for row, (name, labels) in enumerate((("native 2 mm", native), ("1 mm sampled to 2 mm", downsampled))):
                    ax = axes[row, col]
                    background = t1[iz]
                    roi = labels[iz] == 10
                    if b.GetDirection()[0] > 0:
                        background, roi = background[:, ::-1], roi[:, ::-1]
                    ax.imshow(background, origin="lower", cmap="gray")
                    ax.imshow(np.ma.masked_where(~roi, roi), origin="lower", cmap="autumn", vmin=0, vmax=1, alpha=.8)
                    ax.set_title(f"{name}; z={z} mm", fontsize=10)
                    ax.axis("off")
            fig.suptitle(template + ": posterior superior temporal gyrus\nWithin-template control, no inter-template warp; L on left")
            fig.tight_layout()
            fig.savefig(output / (template + "-STG.png"), dpi=130)
            plt.close(fig)
with (output / "within-template-resolution.csv").open("w") as stream:
    writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
runtime = []
for source in volume.FRAMES:
    target = next(t for t in volume.FRAMES if t != source)
    for suffix, kind in (("desc-brain_T1w", "T1w"), ("atlas-HOCPA_desc-th25_dseg", "HOCPA")):
        r = nb.load(work / "public-03" / f"{source}-{target}-{suffix}.nii.gz")
        s = nb.load(work / "volume-02" / f"{source}_to_{target}_res-02-warp-{kind}.nii.gz")
        assert np.allclose(r.affine, s.affine, atol=1e-6, rtol=0) and r.shape == s.shape
        error = np.abs(r.get_fdata() - s.get_fdata())
        runtime.append({"source": source, "target": target, "kind": kind,
                        "max_abs": float(error.max()), "different_voxels": int((error > 0).sum()),
                        "values": int(error.size), "R_output_sha256": volume.sha(r.get_filename()),
                        "SimpleITK_output_sha256": volume.sha(s.get_filename())})
# This sensitivity run starts from 1 mm atlases in both directions and samples
# directly to 2 mm. It complements, never replaces, the original native-2mm cases.
sensitivity = []
for source in volume.FRAMES:
    target = next(t for t in volume.FRAMES if t != source)
    transform = sitk.ReadTransform(str(work / "cache/transform-artifacts-v1" /
        f"tpl-{target}_from-{source}_mode-image_xfm.h5"))
    moving = sitk.ReadImage(str(inputs / f"tpl-{source}_res-01_atlas-HOCPA_desc-th25_dseg.nii.gz"))
    target1 = sitk.ReadImage(str(inputs / f"tpl-{target}_res-01_atlas-HOCPA_desc-th25_dseg.nii.gz"))
    target2 = sitk.ReadImage(str(inputs / f"tpl-{target}_res-02_atlas-HOCPA_desc-th25_dseg.nii.gz"))
    ref = sitk.GetArrayFromImage(volume.resample(target1, target2, sitk.Transform(3, sitk.sitkIdentity), True))
    warped = sitk.GetArrayFromImage(volume.resample(moving, target2, transform, True))
    for key in sorted((set(np.unique(ref)) | set(np.unique(warped))) - {0}):
        sensitivity.append({"source": source, "target": target, "key": int(key),
                            **volume.overlap(ref == key, warped == key, np.array(target2.GetSpacing()[::-1]))})
with (output / "1mm-source-to-2mm-sensitivity.csv").open("w") as stream:
    writer = csv.DictWriter(stream, fieldnames=list(sensitivity[0]))
    writer.writeheader()
    writer.writerows(sensitivity)
(output / "receipt.json").write_text(json.dumps({"post_hoc": True,
    "reason": "Posterior STG concordance falls markedly at 2 mm; audit within-template consistency",
    "script_sha256": volume.sha(__file__), "cross_tabs": cross,
    "public_API_against_SimpleITK": runtime,
    "files": {p.name: volume.sha(p) for p in output.iterdir() if p.is_file()}}, indent=2) + "\n")

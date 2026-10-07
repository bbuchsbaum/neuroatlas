"""Assemble compact evidence and a visual packet; never assign expert ratings."""
import csv
import hashlib
import json
from pathlib import Path
import shutil
import sys
import numpy as np
import nibabel as nb
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

work, pinned, out = map(Path, sys.argv[1:])
out.mkdir(parents=True, exist_ok=False)
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
for folder in ("volume-02", "surface-02", "resolution-03", "t2-01", "public-03", "stages-01", "masks-01"):
    for path in (work / folder).iterdir():
        if path.suffix in (".csv", ".json", ".png"):
            shutil.copyfile(path, out / (folder + "-" + path.name))
for name in ("inputs.json", "cbig-volume-inputs-verified.json"):
    shutil.copyfile(work / "inputs" / name, out / name)
shutil.copyfile(work / "metric-tests.log", out / "metric-tests.log")
volume_rows = list(csv.DictReader((work / "volume-02/regions.csv").open()))
volume_worst = []
for case in sorted({r["case"] for r in volume_rows}):
    reference = {(r["atlas"], r["key"]): r for r in volume_rows
                 if r["case"] == case and r["method"] == "identity"}
    moved = [r for r in volume_rows if r["case"] == case and r["method"] == "warp"]
    for row in sorted(moved, key=lambda r: float(r["dice"]))[:10]:
        previous = reference[row["atlas"], row["key"]]
        volume_worst.append({**row, "identity_dice": previous["dice"],
                             "dice_change": float(row["dice"]) - float(previous["dice"])})
surface_rows = list(csv.DictReader((work / "surface-02/parcels.csv").open()))
surface_worst = []
for case in sorted({r["comparison"] for r in surface_rows if r["comparison"].endswith(":published")}):
    surface_worst.extend(sorted([r for r in surface_rows if r["comparison"] == case],
                                key=lambda r: float(r["dice"]))[:10])
for name, rows in (("worst-volume-regions.csv", volume_worst),
                   ("worst-surface-parcels.csv", surface_worst)):
    with (out / name).open("w") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
lock = json.loads(Path("inst/extdata/surface-inputs-v1.json").read_text())
fig, axes = plt.subplots(2, 4, figsize=(16, 8), subplot_kw={"projection": "3d"})
for col, (hemi, azim) in enumerate((("L", 180), ("L", 0), ("R", 0), ("R", 180))):
    asset = next(a for a in lock["assets"] if a.get("template") == "fsLR" and
                 a.get("hemisphere") == hemi and a.get("role") == "inflated")
    path = pinned / asset["path"]
    assert sha(path) == asset["sha256"]
    mesh = nb.load(path)
    xyz = mesh.get_arrays_from_intent("NIFTI_INTENT_POINTSET")[0].data
    tri = mesh.get_arrays_from_intent("NIFTI_INTENT_TRIANGLE")[0].data
    for row, route in enumerate((f"fsaverage-fsLR-{hemi}-1000", f"projection-{hemi}-1000")):
        data = np.load(work / "surface-02" / (route + "-comparison.npz"))
        mismatch = (data["native"] != data["reference"]) & data["cortex"]
        unavailable = ~np.isfinite(data["native"]) & data["cortex"]
        cortex = data["cortex"][tri].mean(1) > .5
        wrong = mismatch[tri].mean(1)
        color = np.tile([.73, .76, .80, 1.], (len(tri), 1))
        color[:, :3] = (1 - wrong[:, None]) * color[:, :3] + wrong[:, None] * np.array([.87, .10, .14])
        color[~cortex] = [.96, .93, .86, 1]
        color[unavailable[tri].mean(1) > .5] = [1., .63, .06, 1]
        ax = axes[row, col]
        collection = Poly3DCollection(xyz[tri], facecolors=color, linewidths=0, rasterized=True)
        ax.add_collection3d(collection)
        ax.set_xlim(xyz[:, 0].min(), xyz[:, 0].max())
        ax.set_ylim(xyz[:, 1].min(), xyz[:, 1].max())
        ax.set_zlim(xyz[:, 2].min(), xyz[:, 2].max())
        ax.set_box_aspect(np.ptp(xyz, axis=0))
        ax.view_init(elev=0, azim=azim)
        ax.set_proj_type("ortho")
        ax.axis("off")
        ax.set_title(("Surface resampling" if row == 0 else "Volume projection") +
                     f" | {hemi} " + ("lateral" if col % 2 == 0 else "medial"), fontsize=10)
fig.suptitle("Schaefer 1000: comparison with published fsLR labels\nRed = disagreement; orange = unavailable cortex; pale = medial wall. Dependent pipeline comparison.")
fig.tight_layout()
fig.savefig(out / "surface-disagreement-1000.png", dpi=140)
plt.close(fig)

# Localize source-label losses, including labels absent in the published target.
source = nb.load(work / "inputs/Schaefer2018_1000Parcels_7Networks_order_FSLMNI152_1mm.nii.gz").get_fdata()
names = json.loads((work / "surface-inputs/labels-1000.json").read_text())
losses = []
for hemi in ("L", "R"):
    mid = np.fromfile(work / "stages-01" / f"{hemi}-values.bin", "<f8")
    final = np.fromfile(work / "public-03" / f"projection-{hemi}-1000.bin", "<f8")
    ref = nb.load(work / "surface-inputs" / f"fsLR-{hemi}-1000.label.gii").darrays[0].data
    keys = set(range(1, 501) if hemi == "L" else range(501, 1001))
    for key in sorted(keys - set(final[np.isfinite(final)])):
        losses.append({"hemisphere": hemi, "key": key, "label": names[str(key)],
                       "volume_voxels": int((source == key).sum()),
                       "intermediate_fsaverage_vertices": int((mid == key).sum()),
                       "output_fsLR_vertices": int((final == key).sum()),
                       "published_fsLR_vertices": int((ref == key).sum())})
(out / "projection-source-label-losses.json").write_text(json.dumps(losses, indent=2) + "\n")
with (out / "human-review.csv").open("w") as stream:
    writer = csv.writer(stream)
    writer.writerow(["panel", "reviewer", "date", "alignment_rating", "local_defect", "comments"])
    for path in sorted(out.glob("*.png")):
        writer.writerow([path.name, "", "", "", "", ""])
html = ["<!doctype html><html><head><meta charset='utf-8'><title>Transform quality review</title>",
        "<style>body{font:17px system-ui;max-width:1300px;margin:36px auto;padding:0 20px}img{width:100%}a{color:#145fac}section{margin:36px 0}</style></head><body>",
        "<h1>Transform quality evidence - 7 October 2026</h1>",
        "<p>Read <a href='../README.md'>the findings and limitations</a> before interpreting these panels. "
        "T1 and atlas comparisons are dependent diagnostics. No independent anatomical ground truth or human expert sign-off is claimed.</p>",
        "<p><a href='human-review.csv'>Unrated human review form</a>. "
        "Rate visible alignment and regional defects; do not infer accuracy from numerical agreement alone.</p>"]
for path in sorted(out.glob("*.png")):
    html += [f"<section><h2>{path.stem}</h2><img src='{path.name}' alt='{path.stem}'></section>"]
html += ["<h2>Measurements and provenance</h2><ul>"]
for path in sorted(out.iterdir()):
    if path.suffix in (".csv", ".json"):
        html += [f"<li><a href='{path.name}'>{path.name}</a></li>"]
html += ["</ul></body></html>"]
(out / "index.html").write_text("\n".join(html) + "\n")
(out / "SHA256SUMS").write_text("".join(f"{sha(p)}  {p.name}\n" for p in sorted(out.iterdir()) if p.is_file()))
print("Review packet:", out)

#!/usr/bin/env python3
"""Create local QA views; retain upstream geometry licenses for any sharing."""
from pathlib import Path
import hashlib
import json
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection
from matplotlib.colors import hsv_to_rgb
import nibabel as nb
import numpy as np

root = Path(sys.argv[1])
work = root.parent
inputs = work / "inputs"
receipt_path = root / "public-examples-receipt.json"
receipt = json.loads(receipt_path.read_text())
lock = json.loads(Path("data-raw/surface-transforms-v1/inputs.lock.json").read_text())
out = root / (sys.argv[2] if len(sys.argv) > 2 else "visual-qa")
out.mkdir(exist_ok=False)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def geometry(template, hemi, role):
    asset = next(a for a in lock["assets"] if a.get("template") == template
                 and a.get("hemisphere") == hemi and a.get("role") == role)
    path = inputs / asset["path"]
    assert sha(path) == asset["sha256"]
    g = nb.load(path)
    return (g.get_arrays_from_intent("NIFTI_INTENT_POINTSET")[0].data,
            g.get_arrays_from_intent("NIFTI_INTENT_TRIANGLE")[0].data)


def surface_panel(ax, vertices, faces, labels, hemi, view):
    data = labels[faces]
    a, b, c = data.T
    keys = np.where((a == b) | (a == c), a,
                    np.where(b == c, b, np.minimum(a, np.minimum(b, c))))
    supported = np.isfinite(data).all(axis=1)
    hue = np.nan_to_num(keys) * .61803398875 % 1
    colors = hsv_to_rgb(np.c_[hue, np.full(len(hue), .6), np.full(len(hue), .9)])
    colors[keys == 0] = .88
    colors[~supported] = .35
    xyz = vertices[faces]
    direction = (-1 if hemi == "L" else 1) * (1 if view == "lateral" else -1)
    order = np.argsort(direction * xyz[:, :, 0].mean(axis=1))
    polygons = PolyCollection(xyz[order][:, :, [1, 2]], facecolors=colors[order],
                              edgecolors="none", antialiased=False, rasterized=True)
    ax.add_collection(polygons)
    ax.autoscale_view()
    ax.set_aspect("equal")
    ax.set_title(f"{hemi}: {view}")
    ax.set_xlabel("RAS Y (mm)")
    ax.set_ylabel("RAS Z (mm)")


figures = []
for template, role in [("fsaverage", "midthickness"), ("fsLR", "midthickness"),
                       ("fsLR", "inflated")]:
    fig, axes = plt.subplots(2, 2, figsize=(11, 8))
    for row, hemi in enumerate(("L", "R")):
        name = f"MNI152NLin6Asym-{template}-{hemi}"
        path = root / (name + "-labels.bin")
        assert sha(path) == receipt["files"][path.name]
        labels = np.fromfile(path, "<f8")
        vertices, faces = geometry(template, hemi, role)
        for col, view in enumerate(("lateral", "medial")):
            surface_panel(axes[row, col], vertices, faces, labels, hemi, view)
    fig.suptitle(f"Harvard-Oxford cortical labels on {template} {role}\n"
                 "Grey: supported background; dark grey: unsupported/medial wall")
    fig.tight_layout()
    path = out / f"labels-{template}-{role}.png"
    fig.savefig(path, dpi=140)
    plt.close(fig)
    figures.append(path)

source_images = []
for frame in ("MNI152NLin6Asym", "MNI152NLin2009cAsym"):
    image_path = work / f"cache/templateflow/tpl-{frame}/tpl-{frame}_res-02_desc-brain_T1w.nii.gz"
    image = nb.as_closest_canonical(nb.load(image_path))
    data = image.get_fdata()
    affine = image.affine
    fig, axes = plt.subplots(2, 3, figsize=(12, 8))
    points = {}
    for hemi in ("L", "R"):
        path = root / f"{frame}-fsaverage-{hemi}-sampling.bin"
        assert sha(path) == receipt["files"][path.name]
        mask_asset = next(a for a in lock["assets"] if a.get("template") == "fsaverage"
                          and a.get("hemisphere") == hemi and a.get("role") == "mask"
                          and a.get("density") == "164k")
        mask_path = inputs / mask_asset["path"]
        assert sha(mask_path) == mask_asset["sha256"]
        cortex = nb.load(mask_path).darrays[0].data.astype(bool)
        points[hemi] = np.fromfile(path, "<f8").reshape(-1, 3, order="F")[cortex]
    for ax, z in zip(axes.flat, (-20, -5, 10, 25, 40, 55)):
        index = int(round((z - affine[2, 3]) / affine[2, 2]))
        actual_z = affine[2, 3] + index * affine[2, 2]
        extent = [affine[0, 3] - affine[0, 0]/2,
                  affine[0, 3] + (data.shape[0] - .5) * affine[0, 0],
                  affine[1, 3] - affine[1, 1]/2,
                  affine[1, 3] + (data.shape[1] - .5) * affine[1, 1]]
        ax.imshow(data[:, :, index].T, origin="lower", extent=extent, cmap="gray")
        for hemi, color in [("L", "tomato"), ("R", "cyan")]:
            p = points[hemi]
            selected = np.abs(p[:, 2] - actual_z) <= 1
            ax.scatter(p[selected, 0], p[selected, 1], s=.2, c=color, alpha=.5)
        ax.set_title(f"RAS Z = {actual_z:g} mm")
        ax.set_xlabel("RAS X (mm)")
        ax.set_ylabel("RAS Y (mm)")
    fig.suptitle(f"{frame}: population sampling coordinates over exact 2 mm T1w\n"
                 "Included cortex only; red: L; cyan: R; points within 1 mm of slice")
    fig.tight_layout()
    path = out / f"sampling-{frame}.png"
    fig.savefig(path, dpi=150)
    plt.close(fig)
    figures.append(path)
    source_images.append(dict(frame=frame, sha256=sha(image_path)))

(out / "visual-qa-receipt.json").write_text(json.dumps(dict(
    status="rendered; inspection pending", human_review="not performed",
    anatomical_accuracy="not established by these dependent population overlays",
    geometry_redistribution="local QA only; upstream licenses apply",
    public_examples_receipt_sha256=sha(receipt_path),
    plot_driver_sha256=sha(Path(__file__)), matplotlib_version=matplotlib.__version__,
    source_images=source_images,
    figures={path.name: sha(path) for path in figures}), indent=2) + "\n")
print(f"Rendered {len(figures)} local QA figures")

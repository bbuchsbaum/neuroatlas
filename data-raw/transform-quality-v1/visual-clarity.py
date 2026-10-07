"""Render a visual-only supplement from hash-verified campaign volumes."""
import argparse
import base64
import hashlib
import io
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import nibabel as nib
import numpy as np
from PIL import Image

FRAMES = ["MNI152NLin6Asym", "MNI152NLin2009cAsym"]
# Fixed anatomical views, shared by all cases; not selected by improvement.
VIEWS = [
    ("Ventricles / deep gray", 2, 10, (-45, 45), (-55, 40)),
    ("Superior cortex", 2, 30, (-85, 85), (-110, 80)),
    ("Coronal overview", 1, -20, (-85, 85), (-55, 85)),
    ("Midline / brainstem", 0, 0, (-80, 45), (-60, 75)),
]


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def encoded_image(values):
    array = np.uint8(np.clip(values, 0, 1) * 255 + .5)
    stream = io.BytesIO()
    # Anatomical coordinates increase upward; HTML image rows increase downward.
    Image.fromarray(array[::-1]).save(stream, format="PNG")
    return "data:image/png;base64," + base64.b64encode(stream.getvalue()).decode()


def plane(values, affine, axis, coordinate, horizontal, vertical):
    axes = [i for i in range(3) if i != axis]
    index = int(round((coordinate - affine[axis, 3]) / affine[axis, axis]))
    assert 0 <= index < values.shape[axis]
    selection = []
    for a, limits in zip(axes, (horizontal, vertical)):
        positions = affine[a, 3] + affine[a, a] * np.arange(values.shape[a])
        selected = np.flatnonzero((positions >= limits[0]) & (positions <= limits[1]))
        assert len(selected) > 1
        selection.append(slice(selected[0], selected[-1] + 1))
    return np.take(values, index, axis=axis)[tuple(selection)].T


def overlay(reference, moving):
    # Equal intensities are gray. Reference-only = magenta; moving-only = green.
    return np.clip(np.stack([reference, moving, reference], axis=-1), 0, 1)


def run(work, output):
    output.mkdir(parents=True, exist_ok=False)
    receipt_path = work / "volume-02/receipt.json"
    manifest_path = work / "inputs/inputs.json"
    receipt = json.loads(receipt_path.read_text())
    assert sha(manifest_path) == receipt["inputs_manifest_sha256"]
    hashes = {row["path"]: row["sha256"]
              for row in json.loads(manifest_path.read_text())}
    bindings, panels, metrics = [], [], []

    def read(path, expected):
        assert sha(path) == expected, f"Hash mismatch: {path}"
        bindings.append({"path": str(path), "sha256": expected})
        image = nib.as_closest_canonical(nib.load(path))
        assert nib.aff2axcodes(image.affine) == ("R", "A", "S")
        assert np.allclose(image.affine[:3, :3],
                           np.diag(np.diag(image.affine[:3, :3])))
        return image

    for source in FRAMES:
        target = next(frame for frame in FRAMES if frame != source)
        for res in ("01", "02"):
            case = f"{source}_to_{target}_res-{res}"
            short = f"{source.replace('MNI152NLin', 'MNI')} to " + \
                    f"{target.replace('MNI152NLin', 'MNI')} | {int(res)} mm"
            name = f"tpl-{target}_res-{res}_desc-brain_T1w.nii.gz"
            fixed = read(work / "inputs" / name, hashes[name])
            name = f"tpl-{target}_res-{res}_desc-brain_mask.nii.gz"
            mask_image = read(work / "inputs" / name, hashes[name])
            assert np.allclose(fixed.affine, mask_image.affine)
            mask = mask_image.get_fdata() > 0
            arrays = [fixed.get_fdata()]
            for method in ("identity", "warp"):
                name = f"{case}-{method}-T1w.nii.gz"
                moving = read(work / "volume-02" / name, receipt["files"][name])
                assert moving.shape == fixed.shape
                assert np.allclose(moving.affine, fixed.affine)
                arrays.append(moving.get_fdata())
            scales = [float(np.percentile(a[mask], 99)) for a in arrays]
            assert all(s > 0 for s in scales)
            arrays = [a / s for a, s in zip(arrays, scales)]
            for view, axis, coordinate, horizontal, vertical in VIEWS:
                crops = [plane(a, fixed.affine, axis, coordinate, horizontal,
                               vertical) for a in arrays]
                crop_mask = plane(mask, fixed.affine, axis, coordinate,
                                  horizontal, vertical)
                actual = float(fixed.affine[axis, 3] + fixed.affine[axis, axis] *
                               round((coordinate - fixed.affine[axis, 3]) /
                                     fixed.affine[axis, axis]))
                coordinate_text = f"{'xyz'[axis]} = {actual:g} mm (RAS)"
                ref, before, after = crops
                residuals = [np.abs(ref - moving) for moving in (before, after)]
                errors = [float(r[crop_mask].mean()) for r in residuals]
                metric = dict(case=case, view=view, coordinate=coordinate_text,
                              crop_bounds_mm=[horizontal, vertical],
                              target_mask_pixels=int(crop_mask.sum()),
                              mean_absolute_residual=dict(zip(["identity", "warp"], errors)),
                              normalization_p99=dict(zip(["target", "identity", "warp"], scales)))
                metrics.append(metric)
                captions = ["P", "A"] if axis == 0 else ["L", "R"]
                panels.append(dict(
                    label=f"{short} / {view} / {coordinate_text}",
                    ends=captions, reference=encoded_image(ref),
                    before=encoded_image(before), after=encoded_image(after),
                    overlays=[encoded_image(overlay(ref, a)) for a in (before, after)],
                    residuals=[encoded_image(matplotlib.colormaps['magma'](
                        np.clip(r / .3, 0, 1))[:, :, :3]) for r in residuals],
                    errors=errors))
                if view == VIEWS[0][0]:
                    fig, axes = plt.subplots(2, 2, figsize=(10, 11),
                                             layout="constrained")
                    for col, (moving, residual, title) in enumerate(zip(
                            (before, after), residuals,
                            ("Before: identity resampling", "After: released transform"))):
                        axes[0, col].imshow(overlay(ref, moving), origin="lower",
                                            interpolation="nearest")
                        axes[0, col].set_title(title, fontsize=14)
                        im = axes[1, col].imshow(residual, origin="lower",
                            cmap="magma", vmin=0, vmax=.3, interpolation="nearest")
                        axes[1, col].set_title(f"Mean residual in target mask: {errors[col]:.3f}")
                    for ax in axes.flat:
                        ax.set_xticks([]); ax.set_yticks([])
                        ax.set_xlabel("L                                      R")
                    fig.colorbar(im, ax=axes[1, :], shrink=.8,
                                 label="Absolute normalized T1 difference (clipped at 0.30)")
                    fig.suptitle(short + "\nVentricles / deep gray: " + coordinate_text +
                        "\nMagenta = target; green = source; gray = equal intensity",
                        fontsize=14)
                    fig.savefig(output / f"{case}-detail.png", dpi=140)
                    plt.close(fig)
    template = Path(__file__).with_name("visual-clarity.html").read_text()
    colorbar = encoded_image(matplotlib.colormaps["magma"](
        np.linspace(0, 1, 280)[None, :])[:, :, :3])
    (output / "index.html").write_text(template.replace(
        "__PANELS__", json.dumps(panels)).replace("__COLORBAR__", colorbar))
    result = dict(purpose="Post hoc visual supplement; no new qualification gate",
                  source_receipt_sha256=sha(receipt_path),
                  script_sha256=sha(__file__),
                  html_template_sha256=sha(Path(__file__).with_suffix('.html')),
                  inputs=bindings, crops=metrics,
                  interpretation="Intensity residuals are not anatomical error in mm. "
                  "Normalization follows the existing campaign: each image divided "
                  "by its own p99 within the target brain mask. Display scales are "
                  "fixed across cases; no local contrast enhancement or smoothing.",
                  outputs={p.name: sha(p) for p in sorted(output.iterdir())})
    (output / "receipt.json").write_text(json.dumps(result, indent=2) + "\n")
    print(f"Rendered {len(panels)} views; verified {len(bindings)} input bindings")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("work", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    run(args.work, args.output)

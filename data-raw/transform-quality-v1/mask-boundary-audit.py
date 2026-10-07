"""Post hoc separation of enclosed mask holes from exterior-boundary agreement."""
import importlib.util
import json
from pathlib import Path
import sys
import numpy as np
from scipy import ndimage as ndi
import SimpleITK as sitk

spec = importlib.util.spec_from_file_location("volume", Path(__file__).with_name("volume-quality.py"))
volume = importlib.util.module_from_spec(spec)
spec.loader.exec_module(volume)
work, out = map(Path, sys.argv[1:])
out.mkdir(exist_ok=False)
results = []
for source in volume.FRAMES:
    target = next(t for t in volume.FRAMES if t != source)
    transform = sitk.ReadTransform(str(work / "cache/transform-artifacts-v1" /
        f"tpl-{target}_from-{source}_mode-image_xfm.h5"))
    for res in ("01", "02"):
        fixed = sitk.ReadImage(str(work / "inputs" / f"tpl-{target}_res-{res}_desc-brain_mask.nii.gz"))
        moving = sitk.ReadImage(str(work / "inputs" / f"tpl-{source}_res-{res}_desc-brain_mask.nii.gz"))
        a = sitk.GetArrayFromImage(fixed) > 0
        b = sitk.GetArrayFromImage(moving) > 0
        source_holes = int((ndi.binary_fill_holes(b) & ~b).sum())
        target_holes = int((ndi.binary_fill_holes(a) & ~a).sum())
        for method, tr in (("identity", sitk.Transform(3, sitk.sitkIdentity)), ("warp", transform)):
            moved = sitk.GetArrayFromImage(volume.resample(moving, fixed, tr, True)) > 0
            results.append({"source": source, "target": target, "resolution_mm": int(res),
                "method": method, "source_enclosed_hole_voxels": source_holes,
                "target_enclosed_hole_voxels": target_holes,
                "hole_filled_boundary": volume.overlap(ndi.binary_fill_holes(a),
                    ndi.binary_fill_holes(moved), np.array(fixed.GetSpacing()[::-1]))})
(out / "receipt.json").write_text(json.dumps({"post_hoc": True,
    "reason": "Forward mask HD95 worsens despite higher Dice; separate enclosed holes",
    "interpretation": "Supplementary hole-filled diagnostic; original unfilled mask results remain primary",
    "script_sha256": volume.sha(__file__), "cases": results}, indent=2) + "\n")

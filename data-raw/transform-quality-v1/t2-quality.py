"""Additional-contrast diagnostics, explicitly not independent anatomical truth."""
import csv
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import sys
import urllib.request
import SimpleITK as sitk
import numpy as np

spec = importlib.util.spec_from_file_location("volume", Path(__file__).with_name("volume-quality.py"))
volume = importlib.util.module_from_spec(spec)
spec.loader.exec_module(volume)
work, out = map(Path, sys.argv[1:])
out.mkdir(exist_ok=False)
inputs, images, receipts = work / "inputs", {}, []
for frame, rev, resolution in (
    ("MNI152NLin6Asym", "c906e8d808a34719e5024a4bde61f03a4e411ddd", "04"),
    ("MNI152NLin2009cAsym", "15d7c02160f79f5218d2545b4febebeecc11531d", "01")):
    name = f"tpl-{frame}_res-{resolution}_T2w.nii.gz"
    raw = urllib.request.urlopen(f"https://raw.githubusercontent.com/templateflow/tpl-{frame}/{rev}/{name}").read()
    annex = re.search(rb"(MD5E|SHA256E)-s(\d+)--([a-f0-9]+)", raw)
    assert annex
    url = f"https://templateflow.s3.amazonaws.com/tpl-{frame}/{name}"
    path = inputs / name
    data = path.read_bytes() if path.exists() else urllib.request.urlopen(url).read()
    algorithm = "md5" if annex[1] == b"MD5E" else "sha256"
    assert len(data) == int(annex[2]) and hashlib.new(algorithm, data).hexdigest() == annex[3].decode()
    path.write_bytes(data)
    receipts.append({"path": name, "revision": rev, "url": url, "sha256": volume.sha(path)})
    images[frame] = sitk.ReadImage(str(path))
rows = []
for source in volume.FRAMES:
    target = next(t for t in volume.FRAMES if t != source)
    transform = sitk.ReadTransform(str(work / "cache/transform-artifacts-v1" /
        f"tpl-{target}_from-{source}_mode-image_xfm.h5"))
    for res in ("01", "02"):
        fixed = sitk.ReadImage(str(inputs / f"tpl-{target}_res-{res}_desc-brain_T1w.nii.gz"))
        ref = sitk.GetArrayFromImage(volume.resample(images[target], fixed, sitk.Transform(3, sitk.sitkIdentity)))
        brain = sitk.GetArrayFromImage(sitk.ReadImage(str(inputs / f"tpl-{target}_res-{res}_desc-brain_mask.nii.gz"))) > 0
        for method, tr in (("identity", sitk.Transform(3, sitk.sitkIdentity)), ("warp", transform)):
            moved = sitk.GetArrayFromImage(volume.resample(images[source], fixed, tr))
            common = {"source": source, "target": target, "resolution_mm": int(res), "method": method}
            rows.append({**common, "atlas": "brain", "key": 1, "correlation": volume.correlation(ref, moved, brain)})
            for atlas in ("HOCPA", "HOSPA"):
                labels = sitk.GetArrayFromImage(sitk.ReadImage(str(inputs / f"tpl-{target}_res-{res}_atlas-{atlas}_desc-th25_dseg.nii.gz")))
                for key in sorted(set(np.unique(labels)) - {0}):
                    rows.append({**common, "atlas": atlas, "key": int(key),
                                 "correlation": volume.correlation(ref, moved, labels == key)})
with (out / "T2-correlations.csv").open("w") as stream:
    writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
(out / "receipt.json").write_text(json.dumps({"inputs": receipts,
    "protocol_sha256": volume.sha(Path(__file__).with_name("t2-protocol.md")),
    "script_sha256": volume.sha(__file__), "interpretation": "Additional contrast; anatomical independence unestablished",
    "files": {p.name: volume.sha(p) for p in out.iterdir() if p.is_file()}}, indent=2) + "\n")
print("Measured T2 in", len(rows), "region/method/grid cells")

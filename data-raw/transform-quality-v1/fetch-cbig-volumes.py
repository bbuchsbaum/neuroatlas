"""Fetch volume labels and verify their key/name table against same-revision CIFTI."""
import hashlib
import json
from pathlib import Path
import sys
import urllib.request

work = Path(sys.argv[1])
revision = "634f676630929a71297852d01dd92a287103e861"
base = (f"https://raw.githubusercontent.com/ThomasYeoLab/CBIG/{revision}/"
        "stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/")
receipts = []
for scale in (100, 400, 1000):
    stem = f"Schaefer2018_{scale}Parcels_7Networks_order"
    for filename in (stem + "_FSLMNI152_1mm.nii.gz", "freeview_lut/" + stem + ".txt"):
        content = urllib.request.urlopen(base + filename).read()
        path = work / "inputs" / Path(filename).name
        if path.exists():
            assert path.read_bytes() == content
        else:
            path.write_bytes(content)
        receipts.append({"path": path.name, "url": base + filename,
                         "revision": revision, "bytes": len(content),
                         "sha256": hashlib.sha256(content).hexdigest()})
    expected = json.loads((work / "surface-inputs" / f"labels-{scale}.json").read_text())
    for line in (work / "inputs" / (stem + ".txt")).read_text().splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        key, name = line.split()[:2]
        if key != "0":
            assert expected[key] == name, (scale, key, name, expected[key])
(work / "inputs/cbig-volume-inputs-verified.json").write_text(json.dumps(receipts, indent=2) + "\n")
print("All CBIG volume keys match same-revision CIFTI names")

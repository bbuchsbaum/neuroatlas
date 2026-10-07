"""Canonicalize parcel keys by name and split published CBIG CIFTI labels."""
import sys
import json
import hashlib
from pathlib import Path
import nibabel as nb
import numpy as np

work = Path(sys.argv[1])
out = work / "surface-inputs"
out.mkdir(exist_ok=False)
receipts = []
for scale in (100, 400, 1000):
    image = nb.load(work / "inputs" / f"Schaefer2018_{scale}Parcels_7Networks_order.dlabel.nii")
    labels = image.header.get_axis(0).label[0]
    by_name = {name: key for key, (name, rgba) in labels.items() if key != 0}
    assert len(by_name) == scale
    table = nb.gifti.GiftiLabelTable()
    for key, (name, rgba) in labels.items():
        item = nb.gifti.GiftiLabel(key, *rgba)
        item.label = name
        table.labels.append(item)
    for hemi, structure in (("L", "CIFTI_STRUCTURE_CORTEX_LEFT"),
                            ("R", "CIFTI_STRUCTURE_CORTEX_RIGHT")):
        part = next((sl, bm) for name, sl, bm in image.header.get_axis(1).iter_structures()
                    if name == structure)
        sl, bm = part
        assert bm.nvertices[structure] == 32492 and np.all(bm.vertex >= 0)
        fslr = np.zeros(32492, dtype=np.int32)
        fslr[bm.vertex] = image.get_fdata()[0, sl].astype(np.int32)
        avg = nb.load(work / "inputs" / f"tpl-fsaverage_hemi-{hemi}_den-164k_"
                      f"atlas-Schaefer2018_seg-7n_scale-{scale}_dseg.label.gii")
        source_keys = avg.darrays[0].data
        avg_values = np.zeros(len(source_keys), dtype=np.int32)
        mapping = {}
        for key, name in avg.labeltable.get_labels_as_dict().items():
            if key == 0:
                continue
            mapping[key] = by_name[name]
            avg_values[source_keys == key] = by_name[name]
        assert set(np.unique(source_keys)) <= {0, *mapping.keys()}
        for template, values in (("fsaverage", avg_values), ("fsLR", fslr)):
            path = out / f"{template}-{hemi}-{scale}.label.gii"
            nb.save(nb.gifti.GiftiImage(darrays=[nb.gifti.GiftiDataArray(
                values, intent="NIFTI_INTENT_LABEL")], labeltable=table), path)
            receipts.append({"path": path.name, "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                             "vertices": len(values), "present_labels": len(set(values) - {0})})
    with (out / f"labels-{scale}.json").open("w") as stream:
        json.dump({str(key): name for key, (name, rgba) in labels.items()}, stream, indent=2)
(out / "receipt.json").write_text(json.dumps(receipts, indent=2) + "\n")
print("Prepared", len(receipts), "canonicalized label maps")

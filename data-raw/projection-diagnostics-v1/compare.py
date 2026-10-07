"""Compare fixed first-stage labels under the frozen supplementary protocol."""
import argparse
import csv
import hashlib
import json
import subprocess
from pathlib import Path

import nibabel as nb
import numpy as np


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def metric(path, values):
    nb.save(nb.gifti.GiftiImage(darrays=[nb.gifti.GiftiDataArray(
        np.asarray(values, dtype=np.float32), intent="NIFTI_INTENT_SHAPE")]), path)


def values(path):
    return nb.load(path).darrays[0].data


def run(work, exported, pinned, out):
    out.mkdir(parents=True, exist_ok=False)
    protocol = Path("data-raw/projection-diagnostics-v1/protocol.md")
    receipt = json.loads((exported / "receipt.json").read_text())
    assert receipt["protocol_sha256"] == sha(protocol)
    for name, digest in receipt["files"].items():
        assert sha(exported / name) == digest
    lock = json.loads(Path("inst/extdata/surface-inputs-v1.json").read_text())
    version = subprocess.check_output(["wb_command", "-version"], text=True)
    cases = []
    parity = []
    for hemi in ("L", "R"):
        domains = {}
        for template, density in (("fsaverage", "164k"), ("fsLR", "32k")):
            domain = {}
            for role in ("sphere", "area"):
                declared_role = "registered_sphere" if template == "fsLR" and role == "sphere" else role
                asset = next(a for a in lock["assets"] if a.get("template") == template
                             and a.get("density") == density and a.get("hemisphere") == hemi
                             and a.get("role") == declared_role)
                path = pinned / asset["path"]
                assert sha(path) == asset["sha256"]
                domain[role] = path
            cortex = np.fromfile(work / "public-03" / f"{template}-{hemi}-cortex.bin", "<i4") > 0
            roi_path = out / f"{template}-{hemi}-roi.shape.gii"
            metric(roi_path, cortex)
            domain.update(cortex=cortex, roi_path=roi_path)
            domains[template] = domain
        src, dst = domains["fsaverage"], domains["fsLR"]
        for scale in (100, 400, 1000):
            name = f"{hemi}-{scale}"
            expected_table = json.loads((work / "surface-inputs" / f"labels-{scale}.json").read_text())
            expected = {int(k) for k, label in expected_table.items()
                        if int(k) != 0 and label.startswith(f"7Networks_{hemi}H_")}
            other = {int(k) for k in expected_table if int(k) != 0} - expected
            assert len(expected) == scale // 2
            template = nb.load(work / "surface-inputs" / f"fsaverage-{hemi}-{scale}.label.gii")
            sampled = np.fromfile(exported / f"{name}-sampled.bin", "<f8")
            source_roi = (np.fromfile(exported / f"{name}-sampled-available.bin", "<i4") > 0) & src["cortex"]
            assert source_roi.shape == sampled.shape
            assert expected <= set(sampled[source_roi])
            label_path = out / f"{name}-sampled.label.gii"
            nb.save(nb.gifti.GiftiImage(darrays=[nb.gifti.GiftiDataArray(
                np.where(source_roi, sampled, 0).astype(np.int32),
                intent="NIFTI_INTENT_LABEL")], labeltable=template.labeltable), label_path)
            roi_path = out / f"{name}-sampled-roi.shape.gii"
            metric(roi_path, source_roi)
            reference = values(work / "surface-inputs" / f"fsLR-{hemi}-{scale}.label.gii")
            outputs = {}
            for method in ("BARYCENTRIC", "ADAP_BARY_AREA"):
                for largest in (False, True):
                    method_name = f"{method}-{'largest' if largest else 'aggregate'}"
                    label_out = out / f"{name}-{method_name}.label.gii"
                    valid_out = out / f"{name}-{method_name}-valid.shape.gii"
                    args = ["wb_command", "-label-resample", str(label_path), str(src["sphere"]),
                            str(dst["sphere"]), method, str(label_out), "-current-roi", str(roi_path),
                            "-valid-roi-out", str(valid_out)]
                    if largest:
                        args.append("-largest")
                    if method == "ADAP_BARY_AREA":
                        args += ["-area-metrics", str(src["area"]), str(dst["area"])]
                    subprocess.run(args, check=True, capture_output=True)
                    available = (values(valid_out) > 0) & dst["cortex"]
                    output = values(label_out)
                    assert set(output) <= {int(k) for k in expected_table}
                    outputs[method_name] = (output, available)
            for native in ("aggregate", "largest"):
                output = np.fromfile(exported / f"{name}-{native}.bin", "<f8")
                available = np.fromfile(exported / f"{name}-{native}-available.bin", "<i4") > 0
                outputs[f"native-{native}"] = (output, available)
                wb_values, wb_available = outputs[f"BARYCENTRIC-{native}"]
                joint = available & wb_available
                availability_differences = int((available != wb_available).sum())
                label_differences = int((output[joint] != wb_values[joint]).sum())
                parity.append(dict(case=name, method=native,
                                   joint_vertices=int(joint.sum()),
                                   availability_differences=availability_differences,
                                   label_differences=label_differences,
                                   passed=availability_differences == 0 and label_differences == 0))
            for method, (output, available) in outputs.items():
                counts = {key: int(((output == key) & available).sum()) for key in sorted(expected)}
                lost = [key for key, count in counts.items() if count == 0]
                foreign = int((np.isin(output, list(other)) & available).sum())
                disagreements = int(((~available | (output != reference)) & dst["cortex"]).sum())
                dice = []
                for key in sorted(expected):
                    a = (output == key) & available
                    b = (reference == key) & dst["cortex"]
                    den = int(a.sum() + b.sum())
                    dice.append(2 * int((a & b).sum()) / den if den else None)
                cases.append(dict(case=name, hemisphere=hemi, parcels=scale, method=method,
                                  expected_labels=len(expected), lost_labels=lost,
                                  opposite_hemisphere_vertices=foreign,
                                  available_vertices=int(available.sum()),
                                  reference_disagreement_fraction=disagreements / int(dst["cortex"].sum()),
                                  median_parcel_dice=float(np.median([x for x in dice if x is not None])),
                                  counts=counts, retention_gate_passed=not lost and foreign == 0))
            source_path = work / "inputs" / f"Schaefer2018_{scale}Parcels_7Networks_order_FSLMNI152_1mm.nii.gz"
            keys, counts = np.unique(np.asanyarray(nb.load(source_path).dataobj), return_counts=True)
            source_counts = dict(zip(keys, counts))
            target_values, target_available = outputs["native-aggregate"]
            with (exported / f"{name}-labels.csv").open() as stream:
                for row in csv.DictReader(stream):
                    key = int(row["key"])
                    assert int(row["source_voxels"]) == source_counts.get(key, 0)
                    assert int(row["sampled_vertices"]) == int(((sampled == key) & source_roi).sum())
                    assert int(row["target_vertices"]) == int(((target_values == key) & target_available).sum())
            print(name, "compared", flush=True)
    files = {path.name: sha(path) for path in sorted(out.iterdir()) if path.is_file()}
    report = dict(protocol_sha256=sha(protocol), driver_sha256=sha(Path(__file__)),
                  export_receipt_sha256=sha(exported / "receipt.json"), workbench_version=version,
                  numpy_version=np.__version__, nibabel_version=nb.__version__, cases=cases,
                  diagnostic_counts_verified=True,
                  native_workbench_parity_passed=all(case["passed"] for case in parity),
                  native_workbench_parity=parity, files=files)
    (out / "report.json").write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("work", "exported", "pinned", "out"):
        parser.add_argument(name, type=Path)
    args = parser.parse_args()
    run(args.work, args.exported, args.pinned, args.out)

#!/usr/bin/env python3
"""Bind CBIG coordinates, SimpleITK pullbacks and Workbench point sampling."""
from pathlib import Path
import hashlib
import json
import subprocess
import sys

import nibabel as nb
import numpy as np
from scipy.io import loadmat
from scipy.interpolate import interpn
from scipy.spatial import cKDTree
import SimpleITK as sitk


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


root = Path(sys.argv[1])
work = root.parent
repository = Path.cwd()
contract_path = Path(__file__).with_name("contract-v1.json")
contract = json.loads(contract_path.read_text())
receipt_path = root / "consumer-receipt.json"
receipt = json.loads(receipt_path.read_text())
assert sha(contract_path) == receipt["contract_sha256"]
lock_path = repository / "inst/extdata/projection-inputs-v1.json"
assert sha(lock_path) == receipt["source_sha256"][str(lock_path.relative_to(repository))]
lock = json.loads(lock_path.read_text())
surface_lock_path = repository / "inst/extdata/surface-inputs-v1.json"
assert sha(surface_lock_path) == receipt["source_sha256"][
    str(surface_lock_path.relative_to(repository))]
surface_lock = json.loads(surface_lock_path.read_text())
inputs = work / "inputs"
cache = work / "public-surface-cache"
output = root / "independent-references"
output.mkdir(exist_ok=False)
reports = []
cbig = {}
ordering = []


def pinned_input(name):
    entry = next(a for a in surface_lock["assets"] if a["path"] == name)
    path = inputs / name
    assert sha(path) == entry["sha256"]
    return path


for hemi, entry in lock["assets"].items():
    path = cache / "projection-inputs-v1" / (entry["sha256"] + ".mat")
    assert sha(path) == entry["sha256"]
    cbig[hemi] = loadmat(path)["ras"].T
    audit = entry["target_ordering_audit"]
    sphere_path = work / f"archives/cbig-fsaverage-{hemi}-sphere.gii"
    assert sha(sphere_path) == audit["asset"]["sha256"]
    upstream = nb.load(sphere_path)
    pinned_path = pinned_input(f"tpl-fsaverage_hemi-{hemi}_den-164k_sphere.surf.gii")
    pinned = nb.load(pinned_path)
    pointset = "NIFTI_INTENT_POINTSET"
    triangles = "NIFTI_INTENT_TRIANGLE"
    u = upstream.get_arrays_from_intent(pointset)[0].data
    p = pinned.get_arrays_from_intent(pointset)[0].data
    assert np.array_equal(upstream.get_arrays_from_intent(triangles)[0].data,
                          pinned.get_arrays_from_intent(triangles)[0].data)
    distance, indices = cKDTree(p).query(u)
    mismatches = int(np.count_nonzero(indices != np.arange(len(p))))
    assert mismatches == 0
    ordering.append(dict(hemisphere=hemi, upstream_sha256=sha(sphere_path),
        pinned_sha256=sha(pinned_path), ordered_triangles_equal=True,
        nearest_vertex_index_mismatches=mismatches,
        maximum_coordinate_difference=float(np.max(np.abs(u - p))),
        maximum_matched_vertex_distance=float(distance.max())))

for name in receipt["cases"]:
    if name.startswith("synthetic") or "fsaverage" not in name:
        continue
    folder = root / name
    assert sha(folder / "case.json") == receipt["case_sha256"][name]
    m = json.loads((folder / "case.json").read_text())
    if m["data_type"] != "continuous":
        continue
    for file, expected in m["files"].items():
        assert sha(folder / file) == expected
    hemi = m["specification"]["target"]["hemisphere"]
    points = np.fromfile(folder / "points-1.bin", "<f8").reshape(-1, 3)
    cortex = np.fromfile(folder / "cortex.bin", "<i4").astype(bool)
    frame = m["specification"]["from_space"]
    if frame == "MNI152NLin6Asym":
        expected_points = cbig[hemi]
    else:
        route = m["specification"]["volume_route"]
        # jsonlite serializes a one-row data frame as a list of row objects.
        if isinstance(route, list):
            route = route[0]
        warp_path = cache / route["artifact_version"] / (route["artifact_id"] + ".h5")
        assert sha(warp_path) == route["sha256"]
        transform = sitk.ReadTransform(str(warp_path))
        sign = np.array([-1, -1, 1])
        expected_points = np.array([transform.TransformPoint(tuple(point * sign))
                                    for point in cbig[hemi]]) * sign
    point_error = float(np.max(np.abs(points - expected_points)))
    assert point_error <= contract["acceptance"]["warp_coordinate_absolute_error_mm"], (name, point_error)

    volume_path = root / m["volume"]
    assert sha(volume_path) == m["volume_sha256"]
    dims = tuple(m["dimensions"])
    volume = np.fromfile(volume_path, "<f8").reshape(dims, order="F")
    affine = np.array(m["affine"])
    nifti = output / (name + ".nii.gz")
    nb.save(nb.Nifti1Image(volume.astype("float32"), affine), nifti)
    sphere = pinned_input(f"tpl-fsaverage_hemi-{hemi}_den-164k_sphere.surf.gii")
    faces = nb.load(sphere).get_arrays_from_intent("NIFTI_INTENT_TRIANGLE")[0].data
    surface = nb.gifti.GiftiImage(darrays=[
        nb.gifti.GiftiDataArray(points.astype("float32"), intent="NIFTI_INTENT_POINTSET"),
        nb.gifti.GiftiDataArray(faces.astype("int32"), intent="NIFTI_INTENT_TRIANGLE")])
    surface.meta["AnatomicalStructurePrimary"] = "CortexLeft" if hemi == "L" else "CortexRight"
    surface_path = output / (name + ".surf.gii")
    nb.save(surface, surface_path)
    metric_path = output / (name + ".func.gii")
    command = ["wb_command", "-volume-to-surface-mapping", str(nifti),
               str(surface_path), str(metric_path), "-trilinear"]
    completed = subprocess.run(command, capture_output=True, text=True, check=True)
    (output / (name + ".log")).write_text(completed.stdout + completed.stderr)
    actual = np.fromfile(folder / "values.bin", "<f8")
    available = np.fromfile(folder / "available.bin", "<i4").astype(bool)
    reference = nb.load(metric_path).darrays[0].data
    error = float(np.max(np.abs(actual[available] - reference[available])))
    assert error <= contract["acceptance"]["workbench_float32_absolute_error"], (name, error)
    reports.append(dict(name=name, points=len(points), included=int(cortex.sum()),
                        maximum_point_error_mm=point_error,
                        workbench_maximum_scalar_error=error,
                        reference_metric_sha256=sha(metric_path)))
    print(json.dumps(reports[-1]), flush=True)

# Audit exact upstream reference-volume identity without equating whole templates.
original_path = work / "archives/cbig-FSL-MNI152-original.mgz"
template_path = work / "archives/templateflow-MNI6-T1w.nii.gz"
assert sha(original_path) == "f349c757fd223e76215cf9a4518f56694519bada13f2442d04d05c168b1cd5de"
assert sha(template_path) == "f675946c0a077e0682885c3d6085f10acb0dd22b2274a6cbbf04969c08a77258"
original = nb.as_closest_canonical(nb.load(original_path))
template = nb.as_closest_canonical(nb.load(template_path))
assert original.shape == template.shape
assert np.array_equal(original.affine, template.affine)
difference = original.get_fdata() - template.get_fdata()
axes = tuple(np.arange(d) for d in original.shape)
support = []
for hemi in ("L", "R"):
    mask_path = pinned_input(f"tpl-fsaverage_den-164k_hemi-{hemi}_desc-nomedialwall_dparc.label.gii")
    cortex = nb.load(mask_path).darrays[0].data.astype(bool)
    voxel = nb.affines.apply_affine(np.linalg.inv(original.affine), cbig[hemi][cortex])
    discrepancies = interpn(axes, difference, voxel, bounds_error=True)
    maximum = float(np.max(np.abs(discrepancies)))
    assert maximum == 0, (hemi, maximum)
    support.append(dict(hemisphere=hemi, sampled_cortical_vertices=int(cortex.sum()),
                        maximum_sampled_reference_image_difference=maximum))
result = dict(status="PASS", contract_sha256=sha(contract_path),
    consumer_receipt_sha256=sha(receipt_path), reference_driver_sha256=sha(Path(__file__)),
    workbench_version=subprocess.check_output(["wb_command", "-version"], text=True),
    simpleitk_version=sitk.Version_VersionString(),
    workbench_binary_sha256=sha(Path(subprocess.check_output(["which", "wb_command"], text=True).strip())),
    projection_cases=reports,
    target_ordering_audit=ordering,
    source_frame_audit=dict(canonical_shape=list(original.shape),
        canonical_affine=original.affine.tolist(),
        changed_whole_volume_voxels=int(np.count_nonzero(difference)),
        limitation="Whole volumes differ; equivalence is restricted to sampled cortical support",
        hemispheres=support))
(root / "reference-receipt.json").write_text(json.dumps(result, indent=2) + "\n")

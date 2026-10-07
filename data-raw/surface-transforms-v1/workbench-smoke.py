#!/usr/bin/env python3
"""Exercise Workbench on pinned inputs. This is readiness, not qualification."""

import argparse
import base64
import hashlib
import json
import math
import os
from pathlib import Path
import re
import struct
import subprocess
import xml.etree.ElementTree as ET
import zlib


def metric_values(path):
    """Read one scalar GIFTI array, rejecting unsupported encodings/shapes."""
    arrays = ET.parse(path).getroot().findall("DataArray")
    if len(arrays) != 1:
        raise ValueError("Expected exactly one GIFTI array: " + str(path))
    array = arrays[0]
    dims = [int(array.attrib["Dim" + str(i)])
            for i in range(int(array.attrib["Dimensionality"]))]
    if len(dims) > 2 or (len(dims) == 2 and dims[1] != 1):
        raise ValueError("Expected a scalar vertex map")
    text = array.findtext("Data", "")
    encoding = array.attrib["Encoding"]
    if encoding == "ASCII":
        values = [float(v) for v in text.split()]
    else:
        data = base64.b64decode(text)
        if encoding == "GZipBase64Binary":
            data = zlib.decompress(data, wbits=47)
        elif encoding != "Base64Binary":
            raise ValueError("Unsupported GIFTI encoding: " + encoding)
        fmt = {"NIFTI_TYPE_FLOAT32": "f", "NIFTI_TYPE_INT32": "i"}[
            array.attrib["DataType"]]
        endian = {"LittleEndian": "<", "BigEndian": ">"}[array.attrib["Endian"]]
        values = list(struct.unpack(endian + str(dims[0]) + fmt, data))
    if len(values) != dims[0] or not all(math.isfinite(v) for v in values):
        raise ValueError("Wrong length or nonfinite GIFTI data")
    return values


def check_case(metric, labels, valid, target_roi, expected_n, constant, keys):
    arrays = [metric, labels, valid, target_roi]
    if any(len(a) != expected_n for a in arrays):
        raise ValueError("Wrong output vertex count")
    if any(v not in (0, 1) for v in valid + target_roi):
        raise ValueError("Nonbinary coverage or mask")
    covered = [i for i in range(expected_n) if valid[i] and target_roi[i]]
    if not covered:
        raise ValueError("No cortical output coverage")
    error = max(abs(metric[i] - constant) for i in covered)
    if error > 1e-5:
        raise ValueError("Constant field failed: " + str(error))
    if any(metric[i] != 0 for i in range(expected_n) if not target_roi[i]):
        raise ValueError("Target medial wall contains data")
    if any(v != int(v) or v not in keys for v in labels):
        raise ValueError("Output has unknown/fractional labels")
    if not any(v != 0 for v in labels):
        raise ValueError("All labels lost")
    return {"vertices": expected_n, "covered_cortical_vertices": len(covered),
            "target_cortical_vertices": int(sum(target_roi)),
            "max_constant_error": error, "observed_labels": sorted(set(labels))}


def validate_label_fixture(path, allowed):
    table = ET.parse(path).getroot().find("LabelTable")
    if table is None:
        raise ValueError("Missing label table")
    entries = table.findall("Label")
    keys = [int(label.attrib["Key"]) for label in entries]
    unassigned = [label for label in entries if label.text == "???"]
    if (len(keys) != len(set(keys)) or set(keys) != set(allowed) or
            len(unassigned) != 1 or unassigned[0].attrib["Key"] != "0" or
            float(unassigned[0].attrib["Alpha"]) != 0):
        raise ValueError("Fixture requires explicit transparent unassigned key 0")
    if any(v not in allowed for v in metric_values(path)):
        raise ValueError("Fixture values absent from label table")


def main(bundle, output):
    if output.exists():
        raise FileExistsError("Use a new attempt directory: " + str(output))
    output.mkdir(parents=True)
    commands = output / "commands.jsonl"

    def wb(*args):
        command = ["wb_command"] + [str(arg) for arg in args]
        result = subprocess.run(command, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True)
        with commands.open("a") as log:
            log.write(json.dumps({"argv": command, "returncode": result.returncode,
                                  "output": result.stdout}) + "\n")
        if result.returncode:
            raise RuntimeError("Workbench failed; see " + str(commands))
        return result.stdout

    version = wb("-version")
    if not re.search(r"\b2\.0\.1\b", version):
        raise ValueError("Expected Workbench 2.0.1; got " + version)
    fixtures = bundle / "fixtures"
    inputs = bundle / "inputs"
    domains = json.loads((fixtures / "domains.json").read_text())["domains"]
    for domain in domains.values():
        validate_label_fixture(fixtures / (domain["id"] + "-labels.label.gii"),
                               domain["allowed_labels"])
    results = []
    for hemi in ("L", "R"):
        for source_tpl, target_tpl in (("fsaverage-164k", "fsLR-32k"),
                                       ("fsLR-32k", "fsaverage-164k")):
            source = domains[source_tpl + "-" + hemi]
            target = domains[target_tpl + "-" + hemi]
            stem = source["id"] + "_to_" + target["id"]
            metric = output / (stem + ".shape.gii")
            masked = output / (stem + "-masked.shape.gii")
            valid = output / (stem + "-valid.shape.gii")
            labels = output / (stem + ".label.gii")
            common = [inputs / source["sphere"], inputs / target["sphere"],
                      "ADAP_BARY_AREA"]
            options = ["-area-metrics", inputs / source["area"],
                       inputs / target["area"], "-current-roi",
                       fixtures / (source["id"] + "-roi.shape.gii")]
            wb("-metric-resample", fixtures / (source["id"] + "-constant.shape.gii"),
               *common, metric, *options, "-valid-roi-out", valid)
            target_roi = fixtures / (target["id"] + "-roi.shape.gii")
            wb("-metric-mask", metric, target_roi, masked)
            wb("-label-resample", fixtures / (source["id"] + "-labels.label.gii"),
               *common, labels, *options)
            result = check_case(metric_values(masked), metric_values(labels),
                                metric_values(valid), metric_values(target_roi),
                                target["vertices"], source["constant"],
                                source["allowed_labels"])
            result.update(source=source["id"], target=target["id"])
            results.append(result)
    artifacts = {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                 for p in output.iterdir() if p.is_file()}
    receipt = {"schema": "neuroatlas.workbench-readiness.v1", "passed": True,
               "qualification": "not_qualified", "workbench_version": version,
               "slurm_job_id": os.environ.get("SLURM_JOB_ID"),
               "input_lock_sha256": hashlib.sha256(
                   (bundle / "inputs.lock.json").read_bytes()).hexdigest(),
               "bundle_manifest_sha256": hashlib.sha256(
                   (bundle / "SHA256SUMS").read_bytes()).hexdigest(),
               "cases": results, "output_sha256": artifacts}
    (output / "readiness.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(json.dumps({"passed": True, "cases": len(results),
                      "qualification": "not_qualified"}))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bundle", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    main(args.bundle.resolve(), args.output.resolve())

"""Fetch pinned public QA assets; verify TemplateFlow annex digests."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import urllib.request
from concurrent.futures import ThreadPoolExecutor

REVISIONS = {
    "MNI152NLin6Asym": "c906e8d808a34719e5024a4bde61f03a4e411ddd",
    "MNI152NLin2009cAsym": "15d7c02160f79f5218d2545b4febebeecc11531d",
    "fsaverage": "8e53ba4f2e438758f69d11436fe0cd291a28bec6",
    "fsLR": "ca545b4721c2858decef9bbca302c1eac4d0d8bf",
}
CBIG = "634f676630929a71297852d01dd92a287103e861"


def fetch(out):
    out.mkdir(parents=True, exist_ok=True)
    jobs = []
    for template in list(REVISIONS)[:2]:
        for res in ("01", "02"):
            for suffix in ("desc-brain_T1w.nii.gz", "desc-brain_mask.nii.gz",
                           "atlas-HOCPA_desc-th25_dseg.nii.gz",
                           "atlas-HOSPA_desc-th25_dseg.nii.gz"):
                jobs.append((template, f"tpl-{template}_res-{res}_{suffix}"))
        jobs.append((template, "template_description.json"))
    for atlas in ("HOCPA", "HOSPA"):
        jobs.append(("MNI152NLin6Asym",
                     f"tpl-MNI152NLin6Asym_atlas-{atlas}_dseg.tsv"))
    for hemi in ("L", "R"):
        for scale in (100, 400, 1000):
            jobs.append(("fsaverage", f"tpl-fsaverage_hemi-{hemi}_den-164k_"
                         f"atlas-Schaefer2018_seg-7n_scale-{scale}_dseg.label.gii"))
        for kind in ("sulc", "curv"):
            jobs.append(("fsaverage", f"tpl-fsaverage_hemi-{hemi}_den-164k_"
                         f"{kind}.shape.gii"))
    for scale in (100, 400, 1000):
        jobs.append(("MNI152NLin6Asym", "tpl-MNI152NLin6Asym_res-01_"
                     f"atlas-Schaefer2018_desc-{scale}Parcels7Networks_dseg.nii.gz"))
        jobs.append(("MNI152NLin6Asym", "tpl-MNI152NLin6Asym_"
                     f"atlas-Schaefer2018_desc-{scale}Parcels7Networks_dseg.tsv"))

    def download(job):
        template, name = job
        raw_url = (f"https://raw.githubusercontent.com/templateflow/tpl-{template}/"
                   f"{REVISIONS[template]}/{name}")
        raw = urllib.request.urlopen(raw_url).read()
        annex = re.search(rb"(MD5E|SHA256E)-s(\d+)--([a-f0-9]+)", raw)
        url = f"https://templateflow.s3.amazonaws.com/tpl-{template}/{name}"
        path = out / (template + "-" + name if name == "template_description.json" else name)
        content = path.read_bytes() if path.exists() else (
            urllib.request.urlopen(url).read() if annex else raw)
        if annex:
            algorithm = "md5" if annex[1] == b"MD5E" else "sha256"
            assert len(content) == int(annex[2]), name
            assert hashlib.new(algorithm, content).hexdigest() == annex[3].decode(), name
        else:
            assert content == raw
        path.write_bytes(content)
        return {"path": path.name, "url": url if annex else raw_url,
                "revision": REVISIONS[template], "annex": annex[0].decode() if annex else None,
                "bytes": len(content), "sha256": hashlib.sha256(content).hexdigest()}

    receipts = list(ThreadPoolExecutor(6).map(download, jobs))
    for scale in (100, 400, 1000):
        name = f"Schaefer2018_{scale}Parcels_7Networks_order.dlabel.nii"
        url = (f"https://raw.githubusercontent.com/ThomasYeoLab/CBIG/{CBIG}/"
               "stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/"
               f"Parcellations/HCP/fslr32k/cifti/{name}")
        path = out / name
        content = urllib.request.urlopen(url).read()
        path.write_bytes(content)
        receipts.append({"path": name, "url": url, "revision": CBIG,
                         "bytes": len(content), "sha256": hashlib.sha256(content).hexdigest()})
    (out / "inputs.json").write_text(json.dumps(receipts, indent=2) + "\n")
    print(f"Verified {len(receipts)} assets", flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    fetch(parser.parse_args().output)

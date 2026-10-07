"""Record software, checks and final packet hashes after the campaign completes."""
import hashlib
import importlib.metadata
import json
from pathlib import Path
import shutil
import subprocess
import sys

work, packet = map(Path, sys.argv[1:])
base = Path(__file__).parent
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
checks = json.loads((work / "checks.json").read_text())
assert checks["tests"]["failed"] == 0 and checks["tests"]["error"] == 0
assert not checks["check"]["errors"] and not checks["check"]["warnings"]
metadata = {
    "schema": "neuroatlas.transform-quality-campaign.v1",
    "measurement_status": "completed_with_limitations",
    "anatomical_ground_truth": "not_established",
    "projection_no_label_loss": "FAIL",
    "source_commit": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
    "protocol_frozen_utc": "2026-10-07T10:33:47Z",
    "protocol_sha256": sha(base / "protocol.md"),
    "work_directory": str(work),
    "python_packages": {p: importlib.metadata.version(p) for p in
                        ("numpy", "nibabel", "SimpleITK", "scipy", "matplotlib")},
    "scripts": {p.name: sha(p) for p in base.iterdir() if p.suffix in (".R", ".py")},
    "source_manifests": {str(p): sha(p) for p in (
        Path("inst/extdata/transform_registry.csv"), Path("inst/extdata/surface-inputs-v1.json"),
        Path("inst/extdata/projection-inputs-v1.json"))},
    "verification": (work / "verify.log").read_text().strip(),
    "human_expert_review": "pending",
    "package_checks": checks,
}
(packet / "campaign.json").write_text(json.dumps(metadata, indent=2) + "\n")
shutil.copyfile(work / "checks.json", packet / "checks.json")
shutil.copyfile(work / "verify.log", packet / "verify.log")
index = packet / "index.html"
html = index.read_text()
if "campaign.json" not in html:
    html = html.replace("</body>", "<p><a href='campaign.json'>Campaign bindings</a> | "
                        "<a href='checks.json'>Package checks</a></p></body>")
    index.write_text(html)
(packet / "SHA256SUMS").write_text("".join(
    f"{sha(p)}  {p.name}\n" for p in sorted(packet.iterdir()) if p.is_file() and p.name != "SHA256SUMS"))
print("Final evidence packet bound to scripts, software and package checks")

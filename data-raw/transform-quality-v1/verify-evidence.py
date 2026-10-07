"""Verify raw campaign bindings, repeated metrics and committed review files."""
import hashlib
import json
from pathlib import Path
import sys

work, packet = map(Path, sys.argv[1:])
base = Path(__file__).parent
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
drivers = {
    "volume-02": "volume-quality.py", "surface-02": "surface-quality.py",
    "resolution-03": "diagnose-resolution.py", "t2-01": "t2-quality.py",
    "public-03": "public-api.R", "stages-01": "projection-stages.R",
    "masks-01": "mask-boundary-audit.py",
}
checked = 0
for folder, driver in drivers.items():
    receipt = json.loads((work / folder / "receipt.json").read_text())
    assert sha(base / driver) == receipt.get("script_sha256", receipt.get("driver_sha256")), driver
    if "protocol_sha256" in receipt:
        protocol = "t2-protocol.md" if folder == "t2-01" else "protocol.md"
        assert sha(base / protocol) == receipt["protocol_sha256"]
    for filename, expected in receipt.get("files", {}).items():
        assert sha(work / folder / filename) == expected, (folder, filename)
        checked += 1
    for path in (work / folder).iterdir():
        if path.suffix in (".json", ".csv", ".png"):
            assert sha(path) == sha(packet / (folder + "-" + path.name)), path
for filename in ("inputs.json", "cbig-volume-inputs-verified.json"):
    for item in json.loads((work / "inputs" / filename).read_text()):
        assert sha(work / "inputs" / item["path"]) == item["sha256"], item["path"]
        checked += 1
for item in json.loads((work / "t2-01/receipt.json").read_text())["inputs"]:
    assert sha(work / "inputs" / item["path"]) == item["sha256"]
    checked += 1
repeat_verified = (work / "volume-01/receipt.json").exists() and (work / "resolution-01/receipt.json").exists()
if repeat_verified:
    first = json.loads((work / "volume-01/receipt.json").read_text())
    final = json.loads((work / "volume-02/receipt.json").read_text())
    assert first["cases"] == final["cases"], "Presentation repeat changed numerical results"
    assert sha(work / "volume-01/regions.csv") == sha(work / "volume-02/regions.csv")
    for filename in ("within-template-resolution.csv", "1mm-source-to-2mm-sensitivity.csv"):
        assert sha(work / "resolution-01" / filename) == sha(work / "resolution-03" / filename)
for line in (packet / "SHA256SUMS").read_text().splitlines():
    expected, filename = line.split("  ", 1)
    assert sha(packet / filename) == expected, filename
    checked += 1
for path in base.iterdir():
    if path.suffix in (".py", ".R"):
        assert path.read_text().isascii(), path
repeat_note = "numerical repeat identical" if repeat_verified else "historical repeat not available on this machine"
print(f"Verified {checked} raw/input/packet hashes, exact script and protocol bindings; {repeat_note}")

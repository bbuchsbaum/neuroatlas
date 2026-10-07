"""Create a compact two-frame GIF from the verified visual supplement."""
import argparse
import base64
import hashlib
import io
import json
from pathlib import Path
import re

from PIL import Image, ImageDraw, ImageFont, __version__ as pillow_version


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def build(root, font_path):
    supplement = root / "data-raw/transform-quality-v1/visuals-20261007"
    html = supplement / "index.html"
    receipt = json.loads((supplement / "receipt.json").read_text())
    assert sha(html) == receipt["outputs"]["index.html"]
    panels = json.loads(re.search(
        r"const panels = (.*);\nconst byId", html.read_text()).group(1))
    selected = [p for p in panels if p["label"] ==
                "MNI6Asym to MNI2009cAsym | 1 mm / Ventricles / deep gray / "
                "z = 10 mm (RAS)"]
    assert len(selected) == 1
    panel = selected[0]
    anatomy = {}
    for key in ("reference", "before", "after"):
        image = Image.open(io.BytesIO(base64.b64decode(panel[key].split(",")[1])))
        assert image.mode == "L", "Use the existing 8-bit grayscale samples"
        anatomy[key] = image
    assert len({image.size for image in anatomy.values()}) == 1
    width, height = anatomy["reference"].size
    scale, margin, gap, top, bottom = 3, 16, 20, 100, 50
    canvas = (2 * width * scale + 2 * margin + gap,
              height * scale + top + bottom)
    font = ImageFont.truetype(str(font_path), 14)
    title_font = ImageFont.truetype(str(font_path), 17)
    palette = [channel for value in range(64)
               for channel in (round(value * 255 / 63),) * 3] + [0] * (192 * 3)
    boxes, frames = [], []
    for target in (True, False):
        frame = Image.new("L", canvas, 16)
        draw = ImageDraw.Draw(frame)
        draw.text((margin, 10), "MNI6 to MNI2009c | 1 mm | axial z = 10 mm",
                  font=title_font, fill=255)
        state = "TARGET: identical reference in both panels" if target else \
                "SOURCE: before alignment / after alignment"
        draw.text((margin, 40), state, font=font, fill=235)
        for col, key in enumerate(("before", "after")):
            x = margin + col * (width * scale + gap)
            draw.text((x, 72), "Before: identity" if col == 0 else
                      "After: released transform", font=title_font, fill=255)
            image = anatomy["reference" if target else key].resize(
                (width * scale, height * scale), Image.Resampling.NEAREST)
            frame.paste(image, (x, top))
            box = (x, top, x + width * scale, top + height * scale)
            if target:
                boxes.append(box)
            draw.text((x, top + height * scale + 2), "L", font=font, fill=255)
            draw.text((x + width * scale - 13, top + height * scale + 2),
                      "R", font=font, fill=255)
        draw.text((margin, canvas[1] - 23),
                  "Watch the ventricular walls: less motion indicates closer alignment.",
                  font=font, fill=235)
        indexed = Image.new("P", canvas)
        indexed.putpalette(palette)
        indexed.putdata(frame.point(lambda value: round(value * 63 / 255)).tobytes())
        frames.append(indexed)
    output = root / "vignettes/figures/template-transform-blink.gif"
    frames[0].save(output, save_all=True, append_images=frames[1:],
                   duration=1000, loop=0, optimize=False, disposal=2)
    # Enforce a compact asset and lossless frame decoding with a shared palette.
    assert output.stat().st_size < 128_000
    decoded = Image.open(output)
    assert decoded.n_frames == 2 and decoded.info["loop"] == 0
    for i, frame in enumerate(frames):
        decoded.seek(i)
        assert decoded.info["duration"] == 1000
        assert decoded.convert("RGB").tobytes() == frame.convert("RGB").tobytes()
    assert frames[0].crop(boxes[0]).tobytes() == frames[0].crop(boxes[1]).tobytes()
    assert frames[1].crop(boxes[0]).tobytes() != frames[1].crop(boxes[1]).tobytes()
    manifest = dict(
        source_html_sha256=sha(html), selected_view=panel["label"],
        script_sha256=sha(__file__), pillow=pillow_version,
        font_sha256=sha(font_path), dimensions=list(canvas), frames=2,
        frame_duration_ms=1000, palette="fixed 64-level grayscale; no dithering",
        magnification="3x nearest neighbor; same intensity window; 64-level quantization",
        gif_sha256=sha(output), gif_bytes=output.stat().st_size,
        checks="Two decoded frames equal rendered frames byte-for-byte; "
               "target panels identical; source panels differ; GIF below 128 kB.")
    (root / "data-raw/transform-quality-v1/vignette-blink-receipt.json").write_text(
        json.dumps(manifest, indent=2) + "\n")
    print(json.dumps(manifest, indent=2))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument("--font", type=Path, default=Path(
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"))
    args = parser.parse_args()
    build(args.root, args.font)

"""Generate and independently verify small synthetic CIFTI-2 fixtures."""

import argparse
import json
from pathlib import Path
import struct

import nibabel as nib
import numpy as np
from nibabel.cifti2 import cifti2_axes as axes


def generate(root):
    root.mkdir(parents=True, exist_ok=True)
    affine = np.array([[2, 0, 0, 10], [0, -3, 0, 20],
                       [0, 0, 4, -30], [0, 0, 0, 1]], dtype=float)
    mask = np.zeros((2, 1, 2), dtype=bool)
    mask[0, 0, 0] = mask[1, 0, 1] = True
    brain = (axes.BrainModelAxis.from_surface([3, 0, 5], 6, 'CortexRight')
             + axes.BrainModelAxis.from_mask(mask, affine=affine, name='ThalamusLeft')
             + axes.BrainModelAxis.from_surface([4, 1, 2, 0], 6, 'CortexLeft'))
    cases = []
    for kind in ['scalar', 'label']:
        for brain_axis in [0, 1]:
            for maps in [1, 2]:
                names = ['map & one', 'second map'][:maps]
                metadata = [{'note': 'preserve <metadata> & values'}] * maps
                if kind == 'scalar':
                    mapping = axes.ScalarAxis(names, metadata)
                    data = np.arange(9 * maps, dtype=float).reshape(9, maps)
                    data[3, 0] = np.nan
                else:
                    tables = [
                        {0: ('unassigned', (0, 0, 0, 0)),
                         -3: ('negative & shared', (1, 0, 0, 1)),
                         2: ('first', (0, .25, 1, .5))},
                        {0: ('unassigned', (0, 0, 0, 0)),
                         -3: ('negative & shared', (1, 0, 0, 1)),
                         7: ('second', (.5, .25, 1, 1))},
                    ][:maps]
                    mapping = axes.LabelAxis(names, tables, metadata)
                    data = np.column_stack([
                        np.resize([0, -3, 2 if i == 0 else 7], 9)
                        for i in range(maps)]).astype(float)
                for endian in ['<', '>']:
                    case = f'{kind}-axis{brain_axis}-maps{maps}-' + ('le' if endian == '<' else 'be')
                    header = nib.Nifti2Header(endianness=endian)
                    order = (brain, mapping) if brain_axis == 0 else (mapping, brain)
                    stored = data if brain_axis == 0 else data.T
                    image = nib.Cifti2Image(stored,
                        header=nib.Cifti2Header.from_axes(order), nifti_header=header)
                    image.nifti_header.set_intent('ConnDenseLabel' if kind == 'label' else 'ConnDenseScalar')
                    image.nifti_header.extensions.append(
                        nib.nifti1.Nifti1Extension(6, b'extra extension preserved'))
                    file = root / (case + f'.d{kind}.nii')
                    nib.save(image, file)
                    cases.append(file.name)
    # NIfTI scaling is independent of the CIFTI XML and must survive transport.
    source = root / cases[0]
    raw = bytearray(source.read_bytes())
    raw[176:184] = struct.pack('<d', 2.)
    raw[184:192] = struct.pack('<d', 5.)
    scaled = root / 'scalar-scaled.dscalar.nii'
    scaled.write_bytes(raw)
    cases.append(scaled.name)
    (root / 'cases.json').write_text(json.dumps(cases, indent=2) + '\n')
    print(f'Generated {len(cases)} independent fixtures.')


def verify(root):
    cases = json.loads((root / 'cases.json').read_text())
    results = []
    for name in cases:
        source = nib.load(root / name)
        target = nib.load(root / ('roundtrip-' + name))
        a = np.asarray(source.dataobj, dtype=float)
        b = np.asarray(target.dataobj, dtype=float)
        assert a.shape == b.shape, (name, a.shape, b.shape)
        assert np.array_equal(a, b, equal_nan=True), name
        for i in range(2):
            assert source.header.get_axis(i) == target.header.get_axis(i), (name, i)
        assert source.header.to_xml() == target.header.to_xml(), name
        source_extra = [x.get_content() for x in source.nifti_header.extensions if x.get_code() != 32]
        target_extra = [x.get_content() for x in target.nifti_header.extensions if x.get_code() != 32]
        assert source_extra == target_extra, name
        results.append({'file': name, 'shape': list(a.shape), 'max_abs_error': 0})
    receipt = {'status': 'PASS', 'cases': results,
               'nibabel_version': nib.__version__, 'numpy_version': np.__version__}
    (root / 'io-oracle-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(f'PASS: {len(cases)} independent round trips; values/axes/metadata exact.')


parser = argparse.ArgumentParser()
parser.add_argument('action', choices=['generate', 'verify'])
parser.add_argument('directory', type=Path)
args = parser.parse_args()
generate(args.directory) if args.action == 'generate' else verify(args.directory)

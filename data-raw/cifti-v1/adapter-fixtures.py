"""Independent CIFTI encoding and inspection for the cortical adapter driver."""
import argparse
import json
from pathlib import Path

import nibabel as nib
import numpy as np
from nibabel.cifti2 import cifti2_axes as axes


def brain(root, side, reorder=False):
    info = json.loads((root / (side + '.json')).read_text())
    parts = {}
    for hemi in ['L', 'R']:
        item = info[hemi]
        indices = np.asarray(item['indices'], dtype=int)
        if reorder:
            indices = indices[::-1]
        parts[hemi] = axes.BrainModelAxis.from_surface(
            indices, item['n_vertices'],
            'CortexLeft' if hemi == 'L' else 'CortexRight')
    mask = np.ones((2, 1, 1), dtype=bool)
    affine = np.diag([2., 3., 4., 1.])
    volume = axes.BrainModelAxis.from_mask(mask, affine=affine, name='ThalamusLeft')
    return (parts['L'] + volume[::-1] + parts['R'] if reorder
            else parts['R'] + volume + parts['L'])


def maps(kind):
    metadata = [{'note': 'source metadata & preservation'}] * 2
    names = ['first & map', 'second map']
    if kind == 'scalar':
        return axes.ScalarAxis(names, metadata)
    tables = [{0: ('missing', (0, 0, 0, 0)),
               -3: ('negative', (1, .25, 0, .5)),
               key: ('positive', (0, 0, 1, 1))} for key in [2, 7]]
    return axes.LabelAxis(names, tables, metadata)


def generate(root):
    for kind in ['scalar', 'label']:
        for axis in [0, 1]:
            for side in ['source', 'target']:
                model = brain(root, side, reorder=side == 'target')
                if side == 'source':
                    values = np.fromfile(root / (kind + '-source.bin'), '<f8')
                    values = values.reshape((len(model), 2), order='F')
                else:
                    values = np.zeros((len(model), 2))
                order = (model, maps(kind)) if axis == 0 else (maps(kind), model)
                image = nib.Cifti2Image(values if axis == 0 else values.T,
                                      header=nib.Cifti2Header.from_axes(order))
                image.nifti_header.set_intent(
                    'ConnDenseScalar' if kind == 'scalar' else 'ConnDenseLabel')
                image.nifti_header.extensions.append(
                    nib.nifti1.Nifti1Extension(6, b'adapter fixture extension'))
                nib.save(image, root / f'{side}-{kind}-{axis}.nii')


def verify(root):
    results = []
    for kind in ['scalar', 'label']:
        for axis in [0, 1]:
            source = nib.load(root / f'source-{kind}-{axis}.nii')
            target = nib.load(root / f'target-{kind}-{axis}.nii')
            result = nib.load(root / f'result-{kind}-{axis}.nii')
            expected = np.fromfile(root / f'expected-{kind}-{axis}.bin', '<f8')
            expected = expected.reshape((len(target.header.get_axis(axis)), 2), order='F')
            values = np.asarray(result.dataobj)
            if axis == 1:
                values = values.T
            assert np.array_equal(values, expected, equal_nan=True)
            assert result.header.get_axis(axis) == target.header.get_axis(axis)
            a = source.header.get_axis(1 - axis)
            b = result.header.get_axis(1 - axis)
            assert np.array_equal(a.name, b.name)
            if kind == 'label':
                assert np.array_equal(a.label, b.label)
            for before, after in zip(a.meta, b.meta):
                assert all(after.get(k) == v for k, v in before.items())
            extras = lambda x: [e.get_content() for e in x.nifti_header.extensions
                                if e.get_code() != 32]
            assert extras(source) == extras(result)
            results.append({'kind': kind, 'brain_axis': axis,
                            'shape': list(values.shape), 'max_abs_error': 0})
    receipt = {'status': 'PASS', 'nibabel_version': nib.__version__,
               'numpy_version': np.__version__, 'cases': results}
    (root / 'adapter-format-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print('PASS: cortical outputs, axes, map metadata, labels and extensions.')


parser = argparse.ArgumentParser()
parser.add_argument('action', choices=['generate', 'verify'])
parser.add_argument('directory', type=Path)
args = parser.parse_args()
generate(args.directory) if args.action == 'generate' else verify(args.directory)

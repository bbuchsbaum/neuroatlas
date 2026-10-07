"""Synthetic controls for orientation, crops and overlay interpretation."""
import importlib.util
from pathlib import Path
import unittest
import numpy as np

spec = importlib.util.spec_from_file_location(
    "visual", Path(__file__).with_name("visual-clarity.py"))
visual = importlib.util.module_from_spec(spec)
spec.loader.exec_module(visual)


class VisualControls(unittest.TestCase):
    def test_ras_axial_left_and_anterior(self):
        x, y, z = np.indices((5, 7, 9))
        values = x + 10 * y + 100 * z
        affine = np.diag([2., 2., 2., 1.])
        affine[:3, 3] = [-4, -6, -8]
        image = visual.plane(values, affine, 2, 0, (-2, 2), (-4, 4))
        np.testing.assert_array_equal(image, values[1:4, 1:6, 4].T)
        self.assertGreater(image[0, -1], image[0, 0])
        self.assertGreater(image[-1, 0], image[0, 0])

    def test_sagittal_posterior_to_anterior(self):
        values = np.arange(5 * 7 * 9).reshape(5, 7, 9)
        result = visual.plane(values, np.eye(4), 0, 2, (1, 5), (2, 7))
        np.testing.assert_array_equal(result, values[2, 1:6, 2:8].T)

    def test_equal_intensity_is_gray(self):
        values = np.array([[0., .4, 1.]])
        result = visual.overlay(values, values)
        for channel in range(3):
            np.testing.assert_array_equal(result[:, :, channel], values)

    def test_target_magenta_source_green(self):
        np.testing.assert_array_equal(
            visual.overlay(np.ones((1, 1)), np.zeros((1, 1))), [[[1, 0, 1]]])
        np.testing.assert_array_equal(
            visual.overlay(np.zeros((1, 1)), np.ones((1, 1))), [[[0, 1, 0]]])


if __name__ == "__main__":
    unittest.main()

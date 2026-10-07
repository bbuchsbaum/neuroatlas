"""Analytic controls for reporting metrics, independent of atlas fixtures."""
import importlib.util
from pathlib import Path
import unittest
import numpy as np


def load(name):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


volume = load("volume-quality")
surface = load("surface-quality")


class Metrics(unittest.TestCase):
    def test_identity(self):
        a = np.zeros((7, 8, 9), bool)
        a[2:5, 2:5, 2:5] = True
        r = volume.overlap(a, a, [3, 2, 1])
        self.assertEqual(r["dice"], 1)
        self.assertEqual(r["hd95_mm"], 0)
        self.assertEqual(r["centroid_mm"], 0)

    def test_physical_distance(self):
        a = np.zeros((5, 5, 5), bool)
        b = a.copy()
        a[1, 2, 3], b[2, 2, 3] = True, True
        r = volume.overlap(a, b, [3, 2, 1])
        self.assertEqual(r["dice"], 0)
        self.assertEqual(r["mean_boundary_mm"], 3)
        self.assertEqual(r["hd95_mm"], 3)
        self.assertEqual(r["centroid_mm"], 3)

    def test_missing_region_is_retained(self):
        a = np.ones((2, 2, 2), bool)
        r = volume.overlap(a, ~a, [1, 1, 1])
        self.assertEqual(r["dice"], 0)
        self.assertEqual(r["volume_bias_percent"], -100)
        self.assertIsNone(r["hd95_mm"])

    def test_surface_mask_missing_and_area(self):
        xyz = np.array([[1, 0, 0], [0, 1, 0], [0, 0, 1], [-1, 0, 0]], float)
        faces = np.array([[0, 1, 2], [1, 2, 3]])
        info, rows = surface.compare(np.array([1, np.nan, 2, 9]),
            np.array([1, 1, 2, 2]), np.array([1, 1, 1, 0], bool),
            np.array([1, 2, 3, 4.]), xyz, faces, "toy", {"1": "a", "2": "b"})
        self.assertEqual(info["disagreements"], 1)
        self.assertAlmostEqual(info["area_disagreement_percent"], 100 / 3)
        self.assertEqual([r["key"] for r in rows], [1, 2])
        self.assertAlmostEqual(rows[0]["dice"], 2 / 3)
        self.assertAlmostEqual(rows[0]["area_dice"], .5)

    def test_surface_absent_label(self):
        xyz = np.eye(3)
        info, rows = surface.compare(np.array([1, 1, 1]), np.array([1, 2, 2]),
            np.ones(3, bool), np.ones(3), xyz, np.array([[0, 1, 2]]), "toy", {})
        self.assertEqual(info["lost_labels"], [2])
        self.assertEqual(rows[1]["dice"], 0)


if __name__ == "__main__":
    unittest.main()

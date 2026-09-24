#!/usr/bin/env bash
set -euo pipefail
printf '%s  %s\n' '5e11d0200645fc6b9a766501dd974a35fea9073a1c27d6d08bde43ca20567a66' 'calibrate.R' | sha256sum --check --status
printf '%s  %s\n' '44c892b4e2837bb235312bc98b59ce96ac505d2ef09cb0fafaf97e8a7f8bdb90' 'scripts/common.R' | sha256sum --check --status
printf '%s  %s\n' '372e91088fcbe9c671871a527b0b7af02d3279b46958b6712bc2f29dba457c78' 'scripts/measure-qualification.R' | sha256sum --check --status
printf '%s  %s\n' '6c38e02623e5bc5bd02d2ce958b513e6c7a6cf2111cc66e3d3fd9c22600bdd7d' 'routes.json' | sha256sum --check --status
exec Rscript calibrate.R

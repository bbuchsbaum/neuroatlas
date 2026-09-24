#!/usr/bin/env bash
set -euo pipefail
: "${RS_PARAMS_JSON:?campaign wrapper did not provide RS_PARAMS_JSON}"
: "${RS_OUTPUTS_JSON:?campaign wrapper did not provide RS_OUTPUTS_JSON}"
artifact_root="${NEUROATLAS_TRANSFORM_ARTIFACT_ROOT:-$PWD}"
verify_source() {
  local path="$1" expected="$2" actual
  [[ -f "$path" ]] || { echo "missing build source: $path" >&2; exit 66; }
  actual="$(sha256sum "$path" | awk '{print $1}')"
  [[ "$actual" == "$expected" ]] || { echo "source digest mismatch: $path" >&2; exit 65; }
}
verify_source "${artifact_root}/build.R" "2f3a6f3f3fcd2f7831f3443b1be1213cad04b42424cd2aba9a225eb30fd10fbd"
verify_source "${artifact_root}/scripts/common.R" "44c892b4e2837bb235312bc98b59ce96ac505d2ef09cb0fafaf97e8a7f8bdb90"
verify_source "${artifact_root}/routes.json" "6c38e02623e5bc5bd02d2ce958b513e6c7a6cf2111cc66e3d3fd9c22600bdd7d"
exec Rscript "${artifact_root}/build.R"

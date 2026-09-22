#!/usr/bin/env bash
set -euo pipefail

: "${RS_PARAMS_JSON:?campaign wrapper did not provide RS_PARAMS_JSON}"
: "${RS_OUTPUTS_JSON:?campaign wrapper did not provide RS_OUTPUTS_JSON}"
artifact_root="${NEUROATLAS_TRANSFORM_ARTIFACT_ROOT:-$PWD}"
readonly expected_smoke_sha256="47cf2716f25ccc305e05ca96f5414cf9c380d34116ac5d50dbb48b833943fcbd"
readonly expected_common_sha256="aa735b195cc201addabd68472c0716b44d4c252c1e62c6b7dc5acf0dffde912e"

verify_source() {
  local path="$1"
  local expected="$2"
  if [[ ! -f "$path" ]]; then
    echo "missing synchronized source: $path" >&2
    exit 66
  fi
  local actual
  actual="$(sha256sum "$path" | awk '{print $1}')"
  if [[ "$actual" != "$expected" ]]; then
    echo "source digest mismatch for $path" >&2
    exit 65
  fi
}

verify_source "${artifact_root}/smoke.R" "$expected_smoke_sha256"
verify_source "${artifact_root}/scripts/common.R" "$expected_common_sha256"
exec Rscript "${artifact_root}/smoke.R"

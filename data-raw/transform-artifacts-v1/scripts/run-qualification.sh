#!/usr/bin/env bash
set -euo pipefail

: "${RS_PARAMS_JSON:?campaign wrapper did not provide RS_PARAMS_JSON}"
: "${RS_OUTPUTS_JSON:?campaign wrapper did not provide RS_OUTPUTS_JSON}"
artifact_root="${NEUROATLAS_TRANSFORM_ARTIFACT_ROOT:-$PWD}"
if [[ ! -f "${artifact_root}/qualify.R" ]]; then
  echo "missing qualify.R under campaign artifact root: ${artifact_root}" >&2
  exit 66
fi
exec Rscript "${artifact_root}/qualify.R"

#!/usr/bin/env bash
set -euo pipefail
artifact_root="${NEUROATLAS_TRANSFORM_ARTIFACT_ROOT:-$PWD}"
cd "$artifact_root"
printf '%s  %s\n' '12f59b8a432f7fa07dd38c890c5b6cbca95815fdb2edf83e2e2f99024eba82e2' 'qualify.R' | sha256sum --check --status
printf '%s  %s\n' '44c892b4e2837bb235312bc98b59ce96ac505d2ef09cb0fafaf97e8a7f8bdb90' 'scripts/common.R' | sha256sum --check --status
printf '%s  %s\n' '372e91088fcbe9c671871a527b0b7af02d3279b46958b6712bc2f29dba457c78' 'scripts/measure-qualification.R' | sha256sum --check --status
printf '%s  %s\n' '514983d20888cd9dd7c1ca0ce17b538c974cd69721dbbdea23cd93b15be229cc' 'scripts/qualification-gates.R' | sha256sum --check --status
printf '%s  %s\n' '9d6796a802aeb252d68d68950ac156002560ca60c965b04c9e637849ad231819' 'scripts/visual-qa.R' | sha256sum --check --status
printf '%s  %s\n' '6c38e02623e5bc5bd02d2ce958b513e6c7a6cf2111cc66e3d3fd9c22600bdd7d' 'routes.json' | sha256sum --check --status
printf '%s  %s\n' 'b53ccca3dd5db77016150cdcc9458385f390821e85641918df0d9976dbdc912b' 'qualification-policy.json' | sha256sum --check --status
printf '%s  %s\n' '85f62ab5dbd8a396f317cc9129f09de9ec0a5ef0bbf8ce10cf7fd1bdf79c9ef0' 'landmarks-v1/manifest.json' | sha256sum --check --status
printf '%s  %s\n' '80d01d4bbe601a4a30310fe0db1f0ec4675598cd341c260fc43b944070184e17' 'landmarks-v1/review.json' | sha256sum --check --status
printf '%s  %s\n' '716060bbc931845e6f8ff820854acaf9f9375dd64516c1d35f4a9e81cedd265a' 'landmarks-v1/source_1.csv' | sha256sum --check --status
printf '%s  %s\n' 'fe08fca197d3b3eccbc4b2c950c15c69d582505d9e001726e57b56b4c80c81ad' 'landmarks-v1/source_2.csv' | sha256sum --check --status
printf '%s  %s\n' '26e50c7ae48f59acebbac7f9824964770f4ce0c850406a0b2c8da61b8ae752af' 'landmarks-v1/target_1.csv' | sha256sum --check --status
printf '%s  %s\n' '49832696cf50ddc922fd82ac585f1e7d9ba57eb82a36ef76e4d139108663b826' 'landmarks-v1/target_2.csv' | sha256sum --check --status
exec Rscript qualify.R

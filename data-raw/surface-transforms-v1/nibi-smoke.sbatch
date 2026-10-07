#!/bin/bash
#SBATCH --job-name=neuroatlas-surface-smoke
#SBATCH --account=def-brad_cpu
#SBATCH --partition=cpubase_bycore_b1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=00:15:00
# Submit from a verified, immutable bundle directory. No downloads in this job.
set -euo pipefail
module --force purge
module load StdEnv/2023 connectomeworkbench/2.0.1
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export QT_QPA_PLATFORM=offscreen
bundle_dir="${SLURM_SUBMIT_DIR:?Submit from the bundle directory}"
attempt_dir="${bundle_dir}/attempts/${SLURM_JOB_ID:?Must run under Slurm}"
mkdir -p "${bundle_dir}/attempts"
test ! -e "${attempt_dir}"
cd "${bundle_dir}"
sha256sum -c SHA256SUMS
python3 fetch-inputs.py inputs --lock inputs.lock.json --offline
environment_file="${bundle_dir}/attempts/${SLURM_JOB_ID}-environment.txt"
(
  date -u
  hostname
  module list
  command -v wb_command
  wb_command -version
  sha256sum "$(command -v wb_command)"
  ldd "$(command -v wb_command)"
  python3 --version
) > "${environment_file}" 2>&1
python3 workbench-smoke.py "${bundle_dir}" "${attempt_dir}"

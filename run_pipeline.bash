#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J enut-i-pipeline
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 10
#SBATCH --mem-per-cpu=3000
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 1-0:0:0
#SBATCH -o enut-i/pipeline_%A_%a.err.out
#SBATCH -e enut-i/pipeline_%A_%a.err.out

# Full ENUT-I pipeline (steps.md, steps 2 to 4). The expenditure models
# (step 1, run_expenditures.bash) do not need to be rerun.
# The twin matrix is always rebuilt (the existing one predates the current
# twin covariates and the pre weekend file). To reuse the existing matrix
# instead: sbatch --export=ALL,SKIP_TWINS=1 enut-i/run_pipeline.bash
set -e
cd enut-i

# ---------------- Step 2: pre weekend file ----------------
ml intel/2022.00  # rlang in the user library links libimf.so (Intel runtime)
module load r/4.4.0
Rscript data_processing/data_processing.R --pre

# ---------------- Step 3: twin matrix ----------------
if [ "${SKIP_TWINS:-0}" != "1" ] || [ ! -f data/raw/matriz_gemelos.csv.gzip ]; then
  ml purge
  ml intel/2022.00
  ml Python/3.12.3
  source enut-env/bin/activate
  export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
  TWIN_WORKERS=${SLURM_CPUS_PER_TASK:-10} python data_processing/gemelos_matriz.py
  deactivate
  ml purge
  ml intel/2022.00
  module load r/4.4.0
fi

# ---------------- Step 4: rest of the pipeline ----------------
Rscript data_processing/data_processing.R

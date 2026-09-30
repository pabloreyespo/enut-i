#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J enut-i-pipeline
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 20
#SBATCH --mem-per-cpu=3000
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 1-0:0:0
#SBATCH -o enut-i/pipeline_%A_%a.err.out
#SBATCH -e enut-i/pipeline_%A_%a.err.out

# Full ENUT-I pipeline (steps.md, steps 2 to 4). The expenditure models
# (step 1, run_expenditures.bash) do not need to be rerun.
# The shared structure fixes keep the pre weekend rows and the twin
# covariates, so the existing twin matrix is reused. Submit with
#   sbatch --export=ALL,RUN_TWINS=1 enut-i/run_pipeline.bash
# to rebuild it anyway (or when data/raw/matriz_gemelos.csv.gzip is missing).
set -e
cd enut-i

# ---------------- Step 2: pre weekend file ----------------
module load r/4.4.0
Rscript data_processing/data_processing.R --pre

# ---------------- Step 3: twin matrix (optional) ----------------
if [ "${RUN_TWINS:-0}" = "1" ] || [ ! -f data/raw/matriz_gemelos.csv.gzip ]; then
  ml purge
  ml intel/2022.00
  ml Python/3.12.3
  source enut-env/bin/activate
  TWIN_WORKERS=${SLURM_CPUS_PER_TASK:-20} python data_processing/gemelos_matriz.py
  deactivate
  ml purge
  module load r/4.4.0
fi

# ---------------- Step 4: rest of the pipeline ----------------
Rscript data_processing/data_processing.R

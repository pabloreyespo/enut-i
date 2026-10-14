#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J twins
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 10
#SBATCH --mem-per-cpu=3000
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 1-0:0:0
#SBATCH -o enut-i/twins_%A_%a.err.out
#SBATCH -e enut-i/twins_%A_%a.err.out

#-----------------Toolchain---------------------------
ml purge
ml intel/2022.00
# ----------------Modulos----------------------------
ml Python/3.12.3
# ----------------Comando--------------------------

cd enut-i
source enut-env/bin/activate
# one BLAS thread per worker, one worker per allocated core
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
TWIN_WORKERS=${SLURM_CPUS_PER_TASK:-10} python data_processing/gemelos_matriz.py

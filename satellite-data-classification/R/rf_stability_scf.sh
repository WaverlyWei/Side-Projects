#!/bin/bash
#SBATCH --job-name=rf_stability_scf
#SBATCH -o rf_stability_scf.out #File to which standard out will be written
#SBATCH -e rf_stability_scf.err #File to which standard err will be written
export OMP_NUM_THREADS=1

R CMD BATCH --no-save rf_stability_scf.R rf_stability_scf.Rout
#!/bin/bash
#SBATCH --job-name=fit_all_features_scf
#SBATCH -o fit_all_features_scf.out #File to which standard out will be written
#SBATCH -e fit_all_features_scf.err #File to which standard err will be written
export OMP_NUM_THREADS=1

R CMD BATCH --no-save fit_all_features_scf.R fit_all_features_scf.Rout
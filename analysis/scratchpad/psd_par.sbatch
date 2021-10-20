#!/bin/bash

#SBATCH --job-name=psd_par_128
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --time=8:00:00
#SBATCH --mail-type=ALL

# --ntasks-per-node will be used in doParallel_test.R to specify the number
# of cores to use on the machine.

# load modules
ml R
ml gcc

# execute script
Rscript analysis/scratchpad/psd_par.R 128

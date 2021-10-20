#!/bin/bash

#SBATCH --job-name=bcg_s2n
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --time=4:00:00
#SBATCH --mail-type=ALL

# --ntasks-per-node will be used in doParallel_test.R to specify the number
# of cores to use on the machine.

# load modules
ml R
ml gcc

# execute script
Rscript analysis/scratchpad/bcg_s2n.R

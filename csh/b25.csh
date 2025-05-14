#!/bin/tcsh
#BSUB -W 60:00
#BSUB -n 32
#BSUB -o out.%J
#BSUB -e err.%J
#BSUB -R "span[hosts=1]"
#BSUB -q shared_memory
#BSUB -R "rusage[mem=216]"
conda activate /usr/local/usrapps/snb2023/vcastel/test_env
cd /share/snb2023/vcastel/data
Rscript 5_bootstrap_correlations_25.R
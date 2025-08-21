#!/bin/bash
#SBATCH --job-name=lisem_batch
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=1
#SBATCH --time=12:00:00
#SBATCH --mem=8G
#SBATCH --output=lisem_%A_%a.out
#SBATCH --error=lisem_%A_%a.err
#SBATCH --array=1-50

# Load any required modules (uncomment and edit if needed)
# module load Lisem

# Run the LISEM model for each run file
runfile=$(printf "myrun%02d.run" ${SLURM_ARRAY_TASK_ID})

echo "Running $runfile on $SLURM_CPUS_ON_NODE CPUs"

Lisem -ni -r $runfile
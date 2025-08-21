#!/bin/bash
#SBATCH --job-name=lisem_batch
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=00:01:00
#SBATCH --mem=2G
#SBATCH --output=lisem_%A_%a.out
#SBATCH --error=lisem_%A_%a.err
#SBATCH --array=1

# Load any required modules (uncomment and edit if needed)
module load apptainer

# Run the LISEM model for each run file
singularity exec ./lustre/nobackup/WUR/ESG/comme002/openlisem /opt/openlisem/build/Lisem -ni -r /lustre/nobackup/WUR/ESG/comme002/OL_test/Watervalderbeek_20m/runfiles/20230622.run
#runfile=$(printf "myrun%02d.run" ${SLURM_ARRAY_TASK_ID})

#echo "Running $runfile on $SLURM_CPUS_ON_NODE CPUs"

#Lisem -ni -r $runfile
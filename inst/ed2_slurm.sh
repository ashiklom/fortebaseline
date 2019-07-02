#!/bin/sh
#SBATCH --account br19_shik544
#SBATCH --time 02-00:00
#SBATCH --nodes 1

#SBATCH --job-name=forte_ed2
#SBATCH --output=/people/shik544/data/forte-ed-runs/logs/%A_%a.log
#SBATCH --error=/people/shik544/data/forte-ed-runs/logs/%A_%a.log
#SBATCH --array=1-800

ED2_DIRECTORY=$(find /people/shik544/data/forte-ed-runs/cases -mindepth 1 -maxdepth 1 | sort | sed "${SLURM_ARRAY_TASK_ID}q;d")
ED2IN="${ED2_DIRECTORY}/ED2IN"

if [ -f "$ED2IN" ]; then
    echo "Running ED2 with ED2IN file $ED2IN"
    OMP_NUM_THREADS=1 /people/shik544/bin/ed2 -f "$ED2IN"
else
    echo "File $ED2IN not found." 1>&2
    exit 1
fi

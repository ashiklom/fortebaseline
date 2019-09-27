#!/usr/bin/env bash

# If I can load modules, assume I'm on PIC and load ED2 modules
if [[ $(command -v module) ]]; then
   module purge
   module load gcc/5.2.0 mvapich2/2.1
fi

# First argument is the name of the ED2 executable. Second argument is the path
# to the ED2IN file. Optional third argument is any additional arguments to ED2.
echo "Running ED2..."
"$1" -f "$2" $3 || ( echo "ED2 run failed" && exit 1 )

# For post-processing, load default modules
if [[ $(command -v module) ]]; then
   module purge
   while read mod; do
       module load "$mod"
   done < "${HOME}/.modules"
fi

echo "Post-processing output..."
# Now, post-process the output. `overwrite = TRUE` here because, if I'm
# re-running ED2, I almost definitely want to reprocess the output.
OUTDIR=$(dirname "$2")
Rscript -e "fortebaseline::read_efile_dir(\"${OUTDIR}\", overwrite = TRUE)"
echo "Done!"

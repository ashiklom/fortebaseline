#!/usr/bin/env bash
set -o errexit

ROOTDIR="./"
if [[ $(pwd) =~ "analysis/scripts" ]]; then
    ROOTDIR=../../
fi
DATADIR="$ROOTDIR/data-raw"
if [[ ! -d "$DATADIR" ]]; then
    echo "Data directory $DATADIR not found."
    exit 1
fi

WORKFLOWS_FILE=$(tail -n +2 "$DATADIR/current-workflows.csv")
WORKFLOWS=($(echo "$WORKFLOWS_FILE" | cut -f 1 -d ","))
LASTWF=${WORKFLOWS[*]: -1}
SYNCPATH='pecan:/public/shared-docker-volumes/pecan_data/workflows/PEcAn_{'
for w in ${WORKFLOWS[@]}; do
    SYNCPATH+=$w
    if [[ "$w" == "$LASTWF" ]]; then
        SYNCPATH+="}"
    else
        SYNCPATH+=","
    fi
done

echo "$SYNCPATH"

TARGETDIR="$ROOTDIR"/analysis/data/model_output/workflows/
echo "Creating directory " "$TARGETDIR"
mkdir -p "$TARGETDIR"

echo "Downloading workflows..."

FLAGS="--info=progress2"
if [[ $(rsync --version | head -n1) =~ "version [012]" ]]; then
    FLAGS=""
fi

rsync -az $FLAGS $SYNCPATH $TARGETDIR
echo "Done!"
exit 0

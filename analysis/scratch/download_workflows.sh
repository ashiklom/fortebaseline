#!/bin/bash

mkdir -p analysis/data/model_output/workflows/
rsync -avz --progress pecan:"/public/shared-docker-volumes/pecan_data/workflows/PEcAn_99000000{112,113,114,115,116,117,118,119}" analysis/data/model_output/workflows/ 

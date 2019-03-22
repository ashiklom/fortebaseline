#!/bin/bash
set -e

# Run drake to compile everything
./analysis/drake.R

# Copy the output
cd analysis/paper
cp -rf paper.md paper_files _rendered_output/

# Force-push the results up to GitHub
cd _rendered_output
git checkout rendered
git add .
git commit --amend -m "Rendered output"
git push --force

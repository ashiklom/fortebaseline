#!/usr/bin/env bash
set -o errexit

PROJDIR=$(git rev-parse --show-toplevel)
ORIGIN=$(git remote get-url origin)
DEPLOY_DIR=analysis/paper/_deploy
HASH=$(git rev-parse --short HEAD)

cd "$PROJDIR"

if [[ ! -d "$DEPLOY_DIR" ]]; then
    if [[ -n $(git ls-remote --heads "$ORIGIN" gh-pages) ]]; then
        git clone --single-branch --branch gh-pages "$ORIGIN" "$DEPLOY_DIR"
    else
        mkdir -p "$DEPLOY_DIR"
        cd "$DEPLOY_DIR"
        git init
        git remote add origin "$ORIGIN"
        git checkout -b gh-pages
        cd "$PROJDIR"
    fi
fi

cp -Rf analysis/paper/paper{.html,_files} "$DEPLOY_DIR"
cd "$DEPLOY_DIR"

if [[ ! -f index.html ]]; then
    ln -sf paper.html index.html
fi

if [[ -n $(git status --short) ]]; then
    git add .
    MSG="Update paper <$HASH>"
    git commit -a -m "$MSG"
    git push origin gh-pages
else
    echo "Nothing to commit."
fi

#!/usr/bin/bash

EPS3_BASE="/data/eps3/test_problem"
JOBFILE="pream.area.job"
SHA=sha256:a224c6fe95e34b3cb5952add529e7c7418c5bd67657fc0c1e0ad01493da8734a 

cd "$EPS3_BASE" || exit 1

podman run --rm \
  -v "${EPS3_BASE}:${EPS3_BASE}" \
  -w "${EPS3_BASE}/jobs" \
  us-west1-docker.pkg.dev/aqpg-modeling-prod/eps3/eps3@${SHA} \
  /bin/bash -c "./${JOBFILE}"

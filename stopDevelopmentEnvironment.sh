#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
pushd $DIR >/dev/null
docker-compose -p education-dev -f src/dev/docker/docker-compose.yml down --remove-orphans
docker stop wenet_interaction-protocol-engine_dev
docker rm wenet_interaction-protocol-engine_dev
popd >/dev/null
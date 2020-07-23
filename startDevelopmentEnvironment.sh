#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
pushd $DIR >/dev/null
docker-compose -p education-dev -f src/dev/docker/docker-compose.yml up --remove-orphans -d
DOCKER_BUILDKIT=1 docker build -f src/dev/docker/Dockerfile -t wenet/interaction-protocol-engine:dev .
docker run --name wenet_interaction-protocol-engine_dev -v /var/run/docker.sock:/var/run/docker.sock -v ${HOME}/.m2/repository:/root/.m2/repository  -v ${PWD}:/app -it wenet/interaction-protocol-engine:dev /bin/bash
popd >/dev/null
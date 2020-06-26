#!/bin/bash
set -e
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
pushd $DIR
COMPONENT_VERSION=$(grep --max-count=1 '<version>' pom.xml | awk -F '>' '{ print $2 }' | awk -F '<' '{ print $1 }')
COMPONENT_NAME="wenet/interaction-protocol-engine"
DOCKER_TAG="$COMPONENT_NAME:$COMPONENT_VERSION"
DOCKER_BUILDKIT=1 docker build -f src/main/docker/Dockerfile -t $DOCKER_TAG .
popd
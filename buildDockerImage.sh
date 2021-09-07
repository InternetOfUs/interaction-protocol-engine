#!/bin/bash
if ! docker stats --no-stream >/dev/null 2>&1; then
    echo "Docker does not seem to be running, run it first and retry"
    exit 1
else
	DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
	pushd "$DIR" >/dev/null
	COMPONENT_VERSION=$(grep --max-count=1 '<version>' pom.xml | awk -F '>' '{ print $2 }' | awk -F '<' '{ print $1 }')
	COMPONENT_NAME="internetofus/interaction-protocol-engine"
	DOCKER_TAG="$COMPONENT_NAME:$COMPONENT_VERSION"
	DOCKER_BUILDKIT=1 docker build -f src/main/docker/Dockerfile -t $DOCKER_TAG .
	popd >/dev/null
fi
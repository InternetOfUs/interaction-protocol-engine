#!/bin/bash
if [ -f /.dockerenv ]; then
   echo "You can not start the development environment inside a docker container"
else
	DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
	pushd "$DIR" >/dev/null
	DOCKER_BUILDKIT=1 docker build -f src/dev/docker/Dockerfile -t internetofus/interaction-protocol-engine:dev .
	docker run --rm --name wenet_interaction_protocol_engine_dev -v /var/run/docker.sock:/var/run/docker.sock -v "${HOME}/.m2/repository":/root/.m2/repository -p 5003:5005 -v "${PWD}":/app -it internetofus/interaction-protocol-engine:dev /bin/bash
	./stopDevelopmentEnvironment.sh
	popd >/dev/null
fi
#!/bin/bash
if [ -f /.dockerenv ]; then
   echo "You can not stop the development environment inside a docker container"
else
	DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
	pushd $DIR >/dev/null
	docker-compose -p wenet_interaction_protocol_engine_services_dev -f src/dev/docker/docker-compose.yml down --remove-orphans
	if [ "$(docker container ls |grep wenet_interaction_protocol_engine_dev |wc -l)" -gt "0" ]
	then
		docker stop wenet_interaction_protocol_engine_dev
	fi
	popd >/dev/null
fi
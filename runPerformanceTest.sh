#!/bin/bash
if [ -f /.dockerenv ]; then
   echo "You can not stop the performance test inside a docker container"
else
	DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
	docker run --name wenet_interaction_protocol_engine_performance_test -i loadimpact/k6:latest run $@ - <$DIR/src/test/k6/performance.js
	docker stop wenet_interaction_protocol_engine_performance_test
	docker rm wenet_interaction_protocol_engine_performance_test
fi
# --- Build the code
FROM maven:3.6.3-jdk-11-slim AS build
LABEL stage=build
ENV MAVEN_OPTS="-XX:+TieredCompilation -XX:TieredStopAtLevel=1"
RUN mkdir -p /build
WORKDIR /build
COPY pom.xml .
RUN mvn dependency:go-offline
COPY src/ /build/src/
RUN mvn -DfinalName=wenet-interaction-protocol-engine -DskipTests install

# --- Run the code
FROM openjdk:11.0.5-jre
ARG DEFAULT_API_HOST=0.0.0.0
ARG DEFAULT_API_PORT=8080
ARG DEFAULT_DB_HOST=localhost
ARG DEFAULT_DB_PORT=27017
ARG DEFAULT_DB_NAME=wenetInteractionProtocolEngineDB
ARG DEFAULT_DB_USER_NAME=wenetInteractionProtocolEngine
ARG DEFAULT_DB_USER_PASSWORD=password
ARG DEFAULT_WENET_PROFILE_MANAGER_API_HOST="wenet.u-hopper.com"
ARG DEFAULT_WENET_PROFILE_MANAGER_API_PORT=443
ARG DEFAULT_WENET_PROFILE_MANAGER_API_SSL="true"
ARG DEFAULT_WENET_PROFILE_MANAGER_API_PATH="/profile_manager"
ARG DEFAULT_WENET_TASK_MANAGER_API_HOST="wenet.u-hopper.com"
ARG DEFAULT_WENET_TASK_MANAGER_API_PORT=443
ARG DEFAULT_WENET_TASK_MANAGER_API_SSL="true"
ARG DEFAULT_WENET_TASK_MANAGER_API_PATH="/task_manager"
ARG DEFAULT_WENET_SERVICE_API_HOST="wenet.u-hopper.com"
ARG DEFAULT_WENET_SERVICE_API_PORT=443
ARG DEFAULT_WENET_SERVICE_API_SSL="true"
ARG DEFAULT_WENET_SERVICE_API_PATH="/service"
ENV API_HOST=${DEFAULT_API_HOST}
ENV API_PORT=${DEFAULT_API_PORT}
ENV DB_HOST=${DEFAULT_DB_HOST}
ENV DB_PORT=${DEFAULT_DB_PORT}
ENV DB_NAME=${DEFAULT_DB_NAME}
ENV DB_USER_NAME=${DEFAULT_DB_USER_NAME}
ENV DB_USER_PASSWORD=${DEFAULT_DB_USER_PASSWORD}
ENV WENET_PROFILE_MANAGER_API_HOST=${DEFAULT_WENET_PROFILE_MANAGER_API_HOST}
ENV WENET_PROFILE_MANAGER_API_PORT=${DEFAULT_WENET_PROFILE_MANAGER_API_PORT}
ENV WENET_PROFILE_MANAGER_API_SSL=${DEFAULT_WENET_PROFILE_MANAGER_API_SSL}
ENV WENET_PROFILE_MANAGER_API_PATH=${DEFAULT_WENET_PROFILE_MANAGER_API_PATH}
ENV WENET_TASK_MANAGER_API_HOST=${DEFAULT_WENET_TASK_MANAGER_API_HOST}
ENV WENET_TASK_MANAGER_API_PORT=${DEFAULT_WENET_TASK_MANAGER_API_PORT}
ENV WENET_TASK_MANAGER_API_SSL=${DEFAULT_WENET_TASK_MANAGER_API_SSL}
ENV WENET_TASK_MANAGER_API_PATH=${DEFAULT_WENET_TASK_MANAGER_API_PATH}
ENV WENET_SERVICE_API_HOST=${DEFAULT_WENET_SERVICE_API_HOST}
ENV WENET_SERVICE_API_PORT=${DEFAULT_WENET_SERVICE_API_PORT}
ENV WENET_SERVICE_API_SSL=${DEFAULT_WENET_SERVICE_API_SSL}
ENV WENET_SERVICE_API_PATH=${DEFAULT_WENET_SERVICE_API_PATH}
ENV RUN_ARGS=""

# Create running dierectories
RUN mkdir -p /usr/wenet/interaction-protocol-engine/lib
RUN mkdir -p /usr/wenet/interaction-protocol-engine/etc
RUN mkdir -p /usr/wenet/interaction-protocol-engine/var/log

# Create configuration for api
RUN echo "{\"api\":{\"host\":\"${DEFAULT_API_HOST}\",\"port\":${DEFAULT_API_PORT}}}" > /usr/wenet/interaction-protocol-engine/etc/api.json

# Create configuration for persistence
RUN echo "{\"persistence\":{\"db_name\":\"${DEFAULT_DB_NAME}\",\"host\":\"${DEFAULT_DB_HOST}\",\"port\":${DEFAULT_DB_PORT},\"username\":\"${DEFAULT_DB_USER_NAME}\",\"password\":\"${DEFAULT_DB_USER_PASSWORD}\"}}" > /usr/wenet/interaction-protocol-engine/etc/persistence.json

# Create configuration for profile manager components
RUN echo "{\"wenetComponents\":{\"profileManager\":{\"host\":\"${DEFAULT_WENET_PROFILE_MANAGER_API_HOST}\",\"port\":${DEFAULT_WENET_PROFILE_MANAGER_API_PORT},\"ssl\":${DEFAULT_WENET_PROFILE_MANAGER_API_SSL},\"apiPath\":\"${DEFAULT_WENET_PROFILE_MANAGER_API_PATH}\"}}}" > /usr/wenet/interaction-protocol-engine/etc/profileManagerApi.json

# Create configuration for task manager component
RUN echo "{\"wenetComponents\":{\"taskManager\":{\"host\":\"${DEFAULT_WENET_TASK_MANAGER_API_HOST}\",\"port\":${DEFAULT_WENET_TASK_MANAGER_API_PORT},\"ssl\":${DEFAULT_WENET_TASK_MANAGER_API_SSL},\"apiPath\":\"${DEFAULT_WENET_TASK_MANAGER_API_PATH}\"}}}" > /usr/wenet/interaction-protocol-engine/etc/taskManager.json

# Create configuration for service component
RUN echo "{\"wenetComponents\":{\"service\":{\"host\":\"${DEFAULT_WENET_SERVICE_API_HOST}\",\"port\":${DEFAULT_WENET_SERVICE_API_PORT},\"ssl\":${DEFAULT_WENET_SERVICE_API_SSL},\"apiPath\":\"${DEFAULT_WENET_SERVICE_API_PATH}\"}}}" > /usr/wenet/interaction-protocol-engine/etc/serviceApi.json

WORKDIR /usr/wenet/interaction-protocol-engine
COPY --from=build /build/target/wenet-interaction-protocol-engine.jar /usr/wenet/interaction-protocol-engine/wenet-interaction-protocol-engine.jar
COPY --from=build /build/target/lib/ /usr/wenet/interaction-protocol-engine/lib/


## Add the wait script to the image
ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.7.3/wait /wait
RUN chmod +x /wait

EXPOSE ${API_PORT}
CMD /wait && java -jar wenet-interaction-protocol-engine.jar -c etc \
  -papi.host=${API_HOST} -papi.port=${API_PORT}\
  -ppersistence.host=${DB_HOST} -ppersistence.port=${DB_PORT} -ppersistence.db_name=${DB_NAME} -ppersistence.username=${DB_USER_NAME} -ppersistence.password=${DB_USER_PASSWORD}\
  -pwenetComponents.profileManager.host=${WENET_PROFILE_MANAGER_API_HOST} -pwenetComponents.profileManager.port=${WENET_PROFILE_MANAGER_API_PORT} -pwenetComponents.profileManager.ssl=${WENET_PROFILE_MANAGER_API_SSL} -pwenetComponents.profileManager.apiPath=${WENET_PROFILE_MANAGER_API_PATH}\
  -pwenetComponents.taskManager.host=${WENET_TASK_MANAGER_API_HOST} -pwenetComponents.taskManager.port=${WENET_TASK_MANAGER_API_PORT} -pwenetComponents.taskManager.ssl=${WENET_TASK_MANAGER_API_SSL} -pwenetComponents.taskManager.apiPath=${WENET_TASK_MANAGER_API_PATH}\
  -pwenetComponents.service.host=${WENET_SERVICE_API_HOST} -pwenetComponents.service.port=${WENET_SERVICE_API_PORT} -pwenetComponents.service.ssl=${WENET_SERVICE_API_SSL} -pwenetComponents.service.apiPath=${WENET_SERVICE_API_PATH}\
  ${RUN_ARGS}

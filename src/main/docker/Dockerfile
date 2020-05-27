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
ARG DEFAULT_WENET_PROFILE_MANAGER_API="https://wenet.u-hopper.com/prod/profile_manager"
ARG DEFAULT_WENET_TASK_MANAGER_API="https://wenet.u-hopper.com/prod/task_manager"
ARG DEFAULT_WENET_SERVICE_API="https://wenet.u-hopper.com/prod/service"
ARG DEFAULT_WENET_INCENTIVE_SERVER_API="https://wenet.u-hopper.com/prod/incentive_server"
ARG DEFAULT_WENET_SOCIAL_CONTEXT_BUILDER_API="https://wenet.u-hopper.com/prod/social_context_builder"
ENV API_HOST=${DEFAULT_API_HOST}
ENV API_PORT=${DEFAULT_API_PORT}
ENV DB_HOST=${DEFAULT_DB_HOST}
ENV DB_PORT=${DEFAULT_DB_PORT}
ENV DB_NAME=${DEFAULT_DB_NAME}
ENV DB_USER_NAME=${DEFAULT_DB_USER_NAME}
ENV DB_USER_PASSWORD=${DEFAULT_DB_USER_PASSWORD}
ENV WENET_PROFILE_MANAGER_API=${DEFAULT_WENET_PROFILE_MANAGER_API}
ENV WENET_TASK_MANAGER_API=${DEFAULT_WENET_TASK_MANAGER_API}
ENV WENET_SERVICE_API=${DEFAULT_WENET_SERVICE_API}
ENV WENET_INCENTIVE_SERVER_API=${DEFAULT_WENET_INCENTIVE_SERVER_API}
ENV WENET_SOCIAL_CONTEXT_BUILDER_API=${DEFAULT_WENET_SOCIAL_CONTEXT_BUILDER_API}
ENV RUN_ARGS=""

# Create running dierectories
RUN mkdir -p /usr/wenet/interaction-protocol-engine/lib
RUN mkdir -p /usr/wenet/interaction-protocol-engine/etc
RUN mkdir -p /usr/wenet/interaction-protocol-engine/var/log

# Create configuration for api
RUN echo "{\"api\":{\"host\":\"${DEFAULT_API_HOST}\",\"port\":${DEFAULT_API_PORT}}}" > /usr/wenet/interaction-protocol-engine/etc/api.json

# Create configuration for persistence
RUN echo "{\"persistence\":{\"db_name\":\"${DEFAULT_DB_NAME}\",\"host\":\"${DEFAULT_DB_HOST}\",\"port\":${DEFAULT_DB_PORT},\"username\":\"${DEFAULT_DB_USER_NAME}\",\"password\":\"${DEFAULT_DB_USER_PASSWORD}\"}}" > /usr/wenet/interaction-protocol-engine/etc/persistence.json

# Create configuration for profile manager component
RUN echo "{\"wenetComponents\":{\"profileManager\":\"${DEFAULT_WENET_TASK_MANAGER_API}\"}}" > /usr/wenet/interaction-protocol-engine/etc/profileManager.json

# Create configuration for task manager component
RUN echo "{\"wenetComponents\":{\"taskManager\":\"${DEFAULT_WENET_TASK_MANAGER_API}\"}}" > /usr/wenet/interaction-protocol-engine/etc/taskManager.json

# Create configuration for service component
RUN echo "{\"wenetComponents\":{\"service\":\"${DEFAULT_WENET_SERVICE_API}\"}}" > /usr/wenet/interaction-protocol-engine/etc/service.json

# Create configuration for incentive component
RUN echo "{\"wenetComponents\":{\"incentiveServer\":\"${DEFAULT_WENET_INCENTIVE_SERVER_API}\"}}" > /usr/wenet/interaction-protocol-engine/etc/incentiveServer.json

# Create configuration for social context builderr component
RUN echo "{\"wenetComponents\":{\"socialContextBuilder\":\"${DEFAULT_WENET_SOCIAL_CONTEXT_BUILDER_API}\"}}" > /usr/wenet/interaction-protocol-engine/etc/socialContextBuilder.json

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
  -pwenetComponents.profileManager=${WENET_PROFILE_MANAGER_API}\
  -pwenetComponents.taskManager=${WENET_TASK_MANAGER_API}\
  -pwenetComponents.service=${WENET_SERVICE_API}\
  -pwenetComponents.incentiveServer=${WENET_INCENTIVE_SERVER_API}\
  -pwenetComponents.socialContextBuilder=${WENET_SOCIAL_CONTEXT_BUILDER_API}\
  ${RUN_ARGS}

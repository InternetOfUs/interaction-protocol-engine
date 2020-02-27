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
ARG DEFAULT_DB_NAME=wenetTaskManagerDB
ARG DEFAULT_DB_USER_NAME=wenetTaskManager
ARG DEFAULT_DB_USER_PASSWORD=password
ARG DEFAULT_WENET_PROFILE_MANAGER_API_HOST="ardid.iiia.csic.es"
ARG DEFAULT_WENET_PROFILE_MANAGER_API_PORT=80
ARG DEFAULT_WENET_PROFILE_MANAGER_API_PATH="/dev-wenet-profile-manager"
ARG DEFAULT_WENET_TASK_MANAGER_API_HOST="ardid.iiia.csic.es"
ARG DEFAULT_WENET_TASK_MANAGER_API_PORT=80
ARG DEFAULT_WENET_TASK_MANAGER_API_PATH="/dev-wenet-task-manager"
ENV API_HOST=${DEFAULT_API_HOST}
ENV API_PORT=${DEFAULT_API_PORT}
ENV DB_HOST=${DEFAULT_DB_HOST}
ENV DB_PORT=${DEFAULT_DB_PORT}
ENV DB_NAME=${DEFAULT_DB_NAME}
ENV DB_USER_NAME=${DEFAULT_DB_USER_NAME}
ENV DB_USER_PASSWORD=${DEFAULT_DB_USER_PASSWORD}
ARG WENET_PROFILE_MANAGER_API_HOST=${DEFAULT_WENET_PROFILE_MANAGER_API_HOST}
ARG WENET_PROFILE_MANAGER_API_PORT=${DEFAULT_WENET_PROFILE_MANAGER_API_PORT}
ARG WENET_PROFILE_MANAGER_API_PATH=${DEFAULT_WENET_PROFILE_MANAGER_API_PATH}
ARG WENET_TASK_MANAGER_API_HOST=${DEFAULT_WENET_TASK_MANAGER_API_HOST}
ARG WENET_TASK_MANAGER_API_PORT=${DEFAULT_WENET_TASK_MANAGER_API_PORT}
ARG WENET_TASK_MANAGER_API_PATH=${DEFAULT_WENET_TASK_MANAGER_API_PATH}
ENV RUN_ARGS=""
ENV RUN_API_PROPERTIES="-papi.host=${API_HOST} -papi.port=${API_PORT}"
ENV RUN_PERSISTENCE_PROPERTIES="-ppersistence.db_name=${DB_NAME} -ppersistence.username=${DB_USER_NAME} -ppersistence.password=${DB_USER_PASSWORD}"
ENV RUN_PROFILE_MANAGER_PROPERTIES="-pservice.profileManager.host=${WENET_PROFILE_MANAGER_API_HOST} -pservice.profileManager.port=${WENET_PROFILE_MANAGER_API_PORT} -pservice.profileManager.apiPath=${WENET_PROFILE_MANAGER_API_PATH}"
ENV RUN_TASK_MANAGER_PROPERTIES="-pservice.taskManager.host=${WENET_TASK_MANAGER_API_HOST} -pservice.taskManager.port=${WENET_TASK_MANAGER_API_PORT} -pservice.taskManager.apiPath=${WENET_TASK_MANAGER_API_PATH}"
ENV RUN_PROPERTIES="${RUN_API_PROPERTIES} ${RUN_PERSISTENCE_PROPERTIES} ${RUN_PROFILE_MANAGER_PROPERTIES} ${RUN_TASK_MANAGER_PROPERTIES}"

# Create running dierectories
RUN mkdir -p /usr/wenet/interaction-protocol-engine/lib
RUN mkdir -p /usr/wenet/interaction-protocol-engine/etc
RUN mkdir -p /usr/wenet/interaction-protocol-engine/var/log

# Create configuration for api
RUN echo "{\"api\":{\"host\":\"${DEFAULT_API_HOST}\",\"port\":${DEFAULT_API_PORT}}}" > /usr/wenet/interaction-protocol-engine/etc/api.json

# Create configuration for persistence
RUN echo "{\"persistence\":{\"db_name\":\"${DEFAULT_DB_NAME}\",\"host\":\"${DEFAULT_DB_HOST}\",\"port\":${DEFAULT_DB_PORT},\"username\":\"${DEFAULT_DB_USER_NAME}\",\"password\":\"${DEFAULT_DB_USER_PASSWORD}\"}}" > /usr/wenet/interaction-protocol-engine/etc/persistence.json

# Create configuration for service
RUN echo "{\"service\":{\"profileManager\":{\"host\":\"${DEFAULT_WENET_PROFILE_MANAGER_API_HOST}\",\"port\":${DEFAULT_WENET_PROFILE_MANAGER_API_PORT},\"apiPath\":\"${DEFAULT_WENET_PROFILE_MANAGER_API_PATH}\"}},{\"taskManager\":{\"host\":\"${DEFAULT_WENET_TASK_MANAGER_API_HOST}\",\"port\":${DEFAULT_WENET_TASK_MANAGER_API_PORT},\"apiPath\":\"${DEFAULT_WENET_TASK_MANAGER_API_PATH}\"}}}" > /usr/wenet/interaction-protocol-engine/etc/service.json

WORKDIR /usr/wenet/interaction-protocol-engine
COPY --from=build /build/target/wenet-interaction-protocol-engine.jar /usr/wenet/interaction-protocol-engine/wenet-interaction-protocol-engine.jar
COPY --from=build /build/target/lib/ /usr/wenet/interaction-protocol-engine/lib/


## Add the wait script to the image
ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.7.3/wait /wait
RUN chmod +x /wait

EXPOSE ${API_PORT}
CMD /wait && java -jar wenet-interaction-protocol-engine.jar -c etc ${RUN_PROPERTIES} ${RUN_ARGS}

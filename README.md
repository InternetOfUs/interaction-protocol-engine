# Internet of us - Interaction protocol engine

## Introduction

The interaction protocol engine component is the one responsible for guaranteeing interactions
between WeNet users to follow the norms.

The interaction between users is modelled as an exchange of messages.
When a user sends a message through the API, the message is sent to the norm interpreter of the user.
This interpreter needs to first verify that the message does not violate any of the norms,
this includes the community norms, the task norms, the sender’s individual norms, as well as the context-dependent
norms that are attached to this message. If the message violates any of those norms,
an error message is sent back to the user. However, if the message obeys the norms, then the norm interpreter needs
to decide what to do next, usually translated into sending messages to other peers. This decision follows from
the community, individual and context-dependent norms, and takes the user’s profile (both public and private)
into account as needed. If the message is sent to the interpreter of another user. As in the previous case,
the norm interpreter of this new user needs to first verify that the message does not violate any of the community norms.
This re-checking upon receipt ensures that the sender’s norm engine has not been manipulated to cheat.
If the message violates any of the community norms, then it may either be discarded or if the community norms
require sanctioning, then the appropriate sanctions should be executed. However, if the action obeys the community norms,
then the norm interpreter needs to decide what to do next, which is usually translated into sending messages
to other peers and/or sending messages to its user. This decision takes into consideration the community norms,
the context-dependent norms that are attached to the message, the individual private norms of the human whose interpreter
has received this message, as well as their local profile (both private and public). This ensures that the interpreter
abides by human’s private norms without leaking any of their private norms and profile.

There are norms on the individual (user level), task level, and community level.
An individual’s norm might be “Suppress incoming messages at night” (and this will
be applied to the user who sets this norm only). A task norm might be “Don’t ask my ex”
(so that would be applied for a specific task only). A community norm might be
“If you don’t volunteer, you cannot ask for help” and it would be enforced on everyone.
Given the above, this means that norms will be attached to users, tasks and communities.


## Setup and configuration

First of all, you must install the next software.

 - [docker](https://docs.docker.com/install/)
 - [docker compose](https://docs.docker.com/compose/install/)

### Requirements

The interaction protocol engine component requires:

 - [MongoDB](https://docs.mongodb.com/manual/installation/)
 - [Profile manager](https://github.com/InternetOfUs/profile-manager)
 - [Interaction protocol engine](https://github.com/InternetOfUs/interaction-protocol-engine/)
 - [Service API](https://github.com/InternetOfUs/service-api/)
 - [Social context builder](https://github.com/InternetOfUs/social-context-builder/)
 - [Incentive server](https://github.com/InternetOfUs/incentive-server/)

### Development

The development is done using a docker image that can be created and started with the script `./startDevelopmentEnvironment.sh`.
The scrip start the next services:

 - [Mongo express](http://localhost:8081)
 - [Swagger editor](http://localhost:8080)
 - [Shish](http://localhost:3050)

And also start a bash console where you can compile and test the project. The project uses the [Apache maven](https://maven.apache.org/)
to solve the dependencies, generate the Open API documentation, compile the component and run the test.

 - Use `mvn dependency:list` to show the component dependencies.
 - Use `mvn compile` to compile and generate the Open API documentation (**target/classes/wenet-interaction_protocol_engine-openapi.yml**).
 - Use `mvn test` to run the test.
 - Use `mvn -Dmaven.surefire.debug="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=0.0.0.0:5005 -Xnoagent -Djava.compiler=NONE" test` to run the test on debug mode.
 - Use `mvn site` to generate a HTML page (**target/site/index.html**) with all the reports (test, javadoc, PMD,CPD and coverage).

Finally, you can stop the development exiting the bash and executing the script `./stopDevelopmentEnvironment.sh`.

### Create docker image

If you want to create an image execute the next command.

```
./buildDockerImage.sh
```

This creates the generic docker image, but you can create a different wit the **docker build** command and using the next arguments:

 - **DEFAULT_API_HOST** to define the default host where API will be bind. By default is **0.0.0.0**.
 - **DEFAULT_API_PORT** to define the default port where API will be bind. By default is **8080**.
 - **DEFAULT_DB_HOST** to define the default mongo database server hostname. By default is **localhost**.
 - **DEFAULT_DB_PORT** to define the default mongo database server port. By default is **27017**.
 - **DEFAULT_DB_NAME** to define the default mongo database name. By default is **wenetInteractionProtocolEngineDB**.
 - **DEFAULT_DB_USER_NAME** to define the default mongo database user name. By default is **wenetInteractionProtocolEngine**.
 - **DEFAULT_DB_USER_PASSWORD** to define the default mongo database user password. By default is **password**.
 - **DEFAULT_WENET_PROFILE_MANAGER_API** to define the path to the profile manager component to use. By default is **https://wenet.u-hopper.com/prod/profile_manager**.
 - **DEFAULT_WENET_TASK_MANAGER_API** to define the path to the task manager component to use. By default is **https://wenet.u-hopper.com/prod/task_manager**.
 - **DEFAULT_WENET_SERVICE_API** to define the path to the service component to use. By default is **https://wenet.u-hopper.com/prod/service**.
 - **DEFAULT_WENET_INCENTIVE_SERVER_APII** to define the path to the service component to use. By default is **https://wenet.u-hopper.com/prod/incentive_server**.
 - **DEFAULT_WENET_SOCIAL_CONTEXT_BUILDER_API** to define the path to the service component to use. By default is **https://wenet.u-hopper.com/prod/social_context_builder**.
 - **DEFAULT_WENET_PERSONAL_CONTEXT_BUILDER_API** to define the path to the service component to use. By default is **https://wenet.u-hopper.com/prod/personal_context_builder**.
 - **DEFAULT_CACHE_TIMEOUT** to define the time in seconds that a value can be on the cache. By default is **300**.
 - **DEFAULT_CACHE_SIZE** to define the maximum number of entries that can be on the cache. By default is **10000**.

Also, you can define your configuration that modifies these properties and mount to  **/usr/wenet/interaction-protocol-engine/etc**.


### Run, configure and link with a MongoDB

You can start this component starting the [latest docker image upload to docker hub](https://hub.docker.com/r/internetofus/interaction-protocol-engine).

```
docker run internetofus/interaction-protocol-engine:latest
```

On this container, you can use the next environment variables:

 - **API_HOST** to define the default host where API will be bind. By default is **0.0.0.0**.
 - **API_PORT** to define the default port where API will be bind. By default is **8080**.
 - **DB_HOST** to define the default mongo database server hostname. By default is **localhost**.
 - **DB_PORT** to define the default mongo database server port. By default is **27017**.
 - **DB_NAME** to define the default mongo database name. By default is **wenetInteractionProtocolEngineDB**.
 - **DB_USER_NAME** to define the default mongo database user name. By default is **wenetInteractionProtocolEngine**.
 - **DB_USER_PASSWORD** to define the default mongo database user password. By default is **password**.
 - **WENET_PROFILE_MANAGER_API** to define the path to the profile manager component to use. By default is **https://wenet.u-hopper.com/prod/profile_manager**.
 - **WENET_TASK_MANAGER_API** to define the path to the task manager component to use. By default is **https://wenet.u-hopper.com/prod/task_manager**.
 - **WENET_SERVICE_API** to define the path to the service component to use. By default is **https://wenet.u-hopper.com/prod/service**.
 - **WENET_INCENTIVE_SERVER_API** to define the path to the incentive server component to use. By default is **https://wenet.u-hopper.com/prod/incentive_server**.
 - **WENET_SOCIAL_CONTEXT_BUILDER_API** to define the path to the social context builder component to use. By default is **https://wenet.u-hopper.com/prod/social_context_builder**.
 - **WENET_PERSONAL_CONTEXT_BUILDER_API** to define the path to the personal context builder component to use. By default is **https://wenet.u-hopper.com/prod/personal_context_builder**.
 - **COMP_AUTH_KEY** to define the authentication key that the component has to use to interact with the other WeNet components.
 - **CACHE_TIMEOUT** to define the time in seconds that a value can be on the cache. By default is **300**.
 - **CACHE_SIZE** to define the maximum number of entries that can be on the cache. By default is **10000**.

When the container is started, it stores the log messages at **/usr/wenet/interaction-protocol-engine/var/log/interaction-protocol-engine.log**. This file is limited
to 10 MB and rolled every day using the pattern **interaction-protocol-engine.log.X** (where X is a number between 1 and 99).

If you want to start also a database and link both you can use the defined docker compose configuration.

```
docker-compose -f src/main/docker/docker-compose.yml up -d
```

This docker compose has the next variables:

 - **INTERACTION_PROTOCOL_ENGINE_API_PORT** to define the port to listen for the API calls. By default is **8083**.
 - **MONGO_ROOT_USER** to define the root user for the MongoDB. By default is **root**.
 - **MONGO_ROOT_PASSWORD** to define the password of the root user for the MongoDB. By default is **password**.
 - **WENET_PROFILE_MANAGER_API** to define the path to the profile manager component to use. By default is **https://wenet.u-hopper.com/prod/profile_manager**.
 - **WENET_SERVICE_API** to define the path to the service component to use. By default is **https://wenet.u-hopper.com/prod/service**.
 - **WENET_INCENTIVE_SERVER_API** to define the path to the incentive server component to use. By default is **https://wenet.u-hopper.com/prod/incentive_server**.
 - **WENET_SOCIAL_CONTEXT_BUILDER_API** to define the path to the social context builder component to use. By default is **https://wenet.u-hopper.com/prod/social_context_builder**.
 - **WENET_PERSONAL_CONTEXT_BUILDER_API** to define the path to the personal context builder component to use. By default is **https://wenet.u-hopper.com/prod/personal_context_builder**.
 - **CACHE_TIMEOUT** to define the time in seconds that a value can be on the cache. By default is **300**.
 - **CACHE_SIZE** to define the maximum number of entries that can be on the cache. By default is **10000**.

### Show running logs

When the container is ready you can access the logs of the component, following the next steps:

 - Discover the identifier of the container of the component (`docker container ls`).
 - Open a shell to the container of the component (`docker exec -it c82f8f4a136c /bin/bash`).
 - The logs are on the directory **/usr/wenet/interaction-protocol-engine/var/log**.

### Run performance test

This component provides a performance test using [K6](https://k6.io/). To run this test use the script `./runPerformanceTest.sh`.
By default, it is run over the development server, if you want to test another server pass the environment property **INTERACTION_PROTOCOL_ENGINE_API**,
and also you can pass any parameter to configure **k6**. For example to run the test over the production one with 10 virtual users
during 30 seconds execute:

```
./runPerformanceTest.sh -e INTERACTION_PROTOCOL_ENGINE_API="https://wenet.u-hopper.com/prod/interaction_protocol_engine" --vus 10 --duration 30s
```

## Documentation

The latest APIs documentation is available [here](http://swagger.u-hopper.com/?url=https://github.com/InternetOfUs/components-documentation/raw/master/sources/wenet-interaction_protocol_engine-openapi.yaml).


## Instances

The interaction protocol engine has the next available instances:

 - WeNet production interaction protocol engine API is available at [https://wenet.u-hopper.com/prod/interaction_protocol_engine](https://wenet.u-hopper.com/prod/interaction_protocol_engine/help/info).
 - WeNet development interaction protocol engine API is available at [https://wenet.u-hopper.com/dev/interaction_protocol_engine](https://wenet.u-hopper.com/dev/interaction_protocol_engine/help/info).
 - The IIIA stable interaction protocol engine API is available at [http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/prod](http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/prod/help/info).
 - The IIIA development interaction protocol engine API is available at [http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/dev](http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/dev/help/info).


## License

This software is under the [Apache V2 license](LICENSE)


## Interaction with other WeNet components

### [Task manager](https://hub.docker.com/r/internetofus/task-manager)

 - Get task status associated to the message to process (GET {{task_manager_api}}/tasks/{{taskId}})
 - Update the task every time a transaction message is process (PUT {{task_manager_api}}/tasks/{{taskId}})


### [Service](https://hub.docker.com/r/internetofus/service-api)

 - Get all the users of an application to rank them and ask for help when a task is created (GET {{service_api}}/app/{{appId}}/users)
 - Get application information associated to the message to process (GET {{service_api}}/app/{{appId}})


### WeNet application

 - Inform about the result of any task transaction message (POST {{messageCallbackUrl}}/)
 - Inform about an incentive (POST {{messageCallbackUrl}}/)


### Social context builder

 - Inform every time a message is sent to an user in a task. (POST {{social_context_builder_api}}/social/notification/})


### Incentive server

 - Inform every time a task transaction success and change the state. (POST {{incentive_server_api}}/Tasks/TaskStatus/})


### Personal context builder

 - Get the locations of a set of users. (POST {{personal_context_builder_api}}/locations)
 - Get the closest users into a location. (GET {{personal_context_builder_api}}/closest)


## Contact

### Researcher

 - [Nardine Osman](http://www.iiia.csic.es/~nardine/) ( [IIIA-CSIC](https://www.iiia.csic.es/~nardine/) ) nardine (at) iiia.csic.es
 - [Carles Sierra](http://www.iiia.csic.es/~sierra/) ( [IIIA-CSIC](https://www.iiia.csic.es/~sierra/) ) sierra (at) iiia.csic.es

### Developers

 - Joan Jené ( [UDT-IA, IIIA-CSIC](https://www.iiia.csic.es/people/person/?person_id=19) ) jjene (at) iiia.csic.es
 - Bruno Rosell i Gui ( [UDT-IA, IIIA-CSIC](https://www.iiia.csic.es/people/person/?person_id=27) ) rosell (at) iiia.csic.es

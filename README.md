# WeNet - Interaction protocol engine

## Introduction

The interaction protocol engine component is the one responsible for guaranteeing that interactions
between WeNet users follow the norms.

The interaction between users is modeled as an exchange of messages.
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
If the message violates any of the community norms, then it may either be discarded, or if the community norms
require sanctioning, then the appropriate sanctions should be executed. However, if the action obeys the community norms,
then the norm interpreter needs to decide what to do next, which is usually translated into sending messages
to other peers and/or sending messages to its user. This decision takes into consideration the community norms,
the context-dependent norms that are attached to the message, the individual private norms of the human whose interpreter
has received this message, as well as their local profile (both private and public). This ensures that the interpreter
abides by human’s private norms without leaking any of their private norms and profile.

There are norms on the individual (user level), the task level, and the community level.
An individual’s norm might be “Suppress incoming messages at night” (and this will
be applied for the user who sets this norm only). A task norm might be “Don’t ask my ex”
(so that would be applied for a specific task only). A community norm might be
“If you don’t volunteer, you cannot ask for help” and it would be enforced on everyone.
Given the above, this means that norms will be attached to users, tasks and communities.


## Setup and configuration

### Installation

The interaction protocol engine component required [Java version 11](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html) or higher.

All required java packages will be automatic installed by the compilation tool (`./mvnw clean install`).

### Requirements

The interaction protocol engine component requires:

 - [MongoDB](https://docs.mongodb.com/manual/installation/)
 - [WeNet - Profile manager](https://bitbucket.org/wenet/profile-manager/)
 - [WeNet - Interaction protocol engine](https://bitbucket.org/wenet/wenet-interaction-protocol-engine/)
 - [WeNet - Service API](https://bitbucket.org/wenet/wenet-service-api/)


### Docker support

To use this feature you must to install the next software.

 - [docker](https://docs.docker.com/install/)
 - [docker compose](https://docs.docker.com/compose/install/)


#### Create docker image

If you want to create an image execute the next command.

```
./buildDockerImage.sh
```

This create the generic docker image, but you can create a different wit the **docker build** command and using the next arguments:

 - **DEFAULT_API_HOST** to define the default host where API will be bind. By default is **0.0.0.0**.
 - **DEFAULT_API_PORT** to define the default port where API will be bind. By default is **8080**.
 - **DEFAULT_DB_HOST** to define the default mongo database server host name. By default is **localhost**.
 - **DEFAULT_DB_PORT** to define the default mongo database server port. By default is **27017**.
 - **DEFAULT_DB_NAME** to define the default mongo database name. By default is **wenetTaskManagerDB**.
 - **DEFAULT_DB_USER_NAME** to define the default mongo database user name. By default is **wenetTaskManager**.
 - **DEFAULT_DB_USER_PASSWORD** to define the default mongo database user password. By default is **password**.
 - **DEFAULT_WENET_PROFILE_MANAGER_API** to define the path to the profile manager component to use. By default is **"https://wenet.u-hopper.com/prod/profile_manager**.
 - **DEFAULT_WENET_TASK_MANAGER_API** to define the path to the task manager component to use. By default is **"https://wenet.u-hopper.com/prod/task_manager**.
 - **DEFAULT_WENET_SERVICE_API** to define the path to the service component to use. By default is **"https://wenet.u-hopper.com/prod/service**.
 - **DEFAULT_WENET_INCENTIVE_SERVER_APII** to define the path to the service component to use. By default is **"https://wenet.u-hopper.com/prod/incentive_server**.
 - **DEFAULT_WENET_SOCIAL_CONTEXT_BUILDER_API** to define the path to the service component to use. By default is **"https://wenet.u-hopper.com/prod/social_context_builder**.

This arguments are used to create a configurations files at **/usr/wenet/interaction-protocol-engine/etc**.
So you can mount a volume to this if you want to modify any configuration property at runtime.

#### Run docker image

To run a the created docker image, run the next command:

```
docker run -t -i -p 8080:8080 --name wenet_interaction_protocol_engine_api wenet/interaction-protocol-engine
```

You can modify use the next environment properties to modify some parameters of the server:

 - **API_HOST** to define the host where the API has to bind. By default is **0.0.0.0**.
 - **API_PORT** to define the port where the API has to bind. By default is **8080**.
 - **DB_HOST** to define the mongo database server host name. By default is **localhost**.
 - **DB_PORT** to define the mongo database server port. By default is **27017**.
 - **DB_NAME** to define the mongo database name. By default is **wenetTaskManagerDB**.
 - **DB_USER_NAME** to define the mongo database user name. By default is **wenetTaskManager**.
 - **DB_USER_PASSWORD** to define the mongo database user password. By default is **password**.
 - **WENET_PROFILE_MANAGER_API** to define the path to the profile manager component to use. By default is **"https://wenet.u-hopper.com/prod/profile_manager**.
 - **WENET_SERVICE_API** to define the path to the service component to use. By default is **"https://wenet.u-hopper.com/prod/service**.
 - **WENET_INCENTIVE_SERVER_API** to define the path to the incentive server component to use. By default is **"https://wenet.u-hopper.com/prod/incentive_server**.
 - **WENET_SOCIAL_CONTEXT_BUILDER_API** to define the path to the social context builder component to use. By default is **"https://wenet.u-hopper.com/prod/social_context_builder**.

Also you can define your own configuration that modify this properties and mount to  **/usr/wenet/interaction-protocol-engine/etc**.

If you want to start also a database and link both you can use the docker compose (`docker-compose -f src/main/docker/docker-compose.yml up -d`):

After that you can interact with the API at **http://localhost:80**. You can modify the listening port
with the next environment properties:

 - **API_PORT** to define the port where the API has to bind to the localhost. By default is **80**.

When the container is ready you can access the logs of the component, following the next steps:

 - Discover the identifier of the container of the component (`docker container ls`).
 - Open a shell to the container of the component (`docker exec -it c82f8f4a136c /bin/bash`).
 - The logs are on the directory **/usr/wenet/interaction-protocol-engine/var/log**.


## Usage

The project use the [Apache maven](https://maven.apache.org/) tool to solve the dependencies,
generate the Open API documentation, compile the component and run the test.

 - Use `./mvnw dependency:list` to show the component dependencies.
 - Use `./mvnw compile` to compile and generate the Open API documentation (**target/classes/wenet-interaction_protocol_engine-openapi.yml**).
 - Use `./mvnw tests` to run the test.
 - Use `./mvnw site` to generate a HTML page (**target/site/index.html**) with all the reports (test, javadoc, PMD,CPD and coverage).


### Run and configure

We encourage you to use the docker image of this component instead the next commands, because it is easier to use.

If you want to run this component you must to follow the next steps:

 - Compile the project (`./mvnw clean install`)
 - On the directory where you want to install the component (for example **~/interaction-protocol-engine**) create the directories **etc** and **lib**.
 - Copy the compiled jar (`cp target/wenet-interaction-protocol-engine-VERSION.jar ~/interaction-protocol-engine/.`).
 - Copy the jar dependencies (`cp target/lib/* ~/interaction-protocol-engine/lib/.`).
 - Copy the default logging configuration (`cp src/main/resources/tinylog.properties ~/interaction-protocol-engine/etc/log_configuration.properties.`).
 - Copy the default component configuration (`cp src/main/resources/wenet-interaction-protocol-engine.configuration.json ~/interaction-protocol-engine/etc/configuration.conf.`).
 - Edit the component configuration to fix the URL of the other components and the database connection.
 - Go to the install directory and execute the command `java -jar -Dtinylog.configuration=etc/log_configuration.properties wenet-interaction-protocol-engine-VERSION.jar -c etc`.


## Documentation

The latest APIs documentation is available [here](http://swagger.u-hopper.com/?url=https://bitbucket.org/wenet/wenet-components-documentation/raw/master/sources/wenet-interaction_protocol_engine-openapi.yaml).


## Instances

The interaction protocol engine has the next available instances:

 - WeNet production interaction protocol engine API is available at ["https://wenet.u-hopper.com/prod/interaction_protocol_engine]("https://wenet.u-hopper.com/prod/interaction_protocol_engine).
 - WeNet development interaction protocol engine API is available at ["https://wenet.u-hopper.com/dev/interaction_protocol_engine]("https://wenet.u-hopper.com/dev/interaction_protocol_engine).
 - The IIIA stable interaction protocol engine API is available at ["https://wenet.u-hopper.com/dev/interaction_protocol_engine]("https://wenet.u-hopper.com/dev/interaction_protocol_engine).
 - The IIIA development interaction protocol engine API is available at ["https://wenet.u-hopper.com/dev/interaction_protocol_engine]("https://wenet.u-hopper.com/dev/interaction_protocol_engine).
 - The interaction protocol engine API 0.10.X is available at [http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/0.10/](http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/0.10/).
 - The interaction protocol engine API 0.9.0 is available at [http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/0.9.0/](http://ardid.iiia.csic.es/wenet/interaction-protocol-engine/0.9.0/).
 - The interaction protocol engine API 0.8.0 (Dummy version) is available at [http://ardid.iiia.csic.es/dev-wenet-interaction-protocol-engine/](http://ardid.iiia.csic.es/dev-wenet-interaction-protocol-engine/](http://ardid.iiia.csic.es/dev-wenet-interaction-protocol-engine/](http://ardid.iiia.csic.es/dev-wenet-interaction-protocol-engine/).


## License

This software is under the [MIT license](LICENSE)


## Interaction with other WeNet components


### Task manager

 - Get task status associated to the message to process (GET {{task_manager_api}}/tasks/{{taskId}})
 - Update the task every time a transaction message is process (PUT {{task_manager_api}}/tasks/{{taskId}})


### Service

 - Get all the users of an application to rank them and ask for help when a task is created (GET {{service_api}}/app/{{appId}}/users)
 - Get application information associated to the message to process (GET {{service_api}}/app/{{appId}})


### WeNet application

 - Inform about the result of any task transaction message (POST {{messageCallbackUrl}}/)
 - Inform about an incentive (POST {{messageCallbackUrl}}/)


### Social context builder

 - Inform every time a new user accept to provide help. (POST {{social_context_builder_api}}/social/preferences/{{userId}}/{{taskId}})
 - When an user offer as volunteer add to the text of the notification the explanation why the user is selected. (GET {{social_context_builder_api}}/social/explanations/{{userID}}/{{taskID}})


### Incentive server

 - Inform every time a task transaction success and change the state. (POST {{incentive_server_api}}/Tasks/TaskStatus/})


## Contact

### Researcher

 - [Nardine Osman](http://www.iiia.csic.es/~nardine/) ( [IIIA-CSIC](https://www.iiia.csic.es/~nardine/) ) nardine (at) iiia.csic.es
 - [Carles Sierra](http://www.iiia.csic.es/~sierra/) ( [IIIA-CSIC](https://www.iiia.csic.es/~sierra/) ) sierra (at) iiia.csic.es

### Developers

 - Joan Jené ( [UDT-IA, IIIA-CSIC](https://www.iiia.csic.es/people/person/?person_id=19) ) jjene (at) iiia.csic.es
 - Bruno Rosell i Gui ( [UDT-IA, IIIA-CSIC](https://www.iiia.csic.es/people/person/?person_id=27) ) rosell (at) iiia.csic.es

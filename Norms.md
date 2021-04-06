# Norms

A norm is a set of conditions that if they are satisfied provokes that has to do a set of actions,
and with this mechanism is controlled the interactions in the WeNet platform. These interactions
are modelled as messages between WeNet users or between a component and a WeNet user. The component
that do this is the WeNet interaction protocol engine. This component is formed by a set of norm engines,
where each one is responsible to get the norms to evaluate from the point of view of a user and to do
the actions of the norms that will be satisfied. In other words, each user on the WeNet platform has
its norm engine to evaluate the actions that a user do or the notification that it can receive.
The norms are defined on the profile of the user, on the community that the user is, into the task
that the user is involved in and in the same message. The norm engine of each user decided with norms
has to be evaluated depending of the interaction and the status of the user. These norm engines are
in rest mode and when a message is received, they collect the necessary norms evaluate them and do 
the actions of the norms that are satisfied. The messages that start a norm engine are associated
with one of the next actions:

- **Create a task**: When a user wants to create a task.
- **Do transaction**: When a user wants to modify a task.
- **Send Incentive**: When the incentive server wants to send an incentive to a user.
- **Received a message**: When a norm engine receives a message from another norm engine.
- **Received an event**: When a norm engine notifies himself of an event.
 
On the other hand, the norms are formed by a set of conditions that if they are satisfied execute
a set of actions associated with it. A grosso modo the conditions check the action that has started
the norm engine or the status of the user, and the actions can send a message to the user associated
with the norm engine or send a message to the norm engine of another user. 

The norm engine is developed in [SWI Prolog](https://www.swi-prolog.org/), so this is the language
that is used to define the norms. The norm and action as defined as predicates. A predicate is like
a function that starts with a lower case letter followed by letters, numbers or the underscore
and after that between parenthesis are the arguments of the action or the condition separated
by commas. For example:

```prolog
get_task_attribute_value(StartTime,'startTime')
```

The data model of a norm is defined in JSON, and it has the next fields:

- **whenever** is used to define the conditions that have to satisfy to fire the actions.
 	Each condition can be separated by the conjunctions **and**, **or** or **not**.
- **thenceforth** is used to define the actions to do if the conditions are satisfied.
 	Each action is separated by the conjunction **and**.
- **ontology** is used to define new prolog predicates that can be used in any condition
   or action of a norm. If the predicate can be used as an action you must define it as **dynamic**.

The next example is the JSON representation of a norm to send an error to the user if try to create a task
with the start time less than or equals to now.

```json
{
 "whenever": "is_received_created_task() and get_task_attribute_value(StartTime,startTime) and is_now_greater_than_or_equal_to(StartTime)",
 "thenceforth": "close_task() and send_error('cannot_create_task_with_bad_startTime')",
 "ontology": ":- dynamic send_error/1. send_error(Code) :- get_task_id(TaskId), send_user_message('Error',json([code=Code,taskId=TaskId]))."
}
```

As you can see the arguments of the conditions or the actions can be a **variable** or a value. A variable starts
with an upper case letter followed by letters, numbers or the underscore. ATTENTION, the variables
only be associated with a value. In other words, when a variable is associated with a value, it can not be changed
in all the norm. So, if you want to change the value of a variable you must define a new variable. The values 
as argument of the predicate or a value of a variable can be:

- A **Boolean** value that can be ``true`` or ``false``.
- A **Number** value that can represent any integer or floating-point number. For example: ``-1.34`` or ``89``
- A **String** value is a sequence of any characters between single quotes (``'``) or a sequence of characters that
  starts with a lower case followed by letters, numbers or the underscore. For example: ``'String value'`` or ``id``.
- An **Array** value is a list of values between quadratus and separated by commas. For example: ``['One',2,[3,'three']]``
- A **Json** value that is mapped as the predicate ``json`` and with an array as argument where its elements
  are pairs of field name and value separated by an equals. For example: ``json([id='1',goal=json([name='Eat together'])])``.
  You can read more about the conversion from JSON to a predicate [here](https://www.swi-prolog.org/pldoc/doc_for?object=json_read/2).
  Attention if the field name does not start  with a lower case followed for letters, numbers or the underscore,
  it must be written as a string between single quotes. For example: ``json(['Action'='Questions 1','Message'=''])``
- An underscore (``_``) to mark that it accepts any value. In other words, that the value that matches
  this position is ignored.

As you have read before the norms are defined in different data models in the WeNet platform. The norms that affect
any interaction that a user is involved in are defined on the profile of the user. You can modify these norms by doing
HTTP CRUD request over the WeNet profile manager. For the interaction that do any user in a community the norms
are defined on the community profile. You can modify these norms by doing HTTP CRUD request over the WeNet profile manager.
For modifying the logic that is involved in a task you must modify the norms defined on the task and its type using
HTTP CRUD requests over the task manager.


## Protocol actions

Before you have read that the norm engine is activated for one of the five possible actions, now you can read more about
what happens when each of these actions happens.

### Create a task

When a user wants to create a task to do interact with the user interface of the application. This converts this
into an HTTP request to the WeNet service component. This component checks the authentication and authorization
of the user that runs the application. If it is valid the service component calls the WeNet task manager with the task
to create. This component checks that the new task is valid and if it is then the task is stored and finally it calls 
the WeNet interaction protocol engine to inform that a task is created. This last one gets the norms of the user that
create the task, the norms of the community where is defined the task, the norms of the task type associated with the task,
and the norms of the task, and start a new norm engine to validate this tasks. From this point what happens depends
on the fired norms and their associated actions. But normally what happens is that will be fired a set of norms that decide
which of the WeNet users will collaborate to do the created task. These actions normally send a message to each norm engine
of the selected users. Then each norm engine collects the norms of the user, the community, the task type, and the task
and decides the message to send to the user. When is decided this message is posted as an HTTP to the message callback
of the application where the task is created. An then the user interface of the application show that it is selected
to participate in the task.

The task validation by the WeNet task manager consists of verifying:

- If an identifier is set that is not already defined and it has less than 255 characters. If it is not set
  the task manager will set a unique identifier to the task.
- That it has a task type, that this type exists and that the attributes of the task follow the possible attributes
  defined on the type.
- That the user that wants to create the task (requester) has a profile on the WeNet platform.
- That the application associated with the task exists.
- That the community associated with the task exists, or if it is not set it assigns the default community
  of the application where the task is added.
- That the user defines the name of the goal. It can not be larger than 255 characters. If you want to write something
  larger you can add this extra information on the goal description that its maximum is 1023 characters.


### Do a task transaction

When a user wants to interact in a task it has to do a task transaction. This begins with an interaction on the user
interface of the application, that is converted into an HTTP request to the WeNet service component. This component
checks the authentication and authorization of the user that runs the application. If it is valid the service component
calls the WeNet task manager with the transaction to do. This component checks that the transaction is valid and if it is
calls the WeNet interaction protocol engine to inform about the transaction to do. This last one gets the norms of the user
that do the transaction, the norms of the community of the task, the norms of the task type associated with the task,
and the norms of the task and start a new norm engine to validate this transaction. From this point what happens depends
on the fired norms and their associated actions. But normally what happens is that will be fired a set of norms that
changes the state of the user or the task and notify of these changes to the norm engines of the other users that
participate in the task. The notified norm engines get again the norms by with the norms of the user that represents
and as result send a callback message to the user, that will be converted into an event on the user interface of the user
application.


The task transaction validation by the WeNet task manager consists of verifying:

- That the task associated with the transaction is defined.
- That the agent that does the transaction has a profile on the WeNet platform.
- That the label and attributes of the transaction match any of the possibles defined by the task type associated
  with the task of the transaction.
 

### Send an incentive

When the WeNet incentive server wants to send an incentive to a user it posted an HTTP request to the WeNet interaction
protocol engine with the incentive to send to the user. This component validates the incentive and if it is valid gets
the norms of the user and the norms of the community of the application with in the incentive. And after that
start the norm engine to decide what to do next depending of the norms that will be fired. Normally this norms
converts the incentive into a callback message that is sent to the application of the user, which converts the message
into an event on the user interface of the user.

The incentive validation by the WeNet interaction protocol engine consists of verifying:

- That the task associated with the transaction is defined.
- That the agent that does the transaction has a profile on the WeNet platform.
- That the label and attributes of the transaction match any of the possibles defined by the task type associated
  with the task of the transaction.


### Received message

When a norm engine wants to notify the norm engine of another user it posted an HTTP request to the WeNet interaction
protocol engine, which validates the message and passes it through to the norm engine of the user. It obtains the norms
associated with the message and validates them to known with actions to do next, as you can read on the previous actions.


### Received event

An event is a message that a norm engine that is sent to itself after a delay in seconds. It is used to activate the norm
engine and evaluate again the norms. For example: imagine a task that you want to close after a day, then you may define a
norm that when the task is created send an event to be delivered after a day, and another norm to close the task when
this event is received.


## Conditions

The norms are formed by a list of conditions separated by the conjunctions ``and`` or ``or``, or negated using
the predicate ``not(Condition)``. You can use any predicate defined by the [SWI Prolog](https://www.swi-prolog.org/).
The next sections shows the most common conditions that has been provided to help to the norm definition.


### Message conditions

The next conditions are used to check the received message that has started the norm engine.

- ``is_received_created_task()`` This condition is **true** when the received message is to inform that a user
  has created a task.
- ``is_received_do_transaction(Label,Attributes)`` This condition is **true** when the received message is
  to inform that a user wants to do a transaction. The label of the transaction is an string to match
  the variable ``Label``, and the attributes are a JSON model to match ``Attributes``.
- ``is_received_send_incentive(Incentive)`` This condition is **true** when the received message is
  send by the incentive server who want to notify a user about an incentive. The attribute ``Incentive`` is a JSON model
  that contains the incentive to send to the user.
- ``is_received(SenderId,Particle,Content)`` This condition is **true** when the norm engine receive a message from another
  norm engine. The identifier of the sender is a string that matches the variable ``SenderId``, the particle of the message
  is a string to match the variable ``Particle``, and the content is a JSON model associated with ``Content``.
- ``is_received_event(Particle,Content)`` This condition is **true** when the received message is an event. The particle
  of the event is a string to match the variable ``Particle``, and the content is a JSON model associated with ``Content``.


### Users conditions

The next conditions are used to manage the users that can be involved in a task. On this condition **me** refers
to the user of the norm engine that has been started.

- ``get_app_users(Users)`` This condition is **true** when can obtain the users that are on the application.
 The variable ``Users`` is a list of strings with the identifiers of the users that are on the application.
- ``get_app_users_except_me(Users)`` This condition is **true** when can obtain the users that are on the application
 minus my identifier. The variable ``Users`` is a list of strings with the identifiers of the users that are
 on the application where is removed the identifier of me.
- ``get_closest_users_to_me(Users)`` This condition is **true** when can obtain as maximum the 10 closest user to me.
 The variable ``Users`` is a list of strings with the identifiers of the closest user to me.
- ``get_closest_users_to_me(Users,NumUsers)`` This condition is **true** when can obtain some closest user to me.
 The variable ``Users`` is a list of strings with the identifiers of the closest user to me and the variable ``NumUsers``
 is a number of maximum numbers to return.
- ``get_app_users_near_me(Users,Min,Max)`` This condition is **true** when can obtain some users that their distance to me
 is in a range. The variable ``Users`` matches a list of string with the users' identifiers, and the ``Min`` is the minimum
 distance in meters and ``Max``is the maximum distance to me (Both inclusive).


### Status conditions

The next conditions are used to check if that the status of any component. 

- ``get_profile_id(Me)`` This condition is **true** when the variable ``Me`` is an string with the user identifier
  associated to the norm engine.
- ``get_profile(Profile)`` This condition is **true** when the variable ``Profile`` is a JSON model with the profile
  of the user associated to the norm engine.
- ``get_user_state(State)`` This condition is **true** when the variable ``State`` is a JSON model with the state
  of the user associated to the norm engine.
- ``get_user_state_attribute(Value,Key)`` This condition is **true** when the attribute name string value ``Key`` 
  of the state of the user associated to the norm engine is defined and has the value of any type of ``Value``.
- ``get_user_state_attribute(Value,Key,DefaultValue)`` This condition is **true** when the attribute name
  string value ``Key`` of the state of the user associated to the norm engine is defined and has the value
  of any type of ``Value``, or if the attribute is not defined the ``Value`` has to be equals to ``DefaultValue``.
- ``get_app_id(AppId)`` This condition is **true** when the variable ``AppId`` is an string with the identifier
  of the application associated to the received message.
- ``get_app(App)`` This condition is **true** when the variable ``App`` is a JSON model with the application
  associated to the received message.
- ``get_app_message_callback_url(Url)`` This condition is **true** when the variable ``Url`` is a string URL to post
  the callback messages of teh application associated to the received message.
- ``get_community_id(CommunityId)`` This condition is **true** when the variable ``CommunityId`` is an string with the identifier
  of the community associated to the received message.
- ``get_community(Community)`` This condition is **true** when the variable ``Community`` is a JSON model with the community
  profile associated to the received message.
- ``get_community_state(State)`` This condition is **true** when the variable ``State`` is a JSON model with the state
  of the user on the community associated to the norm engine.
- ``get_community_state_attribute(Value,Key)`` This condition is **true** when the attribute name string value ``Key`` 
  of the state of the user on the community associated to the norm engine is defined and has the value of any type of ``Value``.
- ``get_community_state_attribute(Value,Key,DefaultValue)`` This condition is **true** when the attribute name
  string value ``Key`` of the state of the user on the community associated to the norm engine is defined and
  has the value of any type of ``Value``, or if the attribute is not defined the ``Value`` has to be equals to ``DefaultValue``.
- ``get_task_id(TaskId)`` This condition is **true** when the variable ``TaskId`` is an string with the identifier
  of the task associated to the received message.
- ``get_task(Task)`` This condition is **true** when the variable ``Task`` is a JSON model with the task
  associated to the received message.
- ``get_task_requester_id(RequesterId)`` This condition is **true** when the variable ``RequesterId`` is a string with
  the user identifier of the user that has requested to do the task associated to the received message.
- ``get_task_goal_name(Name)`` This condition is **true** when the variable ``Name`` is a string with
  the goal name of the task associated to the received message.
- ``is_task_closed()`` This condition is **true** when the task associated to the received message is closed.
- ``get_task_attribute_value(Value,Key)`` This condition is **true** when the variable ``Key`` is a string of an attribute
  of the task associated to the received message, and the ``Value`` is of any type with the attribute value.
- ``get_task_state(State)`` This condition is **true** when the variable ``State`` is a JSON model with the state
  of the user on the task associated to the norm engine.
- ``get_task_state_attribute(Value,Key)`` This condition is **true** when the attribute name string value ``Key`` 
  of the state of the user on the task associated to the norm engine is defined and has the value of any type of ``Value``.
- ``get_task_state_attribute(Value,Key,DefaultValue)`` This condition is **true** when the attribute name
  string value ``Key`` of the state of the user on the task associated to the norm engine is defined and
  has the value of any type of ``Value``, or if the attribute is not defined the ``Value`` has to be equals to ``DefaultValue``.
- ``get_task_type_id(TaskTypeId)`` This condition is **true** when the variable ``TaskTypeId`` is an string with the identifier
  of the task type associated to the task of the received message.
- ``get_task_type(TaskType)`` This condition is **true** when the variable ``TaskType`` is a JSON model with the task
  type of teh task associated to the received message.
- ``get_transaction_id(TransactionId)`` This condition is **true** when the variable ``TransactionId`` is an string with
  the identifier of the task transaction associated to the received message.
- ``get_transaction(Transaction)`` This condition is **true** when the variable ``Transaction`` is a JSON model with the task
  transaction associated to the received message.
- ``get_transaction(Transaction,TransactionId)`` This condition is **true** when the variable ``Transaction`` is a JSON model
  with the task transaction defined on the task associated to the received message that has an identifier equals to
  the string defined in ``TransactionId``.
- ``get_social_explanation(Explanation,UserId)`` This condition is **true** when the variable ``Explanation`` is a JSON model
  with the explanation provided by the social context builder to selct the user with the identifeir as string defined
  at ``UserId``.


### Time conditions

The constant **now** is set to the difference, measured in seconds, between the time the norm engine is started
and midnight, January 1, 1970 UTC.

- ``is_now_less_than(Time)`` This condition is **true** when **now** is less than the value associated to ``Time``.
- ``is_now_less_than_or_equal_to(Time)`` This condition is **true** when **now** is less than or equal to
  the value associated to ``Time``.
- ``is_now_greater_than(Time)`` This condition is **true** when **now** is greater than the value associated to ``Time``.
- ``is_now_greater_than_or_equal_to(Time)`` This condition is **true** when **now** is greater than or equal
  to the value associated to ``Time``.
- ``is_now_equal_to(Time)`` This condition is **true** when **now** is equal to ``Time``.


### Others

- ``get_attribute(Value,Key,Json)`` This condition is **true** when the attribute with the string value
 defined in ``Key`` is defined on the JSON model of ``Json`` with a value of any type that matches ``Value`.
- ``get_attribute(Value,Key,DefaultValue,Json)`` This condition is **true** when the attribute with the string value
 defined in ``Key`` is defined on the JSON model of ``Json`` with a value of any type that matches ``Value``,
 or if the attribute is not dfined ``Value``is equals to ``DefaultValue``.


## Actions

The norms to do when the conditions of a norm  are predicates separated by the conjunction ``and``. You can use
any predicate defined by the [SWI Prolog](https://www.swi-prolog.org/), but before you have to mark it as ``dynamic``
on the ontology of the norm. The next sections shows the most common actions that has been provided to help 
to the norm definition.


### User actions

This action are done over the user that is associated to the norm engine, thus the receiver user of the message.

- ``send_user_message(Label,Content)`` This action post a callback message in the application associated
 in the received message with the label as string defined in ``Label`` and as content the JSON model of ``Content``.
- ``merge_user_state(State)`` This action merge the current state of the user with the state defined by the JSON
 in ``State``.
- ``put_user_state_attribute(Key,Value)`` This action change the user state attribute named in ``Key`` with the value
 of any type defined in ``Value``.
 

### Community actions

This action are done over the community that is associated to the received message.

- ``merge_community_state(State)`` This action merge the current state of the user for the community with the state
 defined by the JSON in ``State``.
- ``put_community_state_attribute(Key,Value)`` This action change the community user state attribute named
 in ``Key`` with the value of any type defined in ``Value``.


### Task actions

This actions are done in the task that is associated to the received message.

- ``add_created_transaction()`` This action add a new transaction to the task that represents that the creation
 of the task.
- ``add_message_transaction()`` This action add the transaction defined on the received message into the task.
- ``put_task_attribute(Key,Value)`` This action change the attribute with the string name ``Key`` with the value
 associated to the variable``Value``.
- ``merge_task(Task)`` This action merge the task associated with the JSON model defined in ``Task``.
- ``close_task()`` This action mark the task associated to the norm engine as closed.
- ``merge_task_state(State)`` This action merge the current state of the user for the task with the state
 defined by the JSON in ``State``.
- ``put_task_state_attribute(Key,Value)`` This action change the task user state attribute named
 in ``Key`` with the value of any type defined in ``Value``.


### Interact with WeNet components

This actions are used to do an action that interact with the other components of the WeNet platform.

- ``send_messages(Users,Particle,Content)`` This action send messages to the norm engines of the users
 defined in the array of strings ``Users`` with the string particle in ``Particle`` and as content
 the JSON defined in ``Content``,
- ``send_message(UserId,Particle,Content)`` This action send a message to the norm engines of the user
 defined in the string ``UserId`` with the string particle in ``Particle`` and as content
 the JSON defined in ``Content``,
- ``notify_incentive_server(Action,Message)`` This action notify the incentive server about a change of the task
 associated to the received Message. The notification contains as string an action and a message defined
 on the variables of the same name.
- ``notify_volunteers_to_social_context_builder(Volunteers,UserId)`` This action notify the social context builder
 that the user identifier by the string at ``UserId`` is offered as volunteer for the task associated
 to the received message. In ``Volunteers`` is the list of string identifiers of the volunteers users for the task.
 

### Logs messages

The next actions are used to add messages to the logging files of the norm engine.

- ``wenet_log_trace(Text)`` This action print the string defined in ``Text`` into the trace log.
- ``wenet_log_trace(Text,Terms)`` This action print the string defined in ``Text`` with the ``Terms`` into
  the trace log.
- ``wenet_log_error(Text)`` This action print the string defined in ``Text`` into the error log.
- ``wenet_log_error(Text,Terms)`` This action print the string defined in ``Text`` with the ``Terms`` into
  the error log.
- ``wenet_log_error(Text,Terms,Error)`` This action print the string defined in ``Text`` with the ``Terms``
 and exception ``Error`` into  the trace log.



## Other Useful Norms predicates

We provide other predicates that can be used to help to define the conditions and the actions of any norm.

### Facts

The next predicates refers to facts (constants) that has been set when the norm engine has been started.

- ``get_message(Message)`` The ``Message`` is a JSON model that contains the received message that has started
  the norm engine.
- ``get_now(Time)`` This ``Time`` is a number that represents the difference, measured in seconds, between
  the time the norm engine is started and midnight, January 1, 1970 UTC.
- ``wenet_profile_manager_api_url(URL)`` The ``URL`` is a string with the URL to the profile manager API.
- ``wenet_task_manager_api_url(URL)`` The ``URL`` is a string with the URL to the task manager API.
- ``wenet_interaction_protocol_engine_api_url(URL)`` The ``URL`` is a string with the URL to the interaction
 protocol engine API.
- ``wenet_social_context_builder_api_url(URL)`` The ``URL`` is a string with the URL to the social context
 builder API.
- ``wenet_service_api_url(URL)`` The ``URL`` is a string with the URL to the service API.
- ``wenet_incentive_server_api_url(URL)`` The ``URL`` is a string with the URL to the incentive server API.
- ``wenet_personal_context_builder_api_url(URL)`` The ``URL`` is a string with the URL to the personal context
 builder API.



### Utils


- ``wenet_remove(Result,Element,List)`` This predicate returns as ``Result`` the value of the array defined
 in ``List`` without the value defined in ``Element``.
- ``wenet_add(Result,Element,List)`` This predicate returns as ``Result`` the value of the array defined
 in ``List`` adding at the end the value defined in ``Element``.
- ``wenet_format(Msg,WeNetFormat,Arguments)`` This predicate return the ``Msg`` as an string where on 
 the string defined at ``WeNetFormat`` the value **{}** are replaced with the argument ``Arguments``
 on the same position. For example: ``wenet_format('Value One and 2','Value {} and {}',['One',2])``

- ``wenet_math(Number,Expr)`` This predicate evaluates an arithmetical expression defined in ``Expr``
 and return the result in ``Number``. You can read more about the arithmetical expressions
 [here](https://www.swi-prolog.org/pldoc/man?section=functions). For example: ``wenet_format(4,2+2)``



### Profile manager

The next predicates are used to interact with the profile manager component.

- ``wenet_profile_manager_api_url_to(Url,Paths)`` This predicate is used to obtain the URL to interact
  with the API of the profile manager. The ``Url`` is the string of the API point and path in an array
  of strings or variables used to build the URL. For example if ``UserId = '2'`` and the URL of the profile
  manager API is **https://wenet.u-hopper.com/prod/profile_manager** then
  ``wenet_profile_manager_api_url_to(Url,['/profiles/',UserId])`` will produce
  ``URL = 'https://wenet.u-hopper.com/prod/profile_manager/profiles/2'``.
- ``wenet_profile_manager_get_profile(Profile,Id)`` This predicate is used to obtain the profile of an user.
 In the variable ``Profile`` will be  a JSON model with the profile of the user with the identifier 
 defined as string in ``Id ``.
- ``wenet_profile_manager_get_community(Community,Id)`` This predicate is used to obtain a community.
 In the variable ``Community`` will be  a JSON model with the community profile with the identifier 
 defined as string in ``Id ``.
- ``wenet_id_of_profile(Id, Profile)`` This predicate allow to obtain the identifier as string in ``Id``
 of a profile JSON model ``Profile``.
- ``wenet_id_of_community(Id, Community)`` This predicate allow to obtain the identifier as string in ``Id``
 of a community JSON model ``Community``.


### Task manager

The next predicates are used to interact with the task manager component.

- ``wenet_task_manager_api_url_to(Url,Paths)``
  This predicate is used to obtain the URL to interact with the API of the task manager.
  For example if ``UserId = '2'`` and the URL of the task manager API is
  **https://wenet.u-hopper.com/prod/task_manager** then ``wenet_task_manager_api_url_to(Url,['/tasks/',UserId])``
  will produce ``URL = 'https://wenet.u-hopper.com/prod/task_manager/tasks/2'``.
    * ``Url``  _Output_  string of the API point to the task manager.
    * ``Paths``  _Input_  array of values used to build the API point.
- ``wenet_task_manager_get_task(Task,Id)``
  This predicate is used to obtain a task.
    * ``Task``  _Output_  JSON model with the obtained task.
    * ``Id ``  _Input_  string with the idnetifier of the task to obtain.
- ``wenet_task_manager_merge_task(MergedTask,TaskId,Task)``
  This predicate allow to modify a task model.
    * ``MergedTask``  _Output_  JSON model with the updated task.
    * ``TaskId``  _Input_  string with the identifier of the task to modify.
    * ``Task``  _Input_  JSON model of the task to merge with the current one.
- ``wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,TaskId,Transaction)``
 This predicate is used to add a transaction into a task.
    * ``AddedTaskTransaction``  _Output_  JSON model with the added transaction.
    * ``TaskId``  _Input_  string with the identifier of the task to add the transaction.
    * ``Transaction``  _Output_  JSON model of teh transaction to add.
- ``wenet_task_manager_add_message_into_transaction(AddedTransactionMessage,TaskId,TransactionId,Message)``
  This predicate is used to add a message into a transaction of a task.
    * ``AddedTransactionMessage``  _Output_  JSON model with the added message.
    * ``TaskId``  _Input_  string with the identifier of the task where is the transaction.
    * ``TransactionId``  _Input_  string with the identifier of the transaction to add the message.
    * ``Message``  _Input_  JSON model with the message to add into the transaction.
- ``wenet_id_of_task(Id, Task)`` This predicate allow to obtain the identifier of a task.
    * ``Id``  _Output_  string with the identifier of the task.
    * ``Task``  _Input_  JSON model of the task to obtain the identifier.
- ``wenet_task_type_id_of_task(Id, Task)`` 
  This predicate allow to obtain the task type identifier of a task.
    * ``Id``  _Output_  string with the identifier of the task type.
    * ``Task``  _Input_  JSON model of the task to obtain the task type identifier.
- ``wenet_app_id_of_task(Id, Task)``
  This predicate allow to obtain the application identifier of a task.
    * ``Id``  _Output_  string with the identifier of the application.
    * ``Task``  _Input_  JSON model of the task to obtain the application identifier.
- ``wenet_community_id_of_task(Id, Task)``
  This predicate allow to obtain the community identifier of a task.
    * ``Id``  _Output_  string with the identifier of the community.
    * ``Task``  _Input_  JSON model of the task to obtain the community identifier.
- ``wenet_requester_id_of_task(Id, Task)``
  This predicate allow to obtain the requester identifier of a task.
    * ``Id``  _Output_  string with the identifier of the requester. Thus, the user that wants to do the task.
    * ``Task``  _Input_  JSON model of the task to obtain the requester identifier.
- ``wenet_goal_of_task(Goal, Task)``
  This predicate allow to obtain the goal of a task.
    * ``Goal``  _Output_  JSON model with the task goal.
    * ``Task``  _Input_  JSON model of the task to obtain the goal.
- ``wenet_goal_name_of_task(GoalName, Task)``
  This predicate allow to obtain the goal name of a task.
    * ``GoalName``  _Output_  string with the task goal name.
    * ``Task``  _Input_  JSON model of the task to obtain the goal name.
- ``wenet_goal_description_of_task(GoalDescription, Task)``
  This predicate allow to obtain the goal description of a task.
    * ``GoalDescription``  _Output_  string with the task goal description.
    * ``Task``  _Input_  JSON model of the task to obtain the goal description.
- ``wenet_goal_keywords_of_task(GoalKeywords, Task)``
  This predicate allow to obtain the goal keywords of a task.
    * ``GoalKeywords``  _Output_  array of strings with the task goal keywords.
    * ``Task``  _Input_  JSON model of the task to obtain the goal keywords.
- ``wenet_is_closed_task(Task)``
  This predicate is **true** if the task is closed.
    * ``Task``  _Input_  JSON model of the task to check if it is closed.
- ``wenet_close_ts_of_task(CloseTs, Task)``
  This predicate allow to obtain the time-stamp when the task is closed.
    * ``CloseTs``  _Output_  integer with the difference, in seconds ,between the time when the task is closed
    and midnight, January 1, 1970 UTC.
    * ``Task``  _Input_  JSON model of the task to obtain the close time-stamp.
- ``wenet_attributes_of_task(Attributes, Task)``
  This predicate allow to obtain the attributes of a task.
    * ``Attributes``  _Output_  JSON model with the task attributes.
    * ``Task``  _Input_  JSON model of the task to obtain the attributes.
- ``wenet_transactions_of_task(Transactions, Task)``
  This predicate allow to obtain the transaction of a task.
    * ``Transactions``  _Output_  array of JSON models with the task transactions.
    * ``Task``  _Input_  JSON model of the task to obtain the transactions.
- ``wenet_id_of_transaction(Id, Transaction)``
  This predicate allow to obtain the identifier of a transaction.
    * ``Id``  _Output_  string with the transaction identifier.
    * ``Transaction``  _Input_  JSON model of the transaction to obtain the identifier.
- ``wenet_task_id_of_transaction(TaskId, Transaction)``
  This predicate allow to obtain the task identifier of a transaction.
    * ``TaskId``  _Output_  string with the task identifier.
    * ``Transaction``  _Input_  JSON model of the transaction to obtain the task identifier.
- ``wenet_label_of_transaction(Label, Transaction)``
  This predicate allow to obtain the label of a transaction.
    * ``Label``  _Output_  string with the transaction label.
    * ``Transaction``  _Input_  JSON model of the transaction to obtain the label.
- ``wenet_attributes_of_transaction(Attributes, Transaction)``
  This predicate allow to obtain the attributes of a transaction.
    * ``Attributes``  _Output_  JSON model with the transaction attributes.
    * ``Transaction``  _Input_  JSON model of the transaction to obtain the attributes.
- ``wenet_actioneer_id_of_transaction(ActioneerId,, Transaction)``
  This predicate allow to obtain the actioneer of a transaction.
    * ``ActioneerId``  _Output_  string with the transaction actioneer. Thus, the identifier
    of the user that has done the transaction.
    * ``Transaction``  _Input_  JSON model of the transaction to obtain the identifier.
- ``wenet_messages_of_transaction(Messages, Transaction)``
  This predicate allow to obtain the messages of a transaction.
    * ``Messages``  _Output_  array of JSON models with the transaction messages.
    * ``Transaction``  _Input_  JSON model of the transaction to obtain the messages.



### Interaction protocol Engine


TODO

### Service


TODO


### Incentive server


TODO


### Social context builder


TODO

### Personal context builder


TODO


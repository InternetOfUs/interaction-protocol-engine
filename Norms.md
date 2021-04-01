# Norms

The norms are used to controls the interactions into the WeNet platform. This interactions
can be between WeNet users or between a component and a WeNet user. The control is done
by the interaction protocol engine component on the WeNet platform. This component is formed
by a set of norm engines, where each one is the responsible to evaluate the norms associated to an user. 
In other words, each user on the WeNet platform has its norm engine to evaluate the actions
that an user do, or the notification that it can receive. By default the norms
engines are in rest mode an only are started if one of the next actions happens:

 - **Create a task**: When an user want to create a task.
 - **Do transaction**: When an user want to modify a task.
 - **Send Incentive**: When the incentive server want to send an incentive to an user.
 - **Receive a message**: When a norm engine receive a message from another norm engine.
 - **Received an event**: When a norm engine notify himself of an event in an specific time.
 
On the other hand, the norms are formed by a set of conditions that if they are satisfied execute
a set of action associated to it. A grosso modo the conditions check the action that has started
the norm engine or the status of the user, and the actions can send a message to the user associated
to the norm engine or send a message to the norm engine of another user. 

The norm engine is developed in [SWI Prolog](https://www.swi-prolog.org/), so this is language
that is used to define the norms. The norm and action as defined as predicates. A predicate is like
a function that starts with a lower case letter followed for letter, numbers or the underscore
and after that between parenthesis are the arguments of the action or the condition separated
by commas. For example:

```prolog
get_task_attribute_value(StartTime,'startTime')
```

The data model of a norm is defined in JSON, and it has the next fields:

 - **whenever** is used to define the conditions that has to satisfy to fire the actions.
 	Each condition can be separated by the conjunctions **and**, **or** or **not**.
 - **thenceforth** is used to define the actions to do if the conditions are satisfied.
 	Each action is separated by the conjunction **and**.
 - **ontology** is used to define new prolog predicates that can be used in the condition
   or the actions of the norms. If the predicate can be used as action you must define it as **dynamic**.

The next example is the JSON representation of a norm to send an error to the user if try to create a task
with the start time less or equals to now.

```json
{
 "whenever": "is_received_created_task() and get_task_attribute_value(StartTime,startTime) and is_now_greater_than_or_equal_to(StartTime)",
 "thenceforth": "close_task() and send_error('cannot_create_task_with_bad_startTime')",
 "ontology": ":- dynamic send_error/1. send_error(Code) :- get_task_id(TaskId), send_user_message('Error',json([code=Code,taskId=TaskId]))."
}
```

As you can see the arguments of the conditions or the actions can be a **variable** or a value. A variable starts
with an upper case letter followed for letter, numbers or the underscore. ATTENTION, the variables
only be associated into a value. In other words, when a variable is associated into a value, it can not be changed
in all the norm. So, if you want to change the value of a variable you must define a new variable. The values 
as argument of the predicate or a value of a variable can be:

 - A **Boolean** value that can be ``true`` or ``false``.
 - A **Number** value that can represents any integer or floating point number. For example: ``-1.34`` or ``89``
 - A **String** value as a sequence of any characters between single quotes (``'``) or a sequence of characters that
  starts with a lower case followed for letter, numbers or the underscore. For example: ``'String value'`` or ``id``.
 - An **Array** value as a list of values between quadrators and separated by commas. For example: ``['One',2,[3,'three']]``
 - A **Json** value that is mapped as the predicate ``json`` and with an array as argument where its elements
  are pairs of field name and value separated by an equals. For example: ``json([id='1',goal=json([name='Eat together'])])``.
  You can read more about the conversion from JSON to a predicate [here](https://www.swi-prolog.org/pldoc/doc_for?object=json_read/2).
  Attention if the field name not start a lower case followed for letter, numbers or the underscore, it must be written
  as string between single quotes. For example: ``json(['Action'='Questions 1','Message'=''])``
 - An underscore (``_``) to mark that it accept any value. In other words, that the value that match
  this position is ignored.



## Conditions

### is_now_less_than(Time)

This condition is **true** when **Time** is greater than or equals to **NOW**, where **NOW** is the difference,
measured in seconds, between the time the norm engine is started and midnight, January 1, 1970 UTC.

### is_now_less_than_or_equal_to(Time)

This condition is **true** when **Time** is greater than to **NOW**, where **NOW** is the difference,
measured in seconds, between the time the norm engine is started and midnight, January 1, 1970 UTC.

### is_now_greater_than(Time)

This condition is **true** when **Time** is less than or equals to **NOW**, where **NOW** is the difference,
measured in seconds, between the time the norm engine is started and midnight, January 1, 1970 UTC.

### is_now_greater_than_or_equal_to(Time)

This condition is **true** when **Time** is less than to **NOW**, where **NOW** is the difference,
measured in seconds, between the time the norm engine is started and midnight, January 1, 1970 UTC.

### is_now_equal_to(Time)

This condition is **true** when **Time** is equals to **NOW**, where **NOW** is the difference,
measured in seconds, between the time the norm engine is started and midnight, January 1, 1970 UTC.

### get_profile(Profile)

This condition is **true** when **Profile** is equals to the profile in JSON of the user associated to the norm engine.

### get_profile_id(ProfileId)

This condition is **true** when **ProfileId** is equals to the identifier of the user that has started the norm engine.

### get_community(Community)

This condition is **true** when **Community** is equals to the community in JSON associated to the norm engine.

### get_community_id(CommunityId)

This condition is **true** when **CommunityId** is equals to the identifier of the community associated to the norm engine.

### get_task(Task)

This condition is **true** when **Task** is equals to the task in JSON associated to the norm engine.

### get_task_id(TaskId)

This condition is **true** when **TaskId** is equals to the identifier of the task associated to the norm engine.

### get_task_type(TaskType)

This condition is **true** when **TaskType** is equals to the task type in JSON associated to task of the norm engine.

### get_task_type_id(TaskTypeId)

This condition is **true** when **TaskTypeId** is equals to the identifier of the task type of the norm engine task.

### get_task_type(TaskType)

This condition is **true** when **TaskType** is equals to the task type in JSON associated
to task of the norm engine.

### get_transaction(Transaction,TransactionId)

This condition is **true** when **Transaction** is the JSON object equals to transaction defined
on the task of the norm engine with the identifier equals to **TransactionId**.

### get_transaction_id(TransactionId)

This condition is **true** when **TransactionId** is equals to the identifier of the transaction
associated to the norm engine.

### get_app(App)

This condition is **true** when **App** is equals to the application in JSON associated to the norm engine.

### get_app_id(AppId)

This condition is **true** when **AppId** is equals to the identifier of the application
associated to the norm engine.

### get_app_message_callback_url(Url)

This condition is **true** when **Url** is equals to the URL to post messages to the application
associated to the norm engine.

### get_app_users(Users)

This condition is **true** when **Users** is an array of users identifiers that are registered
on the application associated to the norm engine.

### get_app_users_except_me(Users)

This condition is **true** when **Users** is an array of users identifiers that are registered
on the application associated to the norm engine except the identifier of the user associated
to the norm engine.

### is_received_created_task()

This condition is **true** when the received message to process represents that the user has created a task.

### is_received_do_transaction(Label,Attributes)

This condition is **true** when the received message is a transaction to do with the string label equals
to **Label** and the JSON attribute are equals to **Attributes**.

### is_received_send_incentive(Incentive)

This condition is **true** when the received message is an incentive to send to the user of the norm engine,
and the JSON incentive to send is equal to **Incentive**.

### get_task_attribute_value(Value,Key)

This condition is **true** when an attribute of the task associated to the norm engine has the name **Key**
and its value is equal to **Value**.

### is_received(SenderId,Particle,Content)

This condition is **true** when the message that has started the norm engine is sender by the user **SenderId**,
has the particle equals to **Particle** and the JSON content is equals to **Content**.

### get_task_requester_id(RequesterId)

This condition is **true** when the user that has request to do the task associated to the norm engine
is equal to **RequesterId**.

### get_task_goal_name(Name)

This condition is **true** when the name of the goal of the task associated to the norm engine
is equal to **Name**.

### get_social_explanation(Explanation,UserId)

This condition is **true** when the explanation why to choose a volunteer is equals to the JSON **Explanation**
and the user of the explanation is equal to **UserId**.

### is_task_closed()

This condition is **true** when the task associated to the norm engine is closed.

### get_community_state(State)

This condition is **true** when the community state of the user associated in the norm engine
is equals to the JSON **State**.

### get_community_state_attribute(Value,Key)

This condition is **true** when the attribute with the name **Key** on the community state of the user associated
in the norm engine has the value **Value**.


### get_community_state_attribute(Value,Key,DefaultValue)

This condition is **true** when the attribute with the name **Key** on the community state of the user associated
in the norm engine has the value **Value** or if it is not defined is equals to **DefaultValue**.

### get_attribute(Value,Key,DefaultValue,Json)

This condition is **true** when the attribute with the name **Key** on the JSOn model **Json** has the value **Value**
or if it is not defined is equals to **DefaultValue**.

### get_attribute(Value,Key,Json)

This condition is **true** when the attribute with the name **Key** on the JSOn model **Json** has the value **Value**.

### get_closest_users_to_me(Users)

This condition is **true** when the maximum 10 users closest to the user associated to the norm engine is equals
to the array wit the users identifiers **Users**.

### get_closest_users_to_me(Users,NumUsers)

This condition is **true** when the maximum **NumUsers** users closest to the user associated to the norm engine is equals
to the array wit the users identifiers **Users**.

### get_app_users_near_me(Users,Min,Max)

This condition is **true** when the users near the user associated to the norm engine is equals
to the array wit the users identifiers **Users** and the distance between them in meters
is in the range **Min** and **Max**.

### is_event(Particle,Content)

This condition is **true** when the message that has started the norm engine is an event with the particle
equals to **Particle** and the JSON content is equals to **Content**.

## Actions

### add_created_transaction()

This action add a new transaction to the task associated to the norm engine that represents that the task is created.

### add_message_transaction()

This action add the transaction defined on the received message to the task associated to the norm engine.

### new_user_message(Message,Label,Content)

This action create the JSON **Message** with the string **Label** and the JSON **Content** to be send to
the user associated to the norm engine though the application.

### send_user_message(Label,Content)

This action send a message to the user though the application associated to the norm engine with the string **Label** and
the JSON **Content**.

### put_task_attribute(Key,Value)

This action change the the attribute with the name **Key** with the value **Value** to the task
associated to the norm engine.

### send_messages(Users,Particle,Content)

This action send messages to the norm engines of the **Users** with the particle **Particle** and the JSON **Content**,
and as sender the users associated to the norm engine.

### send_message(UserId,Particle,Content)

This action send a message to the norm engine of the **UserId** with the particle **Particle** and the JSON **Content**,
and as sender the users associated to the norm engine.

### notify_incentive_server(Action,Message)

This action notify the incentive server that the **Action** with the **Message** has done in the task associated
to the norm engine.

### notify_volunteers_to_social_context_builder(Volunteers,UserId)

This action notify the social context builder that the user **UserId** is one of the **Volunteers** to do the task
associated to teh norm engine.

### close_task()

This action mark the task associated to the norm engine as closed.

### merge_task(Task)

This action merge the task associated to the norm engine with the values of the JSON **Task**.

### merge_community_state(CommunityState)

This action merge the community user state with the JSON state defined in **CommunityState**.

### put_community_state_attribute(Key,Value)

This action change the community user state attribute with name **Key** with the value **Value**.

## Constants

### get_now(Now)

This predicate is **true** when **Now**  is the difference, measured in seconds, between the time 
the norm engine is started and midnight, January 1, 1970 UTC.

### get_message(Message)

This predicate is **true** when **Message** is equals to the message in JSON that has started the norm engine.

### wenet_profile_manager_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the profile manager component.

### wenet_task_manager_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the task manager component.

### wenet_interaction_protocol_engine_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the interaction_protocol_engine component.

### wenet_social_context_builder_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the social_context_builder component.

### wenet_service_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the service component.

### wenet_incentive_server_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the incentive_server component.

### wenet_personal_context_builder_api_url(URL)

This predicate is **true** when **URL** is equals to the URL of the API for the personal_context_builder component.

## Utility

### wenet_log_trace(Text)

This predicate write on the log trace the string **Text**.

### wenet_log_trace(Text,Term)

This predicate write on the log trace the string **Text** with the **Term**.

### wenet_log_error(Text)

This predicate write on the log error the string **Text**.

### wenet_log_error(Text,Terms)

This predicate write on the log error the string **Text** with the **Terms**.

### wenet_log_error(Text,Terms,Error)

This predicate write on the log error the string **Text** with the **Terms** and **Error**.

### wenet_remove(Result,Element,List)

This predicate return the **Result** as the **List** where has removed the **Element** from it.

### wenet_add(Result,Element,List)

This predicate return the **Result** as the **List** where has added the **Element** on it.

### wenet_format(Msg,WeNetFormat,Arguments)

This predicate return the **Msg** as an string where on the string **WeNetFormat** the value **{}** 
are replaced with the argument **Arguments** on the same position.
For example: **wenet_format('Value One and 2','Value {} and {}',['One',2])**

### wenet_math(Number,Expr)

This predicate evaluate the mathematical expression **Expr** as set the value into the **Number**.
For example: **wenet_format(4,2+2)**

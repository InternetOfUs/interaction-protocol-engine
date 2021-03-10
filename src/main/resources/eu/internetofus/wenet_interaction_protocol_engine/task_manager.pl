%
% Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.
%

:- dynamic
	wenet_task_manager_api_url_to/2,
	wenet_task_manager_get_task/2,
	wenet_task_manager_merge_task/3,
	wenet_task_manager_add_transaction_into_task/3,
	wenet_task_manager_add_message_into_transaction/4,
	wenet_id_of_task/2,
	wenet_task_type_id_of_task/2,
	wenet_app_id_of_task/2,
	wenet_community_id_of_task/2,
	wenet_requester_id_of_task/2,
	wenet_goal_of_task/2,
	wenet_goal_name_of_task/2,
	wenet_goal_description_of_task/2,
	wenet_goal_keywords_of_task/2,
	wenet_is closed_task/1,
	wenet_close_ts_of_task/2,
	wenet_attributes_of_task/2,
	wenet_transactions_of_task/2,
	wenet_id_of_transaction/2,
	wenet_task_id_of_transaction/2,
	wenet_label_of_transaction/2,
	wenet_attributes_of_transaction/2,
	wenet_actioneer_id_of_transaction/2,
	wenet_messages_of_transaction/2,
	get_task/2,
	get_task_id/2,
	get_task_type_id/2,
	get_task_type_id/1,
	get_task_app_id/2,
	get_task_app_id/1,
	get_task_community_id/2,
	get_task_community_id/1,
	get_task_requester_id/2,
	get_task_requester_id/1,
	get_task_goal/2,
	get_task_goal/1,
	get_task_goal_name/2,
	get_task_goal_name/1,
	get_task_goal_description/2,
	get_task_goal_description/1,
	get_task_goal_keywords/2,
	get_task_goal_keywords/1,
	is_task_closed/1,
	is_task_closed/0,
	get_task_close_ts/2,
	get_task_close_ts/1,
	get_task_attributes/2,
	get_task_attributes/1,
	get_task_attribute_value/3,
	get_task_attribute_value/2,
	get_task_transaction_label/2,
	get_task_transaction_attributes/2,
	get_task_transaction_actioneer_id/2,
	get_task_transaction_task_id/2,
	get_task_transaction_id/2,
	get_task_transaction_messages/2,
	wenet_task_manager_add_transaction_into_task/1,
	wenet_task_manager_add_transaction_into_task/2,
	wenet_task_manager_add_transaction_into_task/3,
	wenet_task_manager_add_message_into_transaction/1,
	wenet_task_manager_add_message_into_transaction/2,
	wenet_task_manager_add_message_into_transaction/4,
	wenet_task_manager_add_created_transation/0,
	wenet_task_manager_add_created_transation/1,
	wenet_task_manager_merge_task/3,
	wenet_task_manager_merge_task/2
	.


%!	wenet_task_manager_api_url_to(+Url,-Paths)
%
%	Calculate the URL from a path
%
wenet_task_manager_api_url_to(Url,Paths) :-
	wenet_task_manager_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_task_manager_merge_task(-MergedTask,+TaskId,+Task)
%
%	Merge a task with the information of another task.
%
%	@param MergedTask that has been merged.
%	@param TaskId identifier of the task to merge.
%	@param Task with the data to merge.
%
wenet_task_manager_merge_task(MergedTask,TaskId,Task) :-
	wenet_task_manager_api_url_to(Url,['/tasks/',TaskId]),
	wenet_patch_json_to_url(MergedTask,Url,Task)
	.

%!	wenet_task_manager_add_transaction_into_task(-AddedTaskTransaction,+TaskId,+Transaction)
%
%	Add a task transaction into a task.
%
%	@param AddedTaskTransaction return task transaction that has been added into the task.
%	@param TaskId identifier of the task to add the transaction.
%	@param Transaction to add to the task.
%
wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,TaskId,Transaction) :-
	wenet_task_manager_api_url_to(Url,['/tasks/',TaskId,'/transactions']),
	wenet_post_json_to_url(AddedTaskTransaction,Url,Transaction)
	.

%!	wenet_task_manager_add_message_into_transaction(-AddedTransactionMessage,+TaskId,+TransactionId,+Message)
%
%	Add a message into a transaction.
%
%	@param AddedTransactionMessage return message that has been added into the transaction.
%	@param TaskId identifier of the task where is the transaction to add the message.
%	@param TransactionId identifier of the transaction to add the message.
%	@param Message to add to the transaction.
%
wenet_task_manager_add_message_into_transaction(AddedTransactionMessage,TaskId,TransactionId,Message) :-
	wenet_task_manager_api_url_to(Url,['/tasks/',TaskId,'/transactions/',TransactionId,'/messages']),
	wenet_post_json_to_url(AddedTransactionMessage,Url,Message)
	.

%!	wenet_id_of_task(-Id,+Task)
%
%	Obtain the id of a task.
%
%	@param Id of the task.
%	@param Task to get the id.
%
wenet_id_of_task(Id,json(Task)) :-
	member(taskId=Id,Task)
	.

%!	wenet_id_of_transaction(-Id,+Transaction)
%
%	Obtain the id of a transaction.
%
%	@param Id of the transaction.
%	@param Transaction to get the id.
%
wenet_id_of_transaction(Id,json(Transaction)) :-
	member(transactionId=Id,Transaction)
	.

%!	get_task(+Task,-Id)
%
%	Return the task associated to an identifier.
%
%	@param Task list with the task information.
%	@param Id string identifeir of the task to obtain.
%
get_task(Task,Id) :-
	wenet_task_manager_api_url_to(Url,['/tasks/',Id]),
	wenet_get_json_from_url(Url,Task),
	!,
	asserta(get_task(Task,Id)),
	wenet_log_trace('Loaded task',Task)
	.



%!	get_task_type_id(-TaskTypeId,+Task)
%
%	Obtain the type identifier of a task .
%
%	@param TaskTypeId of the task .
%	@param Task to get the task type identifier.
%
get_task_type_id(TaskTypeId, json(Task)) :-
	member(taskTypeId=TaskTypeId,Task)
	.

%!	get_task_type_id(-TaskTypeId)
%
%	Obtain the type identifier of the last task.
%
%	@param TaskTypeId of the last task.
%
get_task_type_id(TaskTypeId) :-
	env_task(Task),
	!,
	get_task_type_id(TaskTypeId,Task)
	.

%!	get_task_app_id(-AppId,+Task)
%
%	Obtain the app identifier of a task.
%
%	@param AppId of a task.
%	@param Task to get the app identifier.
%
get_task_app_id(AppId, json(Task)) :-
	member(appId=AppId,Task)
	.

%!	get_task_app_id(-AppId)
%
%	Obtain the app identifier of the last task.
%
%	@param AppId of the last task.
%
get_task_app_id(AppId) :-
	env_task(Task),
	!,
	get_task_app_id(AppId, Task)
	.

%!	get_task_community_id(-CommunityId,+Task)
%
%	Obtain the community identifier of a task.
%
%	@param CommunityId of a task.
%	@param Task to get the community identifier.
%
get_task_community_id(CommunityId, json(Task)) :-
	member(communityId=CommunityId,Task)
	.

%!	get_task_community_id(-CommunityId)
%
%	Obtain the community identifier of the last task .
%
%	@param CommunityId of the last task.
%
get_task_community_id(CommunityId) :-
	env_task(Task),
	!,
	get_task_community_id(CommunityId,Task)
	.

%!	get_task_requester_id(-RequesterId,+Task)
%
%	Obtain the requester identifier of a task .
%
%	@param RequesterId of a task .
%	@param Task to get the requester identifier.
%
get_task_requester_id(RequesterId, json(Task)) :-
	member(requesterId=RequesterId,Task)
	.

%!	get_task_requester_id(-RequesterId)
%
%	Obtain the requester identifier of the last task.
%
%	@param RequesterId of the last task.
%
get_task_requester_id(RequesterId, json(Task)) :-
	env_task(Task),
	!,
	get_task_requester_id(RequesterId, Task)
	.

%!	get_task_goal(-Goal,+Task)
%
%	Obtain the goal of a task.
%
%	@param Goal of a task.
%	@param Task to get the goal.
%
get_task_goal(Goal, json(Task)) :-
	member(goal=Goal,Task)
	.

%!	get_task_goal(-Goal)
%
%	Obtain the goal of the last task.
%
%	@param Goal of the last task.
%	@param Task to get the goal.
%
get_task_goal(Goal, json(Task)) :-
	env_task(Task),
	!,
	get_task_goal(Goal, Task)
	.

%!	get_task_goal_name(-GoalName,+Task)
%
%	Obtain the goal name of a task.
%
%	@param GoalName of a task.
%	@param Task to get the goal name.
%
get_task_goal_name(GoalName, json(Task)) :-
	get_task_goal(Goal, Task),
	member(name=GoalName,Goal)
	.

%!	get_task_goal_name(-GoalName)
%
%	Obtain the goal name of the latest task.
%
%	@param GoalName of the latest task.
%
get_task_goal_name(GoalName) :-
	env_task(Task),
	!,
	get_task_goal_name(GoalName,Task)
	.

%!	get_task_goal_description(-GoalDescription,+Task)
%
%	Obtain the goal description of a task.
%
%	@param GoalDescription of a task.
%	@param Task to get the goal description.
%
get_task_goal_description(GoalDescription, json(Task)) :-
	get_task_goal(Goal, json(Task)),
	member(description=GoalDescription,Goal)
	.

%!	get_task_goal_description(-GoalDescription)
%
%	Obtain the goal description of the latest task.
%
%	@param GoalDescription of the latest task.
%
get_task_goal_description(GoalDescription) :-
	env_task(Task),
	!,
	get_task_goal_description(GoalDescription,Task)
	.

%!	get_task_goal_keywords(-GoalKeywords,+Task)
%
%	Obtain the goal keywords of a task .
%
%	@param GoalKeywords of the task .
%	@param Task to get the goal keywords.
%
get_task_goal_keywords(GoalKeywords, json(Task)) :-
	get_task_goal(Goal, json(Task)),
	member(keywords=GoalKeywords,Goal)
	.

%!	get_task_goal_keywords(-GoalKeywords)
%
%	Obtain the goal keywords of the latest task.
%
%	@param GoalKeywords of the latest task.
%
get_task_goal_keywords(GoalKeywords) :-
	env_task(Task),
	!,
	get_task_goal_keywords(GoalKeywords,Task)
	.

%!	get_task_close_ts(+Task)
%
%	Obtain the close time stamp of a task.
%
%	@param CloseTs of a task.
%	@param Task to get the close time stamp.
%
get_task_close_ts(CloseTs, json(Task)) :-
	member(closeTs=CloseTs,Task)
	.

%!	get_task_close_ts(-CloseTs)
%
%	Obtain the close time stamp of the latest task.
%
%	@param CloseTs of the latest task.
%	@param Task to get the close time stamp.
%
get_task_close_ts(CloseTs) :-
	env_task(Task),
	!,
	get_task_close_ts(CloseTs,Task)
	.

%!	is_task_closed(+Task)
%
%	This is true if a task is closed.
%
%	@param Task to check if it is closed.
%
is_task_closed(json(Task)) :-
	not(member(closeTs=_,Task))
	.

%!	is_task_closed()
%
%	This is true if the latest task is closed.
%
is_task_closed() :-
	env_task(Task),
	!,
	is_task_closed(Task)
	.

%!	get_task_attributes(-Attributes,+Task)
%
%	Obtain the attributes of a task.
%
%	@param Attributes of the task.
%	@param Task to get the attributes.
%
get_task_attributes(Attributes, json(Task)) :-
	member(attributes=Attributes,Task)
	.

%!	get_task_attributes(-Attributes)
%
%	Obtain the attributes of the last task.
%
%	@param Attributes of the task.
%
get_task_attributes(Attributes) :-
	env_task(Task),
	!,
	get_task_attributes(Attributes, Task)
	.


%!	get_task_attribute_value(-Value,+Key,+Task)
%
%	Obtain the value of an attributes of a task.
%
%	@param Value of an attribute of a task.
%	@param Key of an attributes to get the value of a task.
%	@param Task to get the attribute value.
%
get_task_attribute_value(Value, Key,Task) :-
	get_task_attributes(json(Attributes),Task),
	member(Key=Value,Attributes)
	.

%!	get_task_attribute_value(-Value,+Key,+Task)
%
%	Obtain the value of an attributes of the last task.
%
%	@param Value of an attribute of the last task.
%	@param Key of an attributes to get the value of the last task.
%
get_task_attribute_value(Value, Key) :-
	env_task(Task),
	!,
	get_task_attribute_value(Value, Key, Task)
	.

%!	get_task_transaction_label(-Label,+TaskTransaction)
%
%	Obtain the label of a task transaction.
%
%	@param Label of the task transaction.
%	@param TaskTransaction to get the label.
%
get_task_transaction_label(Label, json(TaskTransaction)) :-
	member(label=Label,TaskTransaction)
	.

%!	get_task_transaction_attributes(-Attributes,+TaskTransaction)
%
%	Obtain the attributes of a task transaction.
%
%	@param Attributes of the task transaction.
%	@param TaskTransaction to get the attributes.
%
get_task_transaction_attributes(Attributes, json(TaskTransaction)) :-
	member(attributes=Attributes,TaskTransaction)
	.


%!	get_task_transaction_actioneer_id(-ActioneerId,+TaskTransaction)
%
%	Obtain the actioneer identifier of a task transaction.
%
%	@param ActioneerId of the task transaction.
%	@param TaskTransaction to get the actioneer identifier.
%
get_task_transaction_actioneer_id(ActioneerId, json(TaskTransaction)) :-
	member(actioneerId=ActioneerId,TaskTransaction)
	.

%!	get_task_transaction_task_id(-TaskId,+TaskTransaction)
%
%	Obtain the task identifier of a task transaction.
%
%	@param TaskId of the task transaction.
%	@param TaskTransaction to get the task identifier.
%
get_task_transaction_task_id(TaskId, json(TaskTransaction)) :-
	member(taskId=TaskId,TaskTransaction)
	.

%!	get_task_transaction_id(-Id,+TaskTransaction)
%
%	Obtain the id of a task transaction.
%
%	@param Id of the task transaction.
%	@param TaskTransaction to get the id.
%
get_task_transaction_id(Id, json(TaskTransaction)) :-
	member(id=Id,TaskTransaction)
	.

%!	get_task_transaction_messages(-Messages,+TaskTransaction)
%
%	Obtain the messages of a task transaction.
%
%	@param Messages of the task transaction.
%	@param TaskTransaction to get the messages.
%
get_task_transaction_messages(Messages, json(TaskTransaction)) :-
	member(messages=Messages,TaskTransaction)
	.


%!	wenet_task_manager_add_transaction_into_task(+Transaction)
%
%	Add a task transaction into the current task.
%
%	@param Transaction to add to the task.
%
wenet_task_manager_add_transaction_into_task(Transaction) :-
	wenet_task_manager_add_transaction_into_task(json(AddedTaskTransaction),Transaction),
	member(id=AddedTaskTransactionId,AddedTaskTransaction),
	!,
	asserta(env_transaction(json(AddedTaskTransaction))),
	asserta(env_transaction_id(AddedTaskTransactionId))
	.

%!	wenet_task_manager_add_transaction_into_task(-AddedTaskTransaction,+TaskId,+Transaction)
%
%	Add a task transaction into the current task.
%
%	@param AddedTaskTransaction return task transaction that has been added into the task.
%	@param Transaction to add to the task.
%
wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,Transaction) :-
	env_task_id(TaskId),
	wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,TaskId,Transaction)
	.



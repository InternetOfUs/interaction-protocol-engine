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
	get_task_manager_url_to/2,
	get_task/2,
	get_task_id/2,
	get_task_type_id/2,
	get_task_app_id/2,
	get_task_community_id/2,
	get_task_requester_id/2,
	get_task_goal/2,
	get_task_goal_name/2,
	get_task_goal_description/2,
	get_task_goal_keywords/2,
	is_task_closed/1,
	get_task_close_ts/2,
	get_task_transaction_label/2,
	get_task_transaction_attributes/2,
	get_task_transaction_actioneer_id/2,
	get_task_transaction_task_id/2,
	get_task_transaction_id/2,
	get_task_transaction_messages/2,
	wenet_task_manager_add_transaction_into_task/3,
	wenet_task_manager_add_message_into_transaction/4,
	wenet_task_manager_add_created_transation_to_current_task/1
	.


%!	get_task_manager_url_to(+Url,-Paths)
%
%	Calculate the URL from a path
%
get_task_manager_url_to(Url,Paths) :-
	wenet_task_manager_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.


%!	get_task(+Task,-Id)
%
%	Return the task associated to an identifier.
%
%	@param Task list with the task information.
%	@param Id string identifeir of the task to obtain.
%
get_task(Task,Id) :-
	get_task_manager_url_to(Url,['/tasks/',Id]),
	wenet_get_json_from_url(Url,Task),
	asserta(get_task(Task,Id)),
	wenet_log_trace('Loaded task',Task)
	.

%!	get_task_id(-Id,+Task)
%
%	Obtain the id of a task .
%
%	@param Id of the task .
%	@param Task to get the id.
%
get_task_id(Id, json(Task)) :-
	member(id=Id,Task)
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

%!	get_task_app_id(-AppId,+Task)
%
%	Obtain the app identifier of a task .
%
%	@param AppId of the task .
%	@param Task to get the app identifier.
%
get_task_app_id(AppId, json(Task)) :-
	member(appId=AppId,Task)
	.

%!	get_task_community_id(-CommunityId,+Task)
%
%	Obtain the community identifier of a task .
%
%	@param CommunityId of the task .
%	@param Task to get the community identifier.
%
get_task_community_id(CommunityId, json(Task)) :-
	member(communityId=CommunityId,Task)
	.

%!	get_task_requester_id(-RequesterId,+Task)
%
%	Obtain the requester identifier of a task .
%
%	@param RequesterId of the task .
%	@param Task to get the requester identifier.
%
get_task_requester_id(RequesterId, json(Task)) :-
	member(requesterId=RequesterId,Task)
	.

%!	get_task_goal(-Goal,+Task)
%
%	Obtain the goal of a task .
%
%	@param Goal of the task .
%	@param Task to get the goal.
%
get_task_goal(Goal, json(Task)) :-
	member(goal=Goal,Task)
	.

%!	get_task_goal_name(-GoalName,+Task)
%
%	Obtain the goal name of a task .
%
%	@param GoalName of the task .
%	@param Task to get the goal name.
%
get_task_goal_name(GoalName, json(Task)) :-
	get_task_goal(Goal, json(Task)),
	member(name=GoalName,Goal)
	.

%!	get_task_goal_description(-GoalDescription,+Task)
%
%	Obtain the goal description of a task .
%
%	@param GoalDescription of the task .
%	@param Task to get the goal description.
%
get_task_goal_description(GoalDescription, json(Task)) :-
	get_task_goal(Goal, json(Task)),
	member(description=GoalDescription,Goal)
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

%!	get_task_close_ts(+Task)
%
%	Obtain the close time stamp of a task .
%
%	@param CloseTs of the task .
%	@param Task to get the close time stamp.
%
get_task_close_ts(CloseTs, json(Task)) :-
	member(closeTs=CloseTs,Task)
	.

%!	get_task_close_ts(-CloseTs,+Task)
%
%	Obtain the close time stamp of a task .
%
%	@param CloseTs of the task .
%	@param Task to get the close time stamp.
%
get_task_close_ts(CloseTs, json(Task)) :-
	member(closeTs=CloseTs,Task)
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


%!	wenet_task_manager_add_transaction_into_task(-AddedTaskTransaction,+TaskId,+TaskTransaction)
%
%	Add a task transaction into a task.
%
%	@param AddedTaskTransaction return task transaction that has been added into the task.
%	@param TaskId identifier of the task to add the transaction.
%	@param TaskTransaction to add to the task.
%
wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,TaskId,Transaction) :-
	get_task_manager_url_to(Url,['/tasks/',TaskId,'/transactions']),
	wenet_post_json_to_url(AddedTaskTransaction,Url,Transaction)
	.

%!	wenet_task_manager_add_message_into_transaction(-AddedTransactionMessage,+TaskId,+TransactionId,+TransactionMessage)
%
%	Add a message into a transaction.
%
%	@param AddedTransactionMessage return message that has been added into the transaction.
%	@param TaskId identifier of the task where is the transaction to add the message.
%	@param TransactionId identifier of the transaction to add the message.
%	@param TransactionMessage to add to the transaction.
%
wenet_task_manager_add_message_into_transaction(AddedTransactionMessage,TaskId,TransactionId,Message) :-
	get_task_manager_url_to(Url,['/tasks/',TaskId,'/transactions/',TransactionId,'/messages']),
	wenet_post_json_to_url(AddedTransactionMessage,Url,Message)
	.

%!	wenet_task_manager_add_created_transation_to_current_task(-InitialTransaction)
%
%	Add into the current task the transaction a message into a transaction.
%
%	@param InitialTransaction that has been added.
%
wenet_task_manager_add_created_transation_to_current_task(InitialTransaction) :-
	get_task(Task),
	get_task_id(TaskId,Task),
	get_task_requester_id(RequesterId,Task),
	wenet_task_manager_add_transaction_into_task(InitialTransaction,TaskId,json([taskId=TaskId,actioneerId=RequesterId,label='CREATED_TASK']))
	.

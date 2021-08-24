%
% Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
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
	wenet_is_closed_task/1,
	wenet_close_ts_of_task/2,
	wenet_attributes_of_task/2,
	wenet_transactions_of_task/2,
	wenet_id_of_transaction/2,
	wenet_task_id_of_transaction/2,
	wenet_label_of_transaction/2,
	wenet_attributes_of_transaction/2,
	wenet_actioneer_id_of_transaction/2,
	wenet_messages_of_transaction/2
	.

%!	wenet_task_manager_api_url_to(+Url,-Paths)
%
%	Calculate the URL from a path
%
wenet_task_manager_api_url_to(Url,Paths) :-
	wenet_task_manager_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_task_manager_get_task(+Task,-Id)
%
%	Return the task associated to an identifier.
%
%	@param Task list with the task information.
%	@param Id string identifier of the task to obtain.
%
wenet_task_manager_get_task(Task,Id) :-
	wenet_task_manager_api_url_to(Url,['/tasks/',Id]),
	wenet_get_json_from_url(Task,Url)
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

%!	wenet_task_type_id_of_task(-TaskTypeId,+Task)
%
%	Obtain the type identifier of a task .
%
%	@param TaskTypeId of the task .
%	@param Task to get the task type identifier.
%
wenet_task_type_id_of_task(TaskTypeId, json(Task)) :-
	member(taskTypeId=TaskTypeId,Task)
	.

%!	wenet_app_id_of_task(-AppId,+Task)
%
%	Obtain the app identifier of a task.
%
%	@param AppId of a task.
%	@param Task to get the app identifier.
%
wenet_app_id_of_task(AppId, json(Task)) :-
	member(appId=AppId,Task)
	.


%!	wenet_community_id_of_task(-CommunityId,+Task)
%
%	Obtain the community identifier of a task.
%
%	@param CommunityId of a task.
%	@param Task to get the community identifier.
%
wenet_community_id_of_task(CommunityId, json(Task)) :-
	member(communityId=CommunityId,Task)
	.

%!	wenet_requester_id_of_task(-RequesterId,+Task)
%
%	Obtain the requester identifier of a task .
%
%	@param RequesterId of a task .
%	@param Task to get the requester identifier.
%
wenet_requester_id_of_task(RequesterId, json(Task)) :-
	member(requesterId=RequesterId,Task)
	.

%!	wenet_goal_of_task(-Goal,+Task)
%
%	Obtain the goal of a task.
%
%	@param Goal of a task.
%	@param Task to get the goal.
%
wenet_goal_of_task(Goal, json(Task)) :-
	member(goal=Goal,Task)
	.

%!	wenet_goal_name_of_task(-GoalName,+Task)
%
%	Obtain the goal name of a task.
%
%	@param GoalName of a task.
%	@param Task to get the goal name.
%
wenet_goal_name_of_task(GoalName, Task) :-
	wenet_goal_of_task(json(Goal), Task),
	member(name=GoalName,Goal)
	.

%!	wenet_goal_description_of_task(-GoalDescription,+Task)
%
%	Obtain the goal description of a task.
%
%	@param GoalDescription of a task.
%	@param Task to get the goal description.
%
wenet_goal_description_of_task(GoalDescription, Task) :-
	wenet_goal_of_task(json(Goal), Task),
	member(description=GoalDescription,Goal)
	.

%!	wenet_goal_keywords_of_task(-GoalKeywords,+Task)
%
%	Obtain the goal keywords of a task.
%
%	@param GoalKeywords of a task.
%	@param Task to get the goal keywords.
%
wenet_goal_keywords_of_task(GoalKeywords, Task) :-
	wenet_goal_of_task(json(Goal), Task),
	member(keywords=GoalKeywords,Goal)
	.

%!	is_task_closed(+Task)
%
%	This is true if a task is closed.
%
%	@param Task to check if it is closed.
%
wenet_is_closed_task(json(Task)) :-
	member(closeTs = X,Task),
	integer(X)
	.

%!	wenet_close_ts_of_task(+Task)
%
%	Obtain the close time stamp of a task.
%
%	@param CloseTs of a task.
%	@param Task to get the close time stamp.
%
wenet_close_ts_of_task(CloseTs, json(Task)) :-
	member(closeTs=CloseTs,Task)
	.

%!	wenet_attributes_of_task(-Attributes,+Task)
%
%	Obtain the attributes of a task.
%
%	@param Attributes of the task.
%	@param Task to get the attributes.
%
wenet_attributes_of_task(Attributes, json(Task)) :-
	member(attributes=Attributes,Task)
	.

%!	wenet_transactions_of_task(-Transactions,+Task)
%
%	Obtain the transactions of a task.
%
%	@param Transactions of the task.
%	@param Task to get the transactions.
%
wenet_transactions_of_task(Transactions, json(Task)) :-
	member(transactions=Transactions,Task)
	.

%!	wenet_id_of_transaction(-Id,+Transaction)
%
%	Obtain the id of a transaction.
%
%	@param Id of the transaction.
%	@param Transaction to get the id.
%
wenet_id_of_transaction(Id,json(Transaction)) :-
	member(id=Id,Transaction)
	.

%!	wenet_task_id_of_transaction(-TaskId,+Transaction)
%
%	Obtain the task identifier of a transaction.
%
%	@param TsaskId of the transaction.
%	@param Transaction to get the task id.
%
wenet_task_id_of_transaction(TaskId,json(Transaction)) :-
	member(taskId=TaskId,Transaction)
	.

%!	wenet_label_of_transaction(-Label,+Transaction)
%
%	Obtain the label of a transaction.
%
%	@param Label of the transaction.
%	@param Transaction to get the label.
%
wenet_label_of_transaction(Label,json(Transaction)) :-
	member(label=Label,Transaction)
	.

%!	wenet_attributes_of_transaction(-Attributes,+Transaction)
%
%	Obtain the attributes of a transaction.
%
%	@param Attributes of the transaction.
%	@param Transaction to get the attributes.
%
wenet_attributes_of_transaction(Attributes,json(Transaction)) :-
	member(attributes=Attributes,Transaction);Attributes = @(null)
	.

%!	wenet_actioneer_id_of_transaction(-ActioneerId,+Transaction)
%
%	Obtain the actioneer identifier of a transaction.
%
%	@param TsaskId of the transaction.
%	@param Transaction to get the actioneer id.
%
wenet_actioneer_id_of_transaction(ActioneerId,json(Transaction)) :-
	member(actioneerId=ActioneerId,Transaction)
	.

%!	wenet_messages_of_transaction(-Messages,+Transaction)
%
%	Obtain the messages of a transaction.
%
%	@param Messages of the transaction.
%	@param Transaction to get the messages.
%
wenet_messages_of_transaction(Messages,json(Transaction)) :-
	member(messages=Messages,Transaction)
	.

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
	get_task_transaction_label/2,
	get_task_transaction_attributes/2,
	get_task_transaction_actioneer_id/2,
	get_task_transaction_task_id/2,
	get_task_transaction_id/2,
	get_task_transaction_messages/2,
	wenet_task_manager_add_transaction_into_task/2,
	wenet_task_manager_add_transaction_into_task/3
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


%!	wenet_task_manager_add_transaction_into_task(-AddedTaskTransaction,+TaskTransaction)
%
%	Add a task transaction into a task.
%
%	@param AddedTaskTransaction return task transaction that has been added into the task.
%	@param TaskTransaction to add to the task.
%
wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,Transaction) :-
	get_task_transaction_task_id(TaskId,Transaction),
	wenet_task_manager_add_transaction_into_task(AddedTaskTransaction,TaskId,Transaction)
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

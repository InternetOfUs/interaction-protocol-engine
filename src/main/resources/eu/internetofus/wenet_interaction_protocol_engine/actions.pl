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

%
% Contains the high level actions that can be used on the norms.
%

:- dynamic
	add_created_transaction/0,
	add_message_transaction/0,
	new_user_message/3,
	send_user_message/2,
	put_task_attribute/2,
	send_messages/3,
	send_message/3,
	notify_incentive_server/2,
	notify_volunteers_to_social_context_builder/2,
	close_task/0,
	merge_task/1
	.


%!	add_created_transaction()
%
%	Add the first transaction to a created task.
%
add_created_transaction() :-
	!,
	get_profile_id(ProfileId),
	get_task_id(TaskId),
	Transaction = json([taskId=TaskId,actioneerId=ProfileId,label='CREATE_TASK']),
	!,
	ignore(wenet_task_manager_add_transaction_into_task(InitialTransaction,TaskId,Transaction)),
	wenet_id_of_transaction(InitialTransactionId,InitialTransaction),
	!,
	asserta(add_created_transaction()),
	asserta(get_transaction(InitialTransaction)),
	asserta(get_transaction_id(InitialTransactionId))
	.

%!	add_message_transaction()
%
%	Add the transaction of the message into the task.
%
add_message_transaction() :-
	!,
	get_message(Message),
	wenet_content_of_protocol_message(Transaction,Message),
	get_task_id(TaskId),
	!,
	ignore(wenet_task_manager_add_transaction_into_task(AddedTransaction,TaskId,Transaction)),
	wenet_id_of_transaction(AddedTransactionId,AddedTransaction),
	!,
	asserta(add_message_transaction()),
	asserta(get_transaction(AddedTransaction)),
	asserta(get_transaction_id(AddedTransactionId))
	.

%!	new_user_message(-Message,+Label,+Content)
%
%	Create a message to send into an user.
%
%	@param Message for the user.
%	@param Label of the message.
%	@param Content of the message.
%
new_user_message(Message,Label,Content) :-
	get_app_id(AppId),
	get_profile_id(ReceiverId),
	wenet_new_message(Message,AppId,ReceiverId,Label,Content)
	.

%!	send_user_message(+Label,+Content)
%
%	Send a message to the user associated to the interaction protocol engine.
%
%	@param Label of the message.
%	@param Content of the message.
%
send_user_message(Label,Content) :-
	new_user_message(Message,Label,Content),
	get_app_message_callback_url(Url),
	atom_json_term(AtomBody, Message, []),
	Data = atom('application/json',AtomBody),
	Options = [request_header('Content-Type'='application/json')],
	catch(http_post(Url,Data,Result,Options),Result,true),
	!,
	wenet_log_trace('POST CALLBACK',[Url,AtomBody,Result])
	,
	(
		(
			get_task_id(TaskId),
			get_transaction_id(TransactionId)
		)
		->ignore(wenet_task_manager_add_message_into_transaction(_,TaskId,TransactionId,Message))
		;true
	),
	!
	.

%!	get_task_attribute(-Key,-Value)
%
%	Change the value of a task attribute.
%
%	@param Key name of the attribute to put.
%	@param Value of the attribute.
%
put_task_attribute(Key,Value) :-
	merge_task(json([attributes=json([Key=Value])]))
	.

%!	send_messages(+Users,+Particle,+Content)
%
%	Send a new message to the interaction protocol engine of each specified users.
%
%	@param Users to receive the message.
%	@param Particle of the message.
%	@param Content of the message.
%
send_messages([],_,_).
send_messages([User|Tail],Particle,Content) :-
 	send_message(User,Particle,Content),
	send_messages(Tail,Particle,Content)
	.

%!	send_message(+ReceiverUserId,+Particle,+Content)
%
%	Send a message to the interaction protocol engine of an user.
%
%	@param ReceiverUserId to receive the message.
%	@param Particle of the message.
%	@param Content of the message.
%
send_message(ReceiverUserId,Particle,@(null)) :-
	send_message(ReceiverUserId,Particle,json([]))
	.
send_message(ReceiverUserId,Particle,Content) :-
	is_list(Content),
	send_message(ReceiverUserId,Particle,json(Content))
	.
send_message(ReceiverUserId,Particle,Content) :-
	get_profile_id(SenderUserId),
	(
		get_app_id(AppId)
		; AppId = @(null)
	),
	(
		get_community_id(CommunityId)
		; CommunityId = @(null)
	),
	(
		get_task_id(TaskId)
		; TaskId = @(null)
	),
	(
		get_transaction_id(TransactionId)
		; TransactionId = @(null)
	),
	wenet_new_protocol_message(Message,AppId,CommunityId,TaskId,TransactionId,'INTERACTION_PROTOCOL_ENGINE',SenderUserId,'INTERACTION_PROTOCOL_ENGINE',ReceiverUserId,Particle,Content),
	!,
	ignore(wenet_interaction_protocol_engine_send_message(_,Message))
	.

%!	send_message(+Action,+Message)
%
%	Send a message to the interaction protocol engine of an user.
%
%	@param Action to receive the message.
%	@param Message .
%
notify_incentive_server(Action,Message) :-
	get_app_id(AppId),
	get_profile_id(UserId),
	get_community_id(CommunityId),
	get_task_id(TaskId),
	wenet_new_task_status(Status,AppId,UserId,CommunityId,TaskId,Action,Message),
	!,
	ignore(wenet_incentive_server_update_task_status(_,Status))
	.

%!	notify_social_context_builder
%
%	Notify the social context builder about the user preferences in a task.
%
%	@param Volunteers list of volunteers.
%	@param UserId identifier of the users.
%
notify_volunteers_to_social_context_builder(Volunteers,UserId):-
	get_task_id(TaskId),
	!,
	ignore(wenet_social_context_builder_update_preferences(UserId,TaskId,Volunteers))
	.

%!	close_task()
%
%	Mark the current task as closed.
%
close_task() :-
	!,
	get_now(Now),
	merge_task(json([closeTs=Now]))
	.

%!	merge_task()
%
%	Merge the data with the current task.
%
%	@param Task list/json to merge
%
merge_task(Task) :-
	is_list(Task),
	merge_task(json(Task))
	.
merge_task(json(Task)) :-
	get_task_id(TaskId),
	!,
	wenet_task_manager_merge_task(MergedTask,TaskId,json(Task))
		->
			(
				retractall(get_task(_)),
				asserta(get_task(MergedTask))
			)
		;
			true
	.

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

%
% Contains the high level actions that can be used on the norms.
%

:- dynamic
	add_created_transaction/0,
	add_message_transaction/0,
	add_transaction/1,
	new_user_message/3,
	send_user_message/2,
	put_task_attribute/2,
	send_messages/3,
	send_message/3,
	notify_incentive_server_transaction_done/1,
	notify_incentive_server_transaction_done/2,
	notify_incentive_server_message_sent/1,
	notify_incentive_server_message_sent/2,
	notify_incentive_server_task_created/0,
	notify_incentive_server_task_created/1,
	notify_incentive_server_task_created/2,
	close_task/0,
	merge_task/1,
	merge_community_state/1,
	put_community_state_attribute/2,
	send_event/4,
	volunteers_ranking/2,
	answers_ranking/2,
	notify_social_context_builder_message_sent/1,
	merge_task_state/1,
	put_task_state_attribute/2,
	notify_message_interaction/1,
	selected_answer_from_last_ranking/1
	.


%!	add_created_transaction()
%
%	Add the first transaction to a created task.
%
add_created_transaction() :-
	!,
	get_profile_id(ProfileId),
	get_task_id(TaskId),
	InitialTransaction = json([taskId=TaskId,actioneerId=ProfileId,label='CREATE_TASK']),
	ignore(wenet_task_manager_add_transaction_into_task(AddedTransaction,TaskId,InitialTransaction)),
	wenet_id_of_transaction(AddedTransactionId,AddedTransaction),
	ignore(notify_incentive_server_task_created()),
	!,
	asserta(get_transaction(AddedTransaction)),
	asserta(get_transaction_id(AddedTransactionId)),
	asserta(add_created_transaction())
	.

%!	notify_incentive_server_task_created(+Label,+Count)
%
%	Notify the incentive server that a task of the specified task type is
%	created Count times.
%
%	@param TaskTypeId the identifier of the task type of the created task.
%	@param Count of the task types that has bene created.
%
notify_incentive_server_task_created(TaskTypeId,Count) :-
	get_profile_id(ProfileId),
	get_community_id(CommunityId),
	get_app_id(AppId),
	wenet_new_task_type_status(Status,ProfileId,CommunityId,AppId,TaskTypeId,Count),
	!,
	ignore(wenet_incentive_server_update_task_type_status(_,Status))
	.

%!	notify_incentive_server_task_created(+TaskTypeId)
%
%	Notify the incentive server that a task of the specific task type is created.
%   The number of times is counted as a community user property per user and task type.
%
%	@param TaskTypeId the identifier of the task type of the created task.
%
notify_incentive_server_task_created(TaskTypeId) :-
	atomics_to_string(["incentiveServer",TaskTypeId],'#',Key),
	atom_string(AtomKey,Key),
	get_community_state_attribute(Count,AtomKey,0),
	wenet_math(NewCount,Count + 1),
	put_community_state_attribute(Key,NewCount),
	ignore(notify_incentive_server_task_created(TaskTypeId,NewCount))
	.

%!	notify_incentive_server_task_created()
%
%	Notify the incentive server that the current task is created.
%   The number of times is counted as a community user property per user.
%
notify_incentive_server_task_created() :-
	get_task_type_id(TaskTypeId),
	ignore(notify_incentive_server_task_created(TaskTypeId))
	.

%!	add_message_transaction()
%
%	Add the transaction of the message into the task.
% 
add_message_transaction() :-
	!,
	get_message(Message),
	wenet_content_of_protocol_message(Transaction,Message), 
	ignore(add_transaction(Transaction)),
	!,
	asserta(add_message_transaction())
	.

%!	add_transaction(Transaction)
%
%	Add a transaction into the current task.
%
%	@param transaction to add to the task.
%
add_transaction(Transaction) :-
	get_task_id(TaskId),
	ignore(wenet_task_manager_add_transaction_into_task(AddedTransaction,TaskId,Transaction)),
	wenet_id_of_transaction(AddedTransactionId,AddedTransaction),
	wenet_label_of_transaction(Label,AddedTransaction),
	ignore(notify_incentive_server_transaction_done(Label)),
	!,
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
%	Also this informs of the icentive server that an action is doen on the task.
%	The name of the action is the label of the message and the count is increassed
%	by the community property associated top the task type and the label.
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
	wenet_log_trace('POST CALLBACK',[Url,AtomBody,Result]),
	(
		(
			get_task_id(TaskId),
			get_transaction_id(TransactionId)
		)
		->ignore(wenet_task_manager_add_message_into_transaction(_,TaskId,TransactionId,Message))
		;true
	),
	ignore(notify_incentive_server_message_sent(Label)),
	ignore(notify_social_context_builder_message_sent(Message)),
	ignore(notify_message_interaction(Message)),
	!
	.

%!	put_task_attribute(+Key,+Value)
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

%!	notify_incentive_server_transaction_done(+Label,+Count)
%
%	Notify the incentive server that a transaction is done Count times in a task.
%
%	@param Label of the transaction that has been done.
%	@param Count the number of times the user has done the transaction on the community for the current task type.
%
notify_incentive_server_transaction_done(Label,Count) :-
	get_profile_id(UserId),
	get_community_id(CommunityId),
	get_app_id(AppId),
	get_task_type_id(TaskTypeId),
	wenet_new_task_transaction_status(Status,UserId,CommunityId,AppId,TaskTypeId,Label,Count),
	!,
	ignore(wenet_incentive_server_update_task_transaction_status(_,Status))
	.

%!	notify_incentive_server_transaction_done(+Label)
%
%	Notify the incentive server that a transaction has been done in a task.
%   The number of times is counted as a community user property per user and task type.
%
%	@param Label that has changed the task.
%
notify_incentive_server_transaction_done(Label) :-
	get_task_type_id(TaskTypeId),
	atomics_to_string(["incentiveServer",TaskTypeId,Label],'#',Key),
	atom_string(AtomKey,Key),
	get_community_state_attribute(Count,AtomKey,0),
	wenet_math(NewCount,Count + 1),
	put_community_state_attribute(Key,NewCount),
	ignore(notify_incentive_server_transaction_done(Label,NewCount))
	.

%!	notify_incentive_server_message_sent(+Label,+Count)
%
%	Notify the incentive server that a message is sent Count times in a task.
%
%	@param Label that has changed the task.
%	@param Count of the action.
%
notify_incentive_server_message_sent(Label,Count) :-
	notify_incentive_server_transaction_done(Label,Count)
	.

%!	notify_incentive_server_message_sent(+Label)
%
%	Notify the incentive server that a message has been sent to a user in a task.
%   The number of times is counted as a community user property per user and task type.
%
%	@param Label that has changed the task.
%
notify_incentive_server_message_sent(Label) :-
	notify_incentive_server_transaction_done(Label)
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

%!	merge_community_state(-CommunityState)
%
%	Merge the data with the current community user state.
%
%	@param CommunityState list/json to merge
%
merge_community_state(CommunityState) :-
	is_list(CommunityState),
	merge_community_state(json(CommunityState))
	.
merge_community_state(json(CommunityState)) :-
	get_profile_id(ProfileId),
	get_community_id(CommunityId),
	!,
	wenet_interaction_protocol_engine_merge_community_user_state(MergedCommunityUserState,CommunityId,ProfileId,json(CommunityState))
		->
			(
				retractall(get_community_state(_)),
				asserta(get_community_state(MergedCommunityUserState))
			)
		;
			true
	.

%!	put_community_state_attribute(+Key,+Value)
%
%	Change the value of a task attribute.
%
%	@param Key name of the attribute to put.
%	@param Value of the attribute.
%
put_community_state_attribute(Key,Value) :-
	merge_community_state(json([attributes=json([Key=Value])]))
	.


%!	send_event(-Id,+Delay,+Particle,+Content)
%
%	Send an event.
%
%	@param Id identifier of the send event.
%	@param Dealy seconds to wait before to send the event.
%	@param Particle of the event.
%	@param Content of the event.
%
send_event(Id,Delay,Particle,@(null)) :-
	send_event(Id,Delay,Particle,json([]))
	.
send_event(Id,Delay,Particle,Content) :-
	is_list(Content),
	send_event(Id,Delay,Particle,json(Content))
	.
send_event(Id,Delay,Particle,Content) :-
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
	wenet_new_protocol_event(Event,AppId,CommunityId,TaskId,TransactionId,SenderUserId,Delay,Particle,Content),
	!,
	wenet_interaction_protocol_engine_send_event(Sent,Event)->wenet_id_of_protocol_event(Id,Sent);Id = -1
	.

%!	volunteers_ranking(-Ranking,+Volunteers)
%
%	This action calls the social context builder to obtain a ranking for some volunteers.
%
%	@param Ranking array of strings with the ranked answers.
%	@param Volunteers array of strings with the user identifiers that has volunteer.
%
volunteers_ranking(Ranking,Volunteers):-
	get_profile_id(Me),
	get_task_id(TaskId),
	!,
	ignore(wenet_social_context_builder_post_preferences(Ranking,Me,TaskId,Volunteers))
	.

%!	answers_ranking(-Ranking,+Answers)
%
%	This action calls the social context builder to obtain a ranking for some answers.
%
%	@param Ranking array of strings with the ranked answers.
%	@param UserAnswers array of JSON models with the user answers. This array elements
%						can be created using the wenet_new_user_answer.
%
answers_ranking(Ranking,UserAnswers):-
	get_profile_id(Me),
	get_task_id(TaskId),
	!,
	ignore(wenet_social_context_builder_post_preferences_answers(Ranking,Me,TaskId,UserAnswers)),
	ignore(put_task_state_attribute('social_context_builder_ranking',Ranking))
	.

%!	selected_answer_from_last_ranking(+UserAnswer)
%
%	This action update the  the social context builder to obtain a ranking for some answers.
%
%	@param UserAnswer the selected answer.
%	@param UserAnswers array of JSON models with the user answers. This array elements
%						can be created using the wenet_new_user_answer.
%
selected_answer_from_last_ranking(UserAnswer):-
	!,
	ignore(
		(
			get_profile_id(UserId),
			get_task_id(TaskId),
			get_task_state_attribute(Ranking,'social_context_builder_ranking'),
			(
				nth0(Selected,Ranking,UserAnswer)
				-> wenet_social_context_builder_put_preferences_answers_update(UserId,TaskId,Selected,Ranking)
				; 	(
					length(Ranking,Selected),
					wenet_add(NewRanking,UserAnswer,Ranking),
					wenet_social_context_builder_put_preferences_answers_update(UserId,TaskId,Selected,NewRanking)
				)  
			)
		
		)
	)
	.

%!	notify_social_context_builder_message_sent(+Message)
%
%	Notify the social context builder about an interaction between users.
%
%	@param Message that has been sent.
%
notify_social_context_builder_message_sent(Message) :-
	!,
	(
		get_task_id(TaskId),
		get_transaction_id(TransactionId),
		get_now(Timestamp),
		get_transaction(Transaction),
		wenet_actioneer_id_of_transaction(SenderId,Transaction),
		wenet_new_user_message(UserMessage,TaskId,TransactionId,Timestamp,SenderId,Message)		
	)->
	wenet_social_context_builder_post_social_notification(UserMessage)
	; true
	.

%!	merge_task_state(-TaskState)
%
%	Merge the data with the current task user state.
%
%	@param TaskState list/json to merge
%
merge_task_state(TaskState) :-
	is_list(TaskState),
	merge_task_state(json(TaskState))
	.
merge_task_state(json(TaskState)) :-
	get_profile_id(ProfileId),
	get_task_id(TaskId),
	!,
	wenet_interaction_protocol_engine_merge_task_user_state(MergedTaskUserState,TaskId,ProfileId,json(TaskState))
		->
			(
				retractall(get_task_state(_)),
				asserta(get_task_state(MergedTaskUserState))
			)
		;
			true
	.

%!	put_task_state_attribute(+Key,+Value)
%
%	Change the value of a task attribute.
%
%	@param Key name of the attribute to put.
%	@param Value of the attribute.
%
put_task_state_attribute(Key,Value) :-
	merge_task_state(json([attributes=json([Key=Value])]))
	.

%!	notify_message_interaction(+Message)
%
%	Notfy that has been done a message interaction.
%
%	@param Message of the interaction.
% 
notify_message_interaction(Message) :-
	(
		get_app_id(AppId)
		; AppId = @(null)
	),
	(
		get_community_id(CommunityId)
		; CommunityId = @(null)
	),
	(
		get_task_type_id(TaskTypeId)
		; TaskTypeId = @(null)
	),
	(
		get_task_id(TaskId)
		; TaskId = @(null)
	),
	(
		(
			get_transaction(Transaction),
			(
				wenet_actioneer_id_of_transaction(SenderId,Transaction)
				; SenderId = @(null)
			),
			(
				wenet_label_of_transaction(TransactionLabel,Transaction)
				; TransactionLabel = @(null)
			),
			(
				wenet_attributes_of_transaction(TransactionAttributes,Transaction)
				; TransactionAttributes = @(null)
			),
			(
				wenet_creation_ts_of_transaction(TransactionTs,Transaction)
				; TransactionTs = @(null)
			)
		)
		; SenderId = @(null),TransactionLabel = @(null),TransactionAttributes = @(null), TransactionTs =  @(null) 
	),
	(
		(
			wenet_is_json_null(Message),
			ReceiverId = @(null),
			MessageLabel = @(null),
			MessageAttributes = @(null), 
			MessageTs =  @(null)
		)
		;
		(
			(
				wenet_receiver_id_of_message(ReceiverId,Message)
				; ReceiverId = @(null)
			),
			(
				wenet_label_of_message(MessageLabel,Message)
				; MessageLabel = @(null)
			),
			(
				wenet_attributes_of_message(MessageAttributes,Message)
				; MessageAttributes = @(null)
			),
			get_now(MessageTs)			
		)
	),
	wenet_new_interaction(Interaction,AppId,CommunityId,TaskTypeId,TaskId,SenderId,ReceiverId,TransactionLabel,TransactionAttributes,TransactionTs,MessageLabel,MessageAttributes,MessageTs),
	ignore(wenet_interaction_protocol_engine_add_interaction(Interaction))
	.

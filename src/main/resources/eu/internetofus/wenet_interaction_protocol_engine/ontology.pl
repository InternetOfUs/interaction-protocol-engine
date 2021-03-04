%
% Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
%
% Permission = hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE = PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.
%

:- dynamic
	is_now_less_than/1,
	is_now_less_than_or_equal_to/1,
	is_now_greater_than/1,
	is_now_greater_than_or_equal_to/1,
	is_now_equal_to/1,
	is_received_do_transaction/1,
	is_received_do_transaction_with_label/2,
	is_received_created_task/0,
	is_received_send_incentive/1,
	is_received_message_from_another_iteraction_protocol_engine_with_particle/2,
	env_task/1,
	env_task_id/1,
	env_community/1,
	env_community_id/1,
	env_app/1,
	env_app_id/1,
	env_transaction/1,
	env_transaction_id/1,
	env_profile/1,
	env_profile_id/1
	.

%!	is_now_less_than(+Time)
%
%	Check if the wenet time is less than a time.
%
%	@param Actions to execute.
%
is_now_less_than(Time) :-
	wenet_now(Now),
	<(Now,Time)
	.

%!	is_now_less_than_or_equal_to(+Time)
%
%	Check if the wenet time is less than or equal to a time.
%
%	@param Actions to execute.
%
is_now_less_than_or_equal_to(Time) :-
	wenet_now(Now),
	=<(Now,Time)
	.

%!	is_now_greater_than(+Time)
%
%	Check if the wenet time is greater than a time.
%
%	@param Actions to execute.
%
is_now_greater_than(Time) :-
	wenet_now(Now),
	>(Now,Time)
	.

%!	is_now_greater_than_or_equal_to(+Time)
%
%	Check if the wenet time is greater than or equal to a time.
%
%	@param Actions to execute.
%
is_now_greater_than_or_equal_to(Time) :-
	wenet_now(Now),
	>=(Now,Time)
	.

%!	is_now_equal_to(+Time)
%
%	Check if the wenet time is equal to a time.
%
%	@param Actions to execute.
%
is_now_equal_to(Time) :-
	wenet_now(Now),
	=(Now,Time)
	.

%!	is_received_do_transaction(-Transaction)
%
%	Check if received a do transaction from the user.
%
%	@param Transaction that has to do.
%
is_received_do_transaction(Transaction) :-
	env_message(Message),
	get_protocol_message_particle('doTaskTransaction',Message),
	get_protocol_message_sender(Sender,Message),
	get_protocol_address_component('USER_APP',Sender),
	get_protocol_address_user_id(UserId,Sender),
	get_protocol_message_receiver(Receiver,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Receiver),
	get_protocol_address_user_id(UserId,Receiver),
	get_protocol_message_content(Transaction,Message)
	.

%!	is_received_do_transaction_with_label(-Transaction,+Label)
%
%	Check if received a do transaction from the user.
%
%	@param Transaction that has to do.
%	@param Label for the received transaction.
%
is_received_do_transaction_with_label(Transaction,Label) :-
	is_received_do_transaction(Transaction),
	get_task_transaction_label(Label,Transaction)
	.

%!	is_received_created_task()
%
%	Check if received a created task from the user.
%
is_received_created_task() :-
	env_message(Message),
	get_protocol_message_particle('createdTask',Message),
	get_protocol_message_sender(Sender,Message),
	get_protocol_address_component('USER_APP',Sender),
	get_protocol_address_user_id(UserId,Sender),
	get_protocol_message_receiver(Receiver,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Receiver),
	get_protocol_address_user_id(UserId,Receiver)
	.

%!	is_received_send_incentive(-Incentive)
%
%	Check if the incentive server try to send an incentive to an user.
%
%	@param Incentive to send to the user.
%
is_received_send_incentive(Incentive) :-
	env_message(Message),
	get_protocol_message_particle('sendIncentive',Message),
	get_protocol_message_sender(Sender,Message),
	get_protocol_address_component('INCENTIVE_SERVER',Sender),
	get_protocol_message_receiver(Receiver,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Receiver),
	get_protocol_message_content(Incentive,Message)
	.

%!	is_received_message_from_another_iteraction_protocol_engine_with_particle(-Content,+Particle)
%
%	Check if received a do transaction from the user.
%
%	@param Content of the received message.
%	@param Particle of the received message.
%
is_received_message_from_another_iteraction_protocol_engine_with_particle(Message,Particle) :-
	env_message(Message),
	get_protocol_message_particle(Particle,Message),
	get_protocol_message_sender(Sender,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Sender),
	get_protocol_message_receiver(Receiver,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Receiver)
	.

%!	env_task_id(-TaskId)
%
%	Return the identifier of the task associated to the task that is involved in the protocol.
%
%	@param TaskId the identifier of the task.
%
env_task_id(TaskId) :-
	env_message(Message),
	get_protocol_message_task_id(TaskId,Message),
	asserta(env_task_id(TaskId)).

env_task_id(TaskId) :-
	env_task(Task),
	get_task_id(TaskId,Task),
	asserta(env_task_id(TaskId)).

env_task_id(@(null)).


%!	env_task_type_id(-TaskTypeId)
%
%	Return the identifier of the task type associated to the task in the protocol.
%
%	@param task_typeId the identifier of the task_type.
%
env_task_type_id(TaskTypeId) :-
	env_task(Task),
	get_task_type_id(TaskTypeId,Task),
	asserta(env_task_type_id(TaskTypeId)).

env_task_type_id(TaskTypeId) :-
	env_task_type(TaskType),
	member(id=TaskTypeId,TaskType),
	asserta(env_task_type_id(TaskTypeId)).

env_task_type_id(@(null)).


%!	env_community_id(-CommunityId)
%
%	Return the identifier of the community associated to the community that is involved in the protocol.
%
%	@param CommunityId the identifier of the community.
%
env_community_id(CommunityId) :-
	env_message(Message),
	get_protocol_message_community_id(CommunityId,Message),
	asserta(env_community_id(CommunityId)).

env_community_id(CommunityId) :-
	env_community(Community),
	get_community_id(CommunityId,Community),
	asserta(env_community_id(CommunityId)).

env_community_id(CommunityId) :-
	env_task(Task),
	get_task_community_id(CommunityId,Task),
	asserta(env_community_id(CommunityId)).

env_community_id(@(null)).


%!	env_app_id(-AppId)
%
%	Return the identifier of the app associated to the app that is involved in the protocol.
%
%	@param AppId the identifier of the app.
%
env_app_id(AppId) :-
	env_message(Message),
	get_protocol_message_app_id(AppId,Message),
	asserta(env_app_id(AppId)).

env_app_id(AppId) :-
	env_app(App),
	get_app_id(AppId,App),
	asserta(env_app_id(AppId)).

env_app_id(AppId) :-
	env_task(Task),
	get_task_app_id(AppId,Task),
	asserta(env_app_id(AppId)).

env_app_id(@(null)).


%!	env_transaction_id(-TransactionId)
%
%	Return the identifier of the transaction associated to the transaction that is involved in the protocol.
%
%	@param TransactionId the identifier of the transaction.
%
env_transaction_id(TransactionId) :-
	env_message(Message),
	get_protocol_message_transaction_id(TransactionId,Message),
	asserta(env_transaction_id(TransactionId)).

env_transaction_id(TransactionId) :-
	env_transaction(Transaction),
	get_task_transaction_id(TransactionId,Transaction),
	asserta(env_transaction_id(TransactionId)).

env_transaction_id(@(null)).


%!	env_profile_id(-ProfileId)
%
%	Return the identifier of the profile associated to the profile that is involved in the protocol.
%
%	@param ProfileId the identifier of the profile.
%
env_profile_id(ProfileId) :-
	env_message(Message),
	get_protocol_message_receiver(Receiver,Message),
	get_protocol_address_user_id(ProfileId,Receiver),
	asserta(env_profile_id(ProfileId)).

env_profile_id(ProfileId) :-
	env_profile(Profile),
	get_profile_id(ProfileId,Profile),
	asserta(env_profile_id(ProfileId)).

env_profile_id(@(null)).
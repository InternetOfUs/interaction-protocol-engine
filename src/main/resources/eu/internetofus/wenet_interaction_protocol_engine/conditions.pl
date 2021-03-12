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
% Contains the high level conditions that can be used on the norms.
%

:- dynamic
	is_now_less_than/1,
	is_now_less_than_or_equal_to/1,
	is_now_greater_than/1,
	is_now_greater_than_or_equal_to/1,
	is_now_equal_to/1,
	get_message/1,
	get_profile/1,
	get_profile_id/1,
	get_community/1,
	get_community_id/1,
	get_task/1,
	get_task_id/1,
	get_transaction/1,
	get_transaction_id/1,
	get_app/1,
	get_app_id/1,
	get_app_message_callback_url/1,
	get_app_users/1,
	get_app_users_except_me/1,
	is_received_do_transaction/2
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

%!	get_profile(-Profile)
%
%	Return the current profile of the user that is checking the norms.
%
%	@param Profile json definition of the profile.
%
get_profile(Profile) :-
	get_profile_id(ProfileId),
	wenet_profile_manager_get_profile(Profile,ProfileId),
	!,
	asserta(get_profile(Profile))
	.
get_profile(_) :-
	wenet_log_error('No profile defined.'),
	backtrace(100),
	throw(error('No profile defined'))
	.

%!	get_profile_id(-ProfileId)
%
%	Return the current profile identifier of the user that is checking the norms.
%
%	@param ProfileIs string with the user identifier.
%
get_profile_id(ProfileId) :-
	get_message(Message),
	wenet_receiver_id_of_protocol_message(ProfileId,Message),
	!,
	asserta(get_profile_id(ProfileId))
	.

%!	get_community(-Community)
%
%	Return the current community of the user that is checking the norms.
%
%	@param Community json definition of the community.
%
get_community(Community) :-
	get_community_id(CommunityId),
	wenet_profile_manager_get_community(Community,CommunityId),
	!,
	asserta(get_community(Community))
	.
get_community(_) :-
	wenet_log_error('No community defined.'),
	backtrace(100),
	throw(error('No community defined'))
	.

%!	get_community_id(-CommunityId)
%
%	Return the current community identifier of the user that is checking the norms.
%
%	@param CommunityIs string with the user identifier.
%
get_community_id(CommunityId) :-
	get_message(Message),
	wenet_community_id_of_protocol_message(CommunityId,Message),
	!,
	asserta(get_community_id(CommunityId))
	.

%!	get_task(-Task)
%
%	Return the current task of the user that is checking the norms.
%
%	@param Task json definition of the task.
%
get_task(Task) :-
	get_task_id(TaskId),
	wenet_task_manager_get_task(Task,TaskId),
	!,
	asserta(get_task(Task))
	.
get_task(_) :-
	wenet_log_error('No task defined.'),
	backtrace(100),
	throw(error('No task defined'))
	.


%!	get_task_id(-TaskId)
%
%	Return the current task identifier defined on the norm engine.
%
%	@param TaskIs string with the user identifier.
%
get_task_id(TaskId) :-
	get_message(Message),
	wenet_task_id_of_protocol_message(TaskId,Message),
	!,
	asserta(get_task_id(TaskId))
	.

%!	get_transaction(-Transaction)
%
%	Return the current transaction defined on the norm engine.
%
%	@param Transaction json transaction on the norm engine.
%
get_transaction(json(Transaction)) :-
	get_transaction_id(TransactionId),
	get_task(Task),
	wenet_transaction_of_task(Transactions,Task),
	member(json(Transaction),Transactions),
	member(id=TransactionId,Transaction),
	!,
	asserta(get_transaction(json(Transaction)))
	.
get_transaction(_) :-
	wenet_log_error('No transaction defined.'),
	backtrace(100),
	throw(error('No transaction defined'))
	.

%!	get_transaction_id(-TransactionId)
%
%	Return the current transaction identifier defined on the norm engine.
%
%	@param TransactionIs string with the user identifier.
%
get_transaction_id(TransactionId) :-
	get_message(Message),
	wenet_transaction_id_of_protocol_message(TransactionId,Message),
	!,
	asserta(get_transaction_id(TransactionId))
	.

%!	get_app(-App)
%
%	Return the current app of the user that is checking the norms.
%
%	@param App json definition of the app.
%
get_app(App) :-
	get_app_id(AppId),
	wenet_service_get_app(App,AppId),
	!,
	asserta(get_app(App))
	.
get_app(_) :-
	wenet_log_error('No application defined.'),
	backtrace(100),
	throw(error('No application defined'))
	.

%!	get_app_id(-AppId)
%
%	Return the current app identifier of the user that is checking the norms.
%
%	@param AppIs string with the user identifier.
%
get_app_id(AppId) :-
	get_message(Message),
	wenet_app_id_of_protocol_message(AppId,Message),
	!,
	asserta(get_app_id(AppId))
	.

%!	get_app_message_callback_url(-AppId)
%
%	Return the URL to post the callbacks to the user.
%
%	@param Url string with the URL to post the callback messages.
%
get_app_message_callback_url(Url) :-
	get_app(App),
	wenet_message_callback_url_of_app(Url,App),
	!,
	asserta(get_app_message_callback_url(Url))
	.

%!	get_app_users(-Users)
%
%	Return the users of the application.
%
%	@param Users list of string with the identifiers of the application users.
%
get_app_users(Users) :-
	get_app_id(AppId),
	wenet_service_get_app_users(Users,AppId),
	!,
	asserta(get_app_users(Users))
	.


%!	get_app_users_except_me(+Users)
%
%	Return the users of an application except the current user.
%
%	@param Users list of string with the user identifiers of the application
%		except the user that the norms engine represents.
%
get_app_users_except_me(Users) :-
	get_app_users(AppUsers),
	get_profile_id(ProfileId),
	delete(AppUsers,ProfileId,Users),
	!,
	asserta(get_app_users_except_me(Users))
	.

%!	is_received_created_task()
%
%	Check if received that a task is created.
%
is_received_created_task() :-
	get_message(Message),
	wenet_particle_of_protocol_message('createdTask',Message),
	wenet_sender_component_of_protocol_message('TASK_MANAGER',Message),
	wenet_sender_id_of_protocol_message(UserId,Message),
	wenet_receiver_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_receiver_id_of_protocol_message(UserId,Message)
	.

%!	is_received_do_transaction(-Label,-Attributes)
%
%	Check if received a do transaction from the user.
%
%	@param Label of the transaction.
%	@param Attributes of the transaction.
%
is_received_do_transaction(Label,Attributes) :-
	get_message(Message),
	wenet_particle_of_protocol_message('doTaskTransaction',Message),
	wenet_sender_component_of_protocol_message('USER_APP',Message),
	wenet_sender_id_of_protocol_message(UserId,Message),
	wenet_receiver_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_receiver_id_of_protocol_message(UserId,Message),
	wenet_content_of_protocol_message(Transaction,Message),
	wenet_label_of_transaction(Label,Transaction),
	wenet_attributes_of_transaction(Attributes,Transaction)
	.

%!	is_received_send_incentive(-Incentive)
%
%	Check if received a send incentive message.
%
%	@param Incentive that has been received.
%
is_received_send_incentive(Incentive) :-
	get_message(Message),
	wenet_particle_of_protocol_message('sendIncentive',Message),
	wenet_sender_component_of_protocol_message('INCENTIVE_SERVER',Message),
	wenet_receiver_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_content_of_protocol_message(Incentive,Message)
	.

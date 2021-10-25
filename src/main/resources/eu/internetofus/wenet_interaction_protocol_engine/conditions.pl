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
% Contains the high level conditions that can be used on the norms.
%

:- use_module(library(apply)).

:- dynamic
	get_now/1,
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
	get_task_type/1,
	get_task_type_id/1,
	get_transaction/1,
	get_transaction/2,
	get_transaction_id/1,
	get_app/1,
	get_app_id/1,
	get_app_message_callback_url/1,
	get_app_users/1,
	get_app_users_except_me/1,
	is_received_do_transaction/2,
	is_received_created_task/0,
	get_task_attribute_value/2,
	is_received_send_incentive/1,
	is_received/3,
	get_task_goal_name/1,
	get_task_requester_id/1,
	get_social_explanation/2,
	is_task_closed/0,
	get_community_state/1,
	get_community_state_attribute/2,
	get_community_state_attribute/3,
	get_attribute/4,
	get_attribute/3,
	get_closest_users_to_me/1,
	get_closest_users_to_me/2,
	get_app_users_near_me/3,
	is_received_event/2,
	get_task_state/1,
	get_task_state_attribute/2,
	get_task_state_attribute/3,
	get_user_state/1,
	get_user_state_attribute/2,
	get_user_state_attribute/3,
	filter_transactions/3,
	filter_transactions_/4,
	normalized_closeness/3,
	normalized_closeness_/5,
	normalized_social_closeness/2,
	normalized_social_closeness_/4,
	normalized_diversity/3,
	normalized_diversity_/4,
	my_profile_attributes_similars_to/3
	.

%!	is_now_less_than(+Time)
%
%	Check if the wenet time is less than a time.
%
%	@param Time to be greater or equal than.
%
is_now_less_than(Time) :-
	get_now(Now),
	<(Now,Time)
	.

%!	is_now_less_than_or_equal_to(+Time)
%
%	Check if the wenet time is less than or equal to a time.
%
%	@param Actions to execute.
%
is_now_less_than_or_equal_to(Time) :-
	get_now(Now),
	=<(Now,Time)
	.

%!	is_now_greater_than(+Time)
%
%	Check if the wenet time is greater than a time.
%
%	@param Actions to execute.
%
is_now_greater_than(Time) :-
	get_now(Now),
	>(Now,Time)
	.

%!	is_now_greater_than_or_equal_to(+Time)
%
%	Check if the wenet time is greater than or equal to a time.
%
%	@param Actions to execute.
%
is_now_greater_than_or_equal_to(Time) :-
	get_now(Now),
	>=(Now,Time)
	.

%!	is_now_equal_to(+Time)
%
%	Check if the wenet time is equal to a time.
%
%	@param Actions to execute.
%
is_now_equal_to(Time) :-
	get_now(Now),
	=(Now,Time)
	.

%!	get_message(-Message)
%
%	Return the received message to process.
%
%	@param Message json received on the norm engine.
%
get_message(Message) :-
	wenet_protocol_message_file(FilePath),
	wenet_read_json_from_file(Message,FilePath),
	!,
	retractall(get_message(_)),
	asserta(get_message(Message))
	.

%!	get_profile(-Profile)
%
%	Return the current profile of the user that is checking the norms.
%
%	@param Profile json definition of the profile.
%
get_profile(Profile) :-
	(
		current_predicate(wenet_protocol_profile_file/1)->wenet_protocol_profile_file(FilePath);false
	),
	wenet_read_json_from_file(Profile,FilePath),
	!,
	retractall(get_profile(_)),
	asserta(get_profile(Profile))
	.

%!	get_profile_id(-ProfileId)
%
%	Return the current profile identifier of the user that is checking the norms.
%
%	@param ProfileId string with the user identifier.
%
get_profile_id(ProfileId) :-
	get_message(Message),
	wenet_receiver_id_of_protocol_message(ProfileId,Message),
	!,
	retractall(get_profile_id(_)),
	asserta(get_profile_id(ProfileId))
	.

%!	get_community(-Community)
%
%	Return the current community of the user that is checking the norms.
%
%	@param Community json definition of the community.
%
get_community(Community) :-
	(
		current_predicate(wenet_protocol_community_file/1)->wenet_protocol_community_file(FilePath);false
	),
	wenet_read_json_from_file(Community,FilePath),
	!,
	retractall(get_community(_)),
	asserta(get_community(Community))
	.


%!	get_community_id(-CommunityId)
%
%	Return the current community identifier of the user that is checking the norms.
%
%	@param CommunityId string with the user identifier.
%
get_community_id(CommunityId) :-
	get_message(Message),
	wenet_community_id_of_protocol_message(CommunityId,Message),
	!,
	retractall(get_community_id(_)),
	asserta(get_community_id(CommunityId))
	.

%!	get_task(-Task)
%
%	Return the current task of the user that is checking the norms.
%
%	@param Task json definition of the task.
%
get_task(Task) :-
	(
		current_predicate(wenet_protocol_task_file/1)->wenet_protocol_task_file(FilePath);false
	),
	wenet_read_json_from_file(Task,FilePath),
	!,
	retractall(get_task(_)),
	asserta(get_task(Task))
	.


%!	get_task_id(-TaskId)
%
%	Return the current task identifier defined on the norm engine.
%
%	@param TaskId string with the user identifier.
%
get_task_id(TaskId) :-
	get_message(Message),
	wenet_task_id_of_protocol_message(TaskId,Message),
	!,
	retractall(get_task_id(_)),
	asserta(get_task_id(TaskId))
	.

%!	get_task_type(-TaskType)
%
%	Return the current task type of the user that is checking the norms.
%
%	@param TaskType json definition of the task type.
%
get_task_type(TaskType) :-
	(
		current_predicate(wenet_protocol_task_type_file/1)->wenet_protocol_task_type_file(FilePath);false
	),
	wenet_read_json_from_file(TaskType,FilePath),
	!,
	retractall(get_task_type(_)),
	asserta(get_task_type(TaskType))
	.

%!	get_task_type_id(-TaskTypeId)
%
%	Return the current task type identifier defined on the norm engine.
%
%	@param TaskTypeId string with the user identifier.
%
get_task_type_id(TaskTypeId) :-
	get_task(Task),
	wenet_task_type_id_of_task(TaskTypeId,Task),
	!,
	retractall(get_task_type_id(_)),
	asserta(get_task_type_id(TaskTypeId))
	.

%!	get_transaction(-Transaction)
%
%	Return the current transaction defined on the norm engine.
%
%	@param Transaction json transaction on the norm engine.
%
get_transaction(json(Transaction)) :-
	get_transaction_id(TransactionId),
	get_transaction(json(Transaction),TransactionId),
	!,
	retractall(get_transaction(_)),
	asserta(get_transaction(json(Transaction)))
	.

%!	get_transaction(-Transaction,+TransactionId)
%
%	Return the transaction associated to an identifer.
%
%	@param Transaction json transaction on the norm engine.
%	@param TransactionId string identifier of the transaction to return.
%
get_transaction(json(Transaction),TransactionId) :-
	get_task(Task),
	wenet_transactions_of_task(Transactions,Task),
	member(json(Transaction),Transactions),
	member(id=TransactionId,Transaction)
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
	retractall(get_transaction_id(_)),
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
	retractall(get_app(_)),
	asserta(get_app(App))
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
	retractall(get_app_id(_)),
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
	retractall(get_app_message_callback_url(_)),
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
	retractall(get_app_users(_)),
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
	( get_app_users(AppUsers) -> true ; AppUsers=[] ),
	get_profile_id(ProfileId),
	( wenet_remove(Users,ProfileId,AppUsers) -> true ; Users = AppUsers ),
	!,
	retractall(get_app_users_except_me(_)),
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
	wenet_receiver_id_of_protocol_message(UserId,Message),
	!,
	retractall(is_received_created_task()),
	asserta(is_received_created_task())
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
	wenet_attributes_of_transaction(Attributes,Transaction),
	!,
	retractall(is_received_do_transaction(_,_)),
	asserta(is_received_do_transaction(Label,Attributes))
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
	wenet_content_of_protocol_message(Incentive,Message),
	!,
	retractall(is_received_send_incentive(_)),
	asserta(is_received_send_incentive(Incentive))
	.

%!	get_task_attribute_value(-Value,+Key)
%
%	Return the value of an attribute.
%
%	@param Value of the attribute.
%	@param Key name of the attribute to get.
%
get_task_attribute_value(Value,Key) :-
	get_task(Task),
	wenet_attributes_of_task(json(Attributes),Task),
	member(Key=Value,Attributes)
	.

%!	is_received(-SenderId,-Particle,-Content)
%
%	Check if received a send incentive message.
%
%	@param SenderId the identifier of the user that has send the message.
%	@param Particle of the message.
%	@param Content of the message.
%
is_received(SenderId,Particle,Content) :-
	get_message(Message),
	wenet_sender_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_sender_id_of_protocol_message(SenderId,Message),
	wenet_receiver_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_particle_of_protocol_message(Particle,Message),
	wenet_content_of_protocol_message(Content,Message),
	!,
	retractall(is_received(_,_,_)),
	asserta(is_received(SenderId,Particle,Content))
	.

%!	get_task_requester_id(-RequesterId)
%
%	Return the identifier of the task requester.
%
%	@param RequesterId identifier of the user that has request to do the task.
%
get_task_requester_id(RequesterId) :-
	get_task(Task),
	wenet_requester_id_of_task(RequesterId,Task),
	!,
	retractall(get_task_requester_id(_)),
	asserta(get_task_requester_id(RequesterId))
	.

%!	get_task_goal_name(-Name)
%
%	Return the name of the task goal
%
%	@param Name of the task goal.
%
get_task_goal_name(Name) :-
	get_task(Task),
	wenet_goal_name_of_task(Name,Task),
	!,
	retractall(get_task_goal_name(_)),
	asserta(get_task_goal_name(Name))
	.

%!	get_social_explanation(-Explanation,+UserId)
%
%	Obtain the social explanation why choose a volunteer.
%
%	@param Explanation that explains why to choose the user.
%	@param UserId identifier of the user to obtain the social explanation.
%
get_social_explanation(Explanation,UserId) :-
	get_task_id(TaskId),
	wenet_social_context_builder_retrieve_social_explanation(SocialExplanation,UserId,TaskId),
	wenet_description_of_social_explanation(Explanation,SocialExplanation),
	!,
	asserta(get_social_explanation(Explanation,UserId))
	.

%!	is_task_closed()
%
%	Check if the task is closed.
%
is_task_closed() :-
	get_task(Task),
	wenet_is_closed_task(Task),
	!,
	retractall(is_task_closed()),
	asserta(is_task_closed())
	.

%!	get_community_state(-State)
%
%	Return the state of the user on the community.
%
%	@param State of the user on the community.
%
get_community_state(State) :-
	get_profile_id(ProfileId),
	get_community_id(CommunityId),
	(
		wenet_interaction_protocol_engine_get_community_user_state(json(CommunityUserState),CommunityId,ProfileId)
		->
		(
			member(attributes=State,CommunityUserState) -> true ; State = json([])
		)
		; State = json([])
	),
	!,
	retractall(get_community_state(_)),
	asserta(get_community_state(State))
	.

%!	get_community_state_attribute(-Value,+Key)
%
%	Return the state of the user on the community.
%
%	@param Value of the community user state attribute.
%	@param Key of the community user state attribute to get.
%
get_community_state_attribute(Value,Key) :-
	get_community_state(json(State)),
	member(Key=Value,State)
	.

%!	get_community_state_attribute(-Value,+Key,+DefaultValue)
%
%	Return the state of the user on the community or
%	the default value if it is not defined.
%
%	@param Value of the community user state attribute.
%	@param Key of the community user state attribute to get.
%	@param DefaultValue to return if the key is not defined. 
%
get_community_state_attribute(Value,Key,DefaultValue) :-
	get_community_state_attribute(Value,Key) -> true ; Value = DefaultValue
	.

%!	get_attribute(-Value,+Key,+DefaultValue,+Json)
%
%	Return the attribute value for the specified key
%	or the default value if it is not defined.
%
%	@param Value of the attribute.
%	@param Key to get the value.
%	@param DefaultValue to return if the attribute is not defined.
%	@param Json to obtain the value.
%
get_attribute(Value,Key,DefaultValue,Json):-
	get_attribute(Value,Key,Json)->true;Value = DefaultValue
	.
	
%!	get_attribute(-Value,+Key,+DefaultValue,+Json)
%
%	Return the attribute value for the specified key
%	or the default value if it is not defined.
%
%	@param Value of the attribute.
%	@param Key to get the value.
%	@param Attributes to obtain the value.
%
get_attribute(Value,Key,json(Attributes)):-
	member(Key=Value,Attributes)
	.

%!	get_closest_users_to_me(-Users,)
%
%	Get the 10 users that are closed to me.
%
%	@param Users return the users that are closest to me.
%
get_closest_users_to_me(Users) :-
	get_closest_users_to_me(Users,10),
	!,
	retractall(get_closest_users_to_me(_)),
	asserta(get_closest_users_to_me(Users))
	.

%!	get_closest_users_to_me(-Users,+NumUsers)
%
%	Get the users that are closed to me.
%
%	@param Users return the users that are closest to me.
%	@param NumUsers number of closest users to return.
%
get_closest_users_to_me(Users,NumUsers) :-
	get_profile_id(Me),
	( 
		(
			wenet_personal_context_builder_locations([Location|_],[Me]), 
			wenet_longitude_of_location(Longitude,Location),
			wenet_latitude_of_location(Latitude,Location),
			Max is NumUsers + 1,
			wenet_personal_context_builder_closest(ClosestUsers,Latitude,Longitude,Max),
			wenet_users_of_closest(UserIds,ClosestUsers),
			ignore(wenet_remove(Users,Me,UserIds))
		)
		-> true; Users = [] 
	),
	!,
	asserta(get_closest_users_to_me(Users,NumUsers))
	.

%!	get_app_users_near_me(-Users,+Min,+Max)
%
%	Return the app users that are near to me on the specified range.
%
%	@param Users that are near to me on the specified range.
%	@param Min minimum distance (inclusive) in meters between me and the rest of user. 
%	@param Max maximum distance (inclusive) in meters between me and the rest of user.
%
get_app_users_near_me(Users,Min,Max) :-
	(
		(
			get_app_users(AppUsers),
			get_profile_id(ProfileId),
			wenet_personal_context_builder_locations(Locations,AppUsers),
			member(json(Location),Locations),
			member(userId=ProfileId,Location),
			wenet_remove(UserLocations,json(Location),Locations),
			wenet_filter_locations_by_distance(Filtered,json(Location),UserLocations,Min,Max),
			wenet_users_of_locations(Users,Filtered)
		)
		-> true; Users = []
	),
	!,
	asserta(get_app_users_near_me(Users,Min,Max))
	.
	
%!	is_received_event(-Particle,-Content)
%
%	Check if received an event.
%
%	@param Particle of the event.
%	@param Content of the event.
%
is_received_event(Particle,Content) :-
	get_message(Message),
	get_profile_id(Me),
	wenet_sender_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_sender_id_of_protocol_message(Me,Message),
	wenet_receiver_component_of_protocol_message('INTERACTION_PROTOCOL_ENGINE',Message),
	wenet_receiver_id_of_protocol_message(Me,Message),
	wenet_particle_of_protocol_message(Particle,Message),
	wenet_content_of_protocol_message(Content,Message),
	!,
	retractall(is_event(_,_)),
	asserta(is_event(Particle,Content))
	.

%!	get_task_state(-State)
%
%	Return the state of the user on the task.
%
%	@param State of the user on the task.
%
get_task_state(State) :-
	get_profile_id(ProfileId),
	get_task_id(TaskId),
	(
		wenet_interaction_protocol_engine_get_task_user_state(json(TaskUserState),TaskId,ProfileId)
		->
		(
			member(attributes=State,TaskUserState) -> true ; State = json([])
		)
		; State = json([])
	),
	!,
	retractall(get_task_state(_)),
	asserta(get_task_state(State))
	.

%!	get_task_state_attribute(-Value,+Key)
%
%	Return the state of the user on the task.
%
%	@param Value of the task user state attribute.
%	@param Key of the task user state attribute to get.
%
get_task_state_attribute(Value,Key) :-
	get_task_state(json(State)),
	member(Key=Value,State)
	.

%!	get_task_state_attribute(-Value,+Key,+DefaultValue)
%
%	Return the state of the user on the task or
%	the default value if it is not defined.
%
%	@param Value of the task user state attribute.
%	@param Key of the task user state attribute to get.
%	@param DefaultValue to return if the key is not defined. 
%
get_task_state_attribute(Value,Key,DefaultValue) :-
	get_task_state_attribute(Value,Key) -> true ; Value = DefaultValue
	.

%!	get_user_state(-State)
%
%	Return the state of the user on the user.
%
%	@param State of the user on the user.
%
get_user_state(State) :-
	get_profile_id(ProfileId),
	(
		wenet_interaction_protocol_engine_get_user_state(json(UserUserState),ProfileId)
		->
		(
			member(attributes=State,UserUserState) -> true ; State = json([])
		)
		; State = json([])
	),
	!,
	retractall(get_user_state(_)),
	asserta(get_user_state(State))
	.

%!	get_user_state_attribute(-Value,+Key)
%
%	Return the state of the user on the user.
%
%	@param Value of the user user state attribute.
%	@param Key of the user user state attribute to get.
%
get_user_state_attribute(Value,Key) :-
	get_user_state(json(State)),
	member(Key=Value,State)
	.

%!	get_user_state_attribute(-Value,+Key,+DefaultValue)
%
%	Return the state of the user on the user or
%	the default value if it is not defined.
%
%	@param Value of the user user state attribute.
%	@param Key of the user user state attribute to get.
%	@param DefaultValue to return if the key is not defined. 
%
get_user_state_attribute(Value,Key,DefaultValue) :-
	get_user_state_attribute(Value,Key) -> true ; Value = DefaultValue
	.

	
%!	filter_transactions(-Transactions,+Test,+Map)
%
%	This condition is used to obtain a sub set of the transactions that
%	has been done in the task and map them to a new value. In other words, for
%   each transaction of the current task if call(Test,Transaction) is True,
%	it transforms the transaction with call(Map,Value,Transaction) and it adds
%	the obtained Value to the result list.  
%
%	@param Result the filtered and mapped task transactions.
%	@param Test predicate to call to known if the transaction is accepted.
%	@param Map predicate to call to convert the accepted transaction.
%
filter_transactions(Result,Test,Map):-
	get_task(Task),
	wenet_transactions_of_task(DoneTransactions,Task),
	!,
	filter_transactions_(Result,DoneTransactions,Test,Map)
	.
filter_transactions_([],[],_,_).
filter_transactions_(Target,[Head|Tail],Test,Map):-
	wenet_log_trace('Head:',[Head]),
	(
		call(Test,Head) 
		-> (
			call(Map,NewHead,Head)
			-> Target = [NewHead|NewTarget]
			; !,fail
			)
		; Target = NewTarget
	),
	filter_transactions_(NewTarget,Tail,Test,Map)
	.

%!	normalized_closeness(-Closeness,+Users,+MaxDistance)
%
%	Calculate the closeness (in distance) of a user repect some others.
%
%	@param Closeness a list with the closeness between a user and some others.
%	@param Users identifiers of the users to calculate the closeness.
%	@param MaxDistance the maximum distance in meters that any user can be.
%
normalized_closeness(Closeness,Users,MaxDistance) :-
	(
		get_profile_id(UserId),
		wenet_personal_context_builder_locations(Locations,[UserId|Users]),
		!,
		member(SourceLocation,Locations),
		wenet_user_id_of_location(UserId,SourceLocation),
		!,
		normalized_closeness_(Closeness,Users,MaxDistance,Locations,SourceLocation)
	)
	-> true
	; wenet_initialize_user_values(Closeness,Users,0.0)
	.
normalized_closeness_([],[],_,_,_).
normalized_closeness_([UserCloseness|ClosenessRest],[UserId|Users],MaxDistance,Locations,SourceLocation) :-
	(
		(
			member(TargetLocation,Locations),
			wenet_user_id_of_location(UserId,TargetLocation),
			!,
			wenet_distance_between_locations(DistanceInMeters,SourceLocation,TargetLocation)
		)
		-> Distance is 1.0 - min(DistanceInMeters,MaxDistance) / MaxDistance
		; Distance = 0.0
	),
	!,
	wenet_new_user_value(UserCloseness,UserId,Distance),
	normalized_closeness_(ClosenessRest,Users,MaxDistance,Locations,SourceLocation)
	.

%!	normalized_social_closeness(-Socialness,+Users)
%
%	Calculate the socialness of a user repect some others.
%
%	@param Socialness a list with the socialness between a user and some others.
%	@param Users identifiers of the users to calculate the socialness.
%
normalized_social_closeness(Socialness,Users) :-
	(
		get_profile(Profile),
		get_app_id(AppId),
		wenet_relationships_of_profile(Relationships,Profile),
		!,
		normalized_social_closeness_(Socialness,Users,Relationships,AppId)
	)
	-> true
	; wenet_initialize_user_values(Socialness,Users,0.0)
	.
normalized_social_closeness_([],[],_,_).
normalized_social_closeness_([UserSocialness|SocialnessRest],[UserId|Users],Relationships,AppId) :-
	(
		(
			member(Relationship,Relationships),
			wenet_user_id_of_relationship(UserId,Relationship),
			wenet_app_id_of_relationship(AppId,Relationship)
		)
		-> wenet_weight_of_relationship(Weight,Relationship)
		; Weight = 0.0
	),
	!,
	wenet_new_user_value(UserSocialness,UserId,Weight),
	normalized_social_closeness_(SocialnessRest,Users,Relationships,AppId)
	.

%!	normalized_diversity(-Diversity,+Users,+Attributes)
%
%	Calculate the socialness of a user repect some others.
%
%	@param Diversity a value in the range [0,1] that says how the diverse are the users team.
%	@param Users array with the users identifiers to calculate the diversity.
%	@param Attributes array with the names of the attributes to calculate the diversity.
%
normalized_diversity(Diversity,Users,Attributes) :-
	(
		get_profile_id(Me),
		normalized_diversity_(Diversity,Users,Attributes,Me)
	)
	-> true
	; wenet_initialize_user_values(Diversity,Users,0.0)
	.
normalized_diversity_([],[],_,_).
normalized_diversity_([UserDiversity|UsersDiversity],[User|Users],Attributes,Me) :-
	(
		(
			wenet_new_diversity_data(Data,[Me,User],Attributes),
			!,
			wenet_profile_manager_operations_calculate_diversity(Diversity,Data)
		)
		-> true
		; Diversity = 0.0
	),
	!,
	wenet_new_user_value(UserDiversity,User,Diversity),
	normalized_diversity_(UsersDiversity,Users,Attributes,Me)
	.

%!	my_profile_attributes_similars_to(-Attributes,+Text,+MinSimilarity)
%
%	Obtain the attributes of my profile that has a similarity to a text 
%	equals or greater than the minimum.
%
%	@param Attributes a list with the name of the profile attributes that are similar to the text.
%	@param Text to compare the profile attributes.
%	@param MinSimilarity a value in the range [0,1] that define the minimum similarity to take an attribute.
%
my_profile_attributes_similars_to(Attributes,Text,MinSimilarity) :-
	(
		get_profile_id(Me),
		wenet_new_similarity_data(Data,Me,Text),
		wenet_profile_manager_operations_calculate_similarity(Similarity,Data),
		wenet_attributes_of_similarity_result(Attributes,Similarity,MinSimilarity)
	)
	-> true
	; Attributes = []
	.

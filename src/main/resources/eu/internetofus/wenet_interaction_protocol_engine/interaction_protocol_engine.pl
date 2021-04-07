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
	wenet_interaction_protocol_engine_api_url_to/2,
	wenet_interaction_protocol_engine_send_message/2,
	wenet_interaction_protocol_engine_send_event/2,
	wenet_interaction_protocol_engine_get_community_user_state/3,
	wenet_interaction_protocol_engine_merge_community_user_state/4,
	wenet_app_id_of_protocol_message/2,
	wenet_community_id_of_protocol_message/2,
	wenet_task_id_of_protocol_message/2,
	wenet_transaction_id_of_protocol_message/2,
	wenet_sender_of_protocol_message/2,
	wenet_sender_component_of_protocol_message/2,
	wenet_sender_id_of_protocol_message/2,
	wenet_receiver_of_protocol_message/2,
	wenet_receiver_component_of_protocol_message/2,
	wenet_receiver_id_of_protocol_message/2,
	wenet_particle_of_protocol_message/2,
	wenet_content_of_protocol_message/2,
	wenet_new_protocol_message/11,
	wenet_interaction_protocol_engine_get_task_user_state/3,
	wenet_interaction_protocol_engine_merge_task_user_state/4,
	wenet_interaction_protocol_engine_get_user_state/2,
	wenet_interaction_protocol_engine_merge_user_state/3,
	wenet_id_of_protocol_event/2,
	wenet_interaction_protocol_engine_delete_event/2
	.


%!	wenet_interaction_protocol_engine_api_url_to(-Url,+Paths)
%
%	Calculate the URL to interact to the specified path of the interaction protocol engine.
%
%	@param Url to the interaction protocol engine to the specified path.
%	@param Paths to the interaction protocol engine.
%
wenet_interaction_protocol_engine_api_url_to(Url,Paths) :-
	wenet_interaction_protocol_engine_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_interaction_protocol_engine_send_message(-Sent,+Message)
%
%	Send a message to the interaction protocol engine of another user.
%
%	@param Sent the sent message result.
%	@param Message to send.
%
wenet_interaction_protocol_engine_send_message(Sent,Message) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/messages']),
	wenet_post_json_to_url(Sent,Url,Message)
	.

%!	wenet_interaction_protocol_engine_merge_community_user_state(-State,+CommunityId,+UserId)
%
%	Retrieve the state of an user for a community.
%
%	@param State of the user on the community.
%	@param CommunityId identifier of the community to get the state.
%	@param UserId identifier of the user to get the state.
%
wenet_interaction_protocol_engine_get_community_user_state(State,CommunityId,UserId) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/states/communities/',CommunityId,'/users/',UserId]),
	wenet_get_json_from_url(State,Url)
	.

%!	wenet_interaction_protocol_engine_merge_community_user_state(-MergedState,+CommunityId,+UserId,+NewState)
%
%	Merge the state of an user for a community.
%
%	@param MergedState the merged state of the user.
%	@param CommunityId identifier of the community to change the state.
%	@param UserId identifier of the user to change the state.
%	@param NewState the value to set the community user state.
%
wenet_interaction_protocol_engine_merge_community_user_state(MergedState,CommunityId,UserId,NewState) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/states/communities/',CommunityId,'/users/',UserId]),
	wenet_patch_json_to_url(MergedState,Url,NewState)
	.

%!	wenet_app_id_of_protocol_message(-AppId,+Message)
%
%	Obtain the application identifier associated to a protocol message.
%
%	@param AppId of the message.
%	@param Message to get the application identifier.
%
wenet_app_id_of_protocol_message(AppId, json(Message)) :-
	member(appId=AppId,Message)
	.

%!	wenet_community_id_of_protocol_message(-CommunityId,+Message)
%
%	Obtain the community identifier associated to a protocol message.
%
%	@param CommunityId of the message.
%	@param Message to get the community identifier.
%
wenet_community_id_of_protocol_message(CommunityId, json(Message)) :-
	member(communityId=CommunityId,Message)
	.

%!	wenet_task_id_of_protocol_message(-TaskId,+Message)
%
%	Obtain the task identifier associated to a protocol message.
%
%	@param TaskId of the message.
%	@param Message to get the task identifier.
%
wenet_task_id_of_protocol_message(TaskId, json(Message)) :-
	member(taskId=TaskId,Message)
	.

%!	wenet_transaction_id_of_protocol_message(-TransactionId,+Message)
%
%	Obtain the transaction identifier associated to a protocol message.
%
%	@param TransactionId of the message.
%	@param Message to get the transaction identifier.
%
wenet_transaction_id_of_protocol_message(TransactionId, json(Message)) :-
	member(transactionId=TransactionId,Message)
	.

%!	wenet_sender_of_protocol_message(-Sender,+Message)
%
%	Obtain the sender associated to a protocol message.
%
%	@param Sender of the message.
%	@param Message to get the sender.
%
wenet_sender_of_protocol_message(Sender, json(Message)) :-
	member(sender=Sender,Message)
	.

%!	wenet_sender_component_of_protocol_message(-SenderComponent,+Message)
%
%	Obtain the sender component associated to a protocol message.
%
%	@param SenderComponent of the message.
%	@param Message to get the sender.
%
wenet_sender_component_of_protocol_message(SenderComponent, json(Message)) :-
	wenet_sender_of_protocol_message(json(Sender), json(Message)),
	member(component=SenderComponent,Sender)
	.

%!	wenet_sender_id_of_protocol_message(-SenderId,+Message)
%
%	Obtain the sender identifier associated to a protocol message.
%
%	@param SenderId of the message.
%	@param Message to get the sender.
%
wenet_sender_id_of_protocol_message(SenderId, json(Message)) :-
	wenet_sender_of_protocol_message(json(Sender), json(Message)),
	member(userId=SenderId,Sender)
	.

%!	wenet_receiver_of_protocol_message(-Receiver,+Message)
%
%	Obtain the receiver associated to a protocol message.
%
%	@param Receiver of the message.
%	@param Message to get the receiver.
%
wenet_receiver_of_protocol_message(Receiver, json(Message)) :-
	member(receiver=Receiver,Message)
	.

%!	wenet_receiver_component_of_protocol_message(-ReceiverComponent,+Message)
%
%	Obtain the receiver component associated to a protocol message.
%
%	@param ReceiverComponent of the message.
%	@param Message to get the receiver.
%
wenet_receiver_component_of_protocol_message(ReceiverComponent, json(Message)) :-
	wenet_receiver_of_protocol_message(json(Receiver), json(Message)),
	member(component=ReceiverComponent,Receiver)
	.

%!	wenet_receiver_id_of_protocol_message(-ReceiverId,+Message)
%
%	Obtain the receiver identifier associated to a protocol message.
%
%	@param ReceiverId of the message.
%	@param Message to get the receiver.
%
wenet_receiver_id_of_protocol_message(ReceiverId, json(Message)) :-
	wenet_receiver_of_protocol_message(json(Receiver), json(Message)),
	member(userId=ReceiverId,Receiver)
	.

%!	wenet_particle_of_protocol_message(-Particle,+Message)
%
%	Obtain the particle associated to a protocol message.
%
%	@param Particle of the message.
%	@param Message to get the particle.
%
wenet_particle_of_protocol_message(Particle, json(Message)) :-
	member(particle=Particle,Message)
	.

%!	wenet_content_of_protocol_message(-Content,+Message)
%
%	Obtain the content associated to a protocol message.
%
%	@param Content of the message.
%	@param Message to get the content.
%
wenet_content_of_protocol_message(Content, json(Message)) :-
	member(content=Content,Message)
	.


%!	wenet_new_protocol_message(-Message,+AppId,+CommunityId,+TaskId,+TransactionId,+SenderComponent,+SenderUserId,+ReceiverComponent,+ReceiverUserId,+Particle,+Content)
%
%	Create a protocol message.
%
%	@param Message that has been created.
%	@param AppId application identifier of the message.
%	@param CommunityId community identifier of the message.
%	@param TaskId task identifier of the message.
%	@param TransactionId transaction identifier of the message.
%	@param SenderComponent of the message.
%	@param SenderUserId of the message.
%	@param ReceiverComponent of the message.
%	@param ReceiverUserId of the message.
%	@param Particle of the message.
%	@param Content of the message.
%
wenet_new_protocol_message(Message,AppId,CommunityId,TaskId,TransactionId,SenderComponent,SenderUserId,ReceiverComponent,ReceiverUserId,Particle,@(null)) :-
	wenet_new_protocol_message(Message,AppId,CommunityId,TaskId,TransactionId,SenderComponent,SenderUserId,ReceiverComponent,ReceiverUserId,Particle,json([])).
wenet_new_protocol_message(Message,AppId,CommunityId,TaskId,TransactionId,SenderComponent,SenderUserId,ReceiverComponent,ReceiverUserId,Particle,Content) :-
	Message = json([appId=AppId,communityId=CommunityId,taskId=TaskId,transactionId=TransactionId,sender=json([component=SenderComponent,userId=SenderUserId]),receiver=json([component=ReceiverComponent,userId=ReceiverUserId]),particle=Particle,content=Content])
	.

%!	wenet_interaction_protocol_engine_merge_task_user_state(-State,+TaskId,+UserId)
%
%	Retrieve the state of an user for a task.
%
%	@param State of the user on the task.
%	@param TaskId identifier of the task to get the state.
%	@param UserId identifier of the user to get the state.
%
wenet_interaction_protocol_engine_get_task_user_state(State,TaskId,UserId) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/states/tasks/',TaskId,'/users/',UserId]),
	wenet_get_json_from_url(State,Url)
	.

%!	wenet_interaction_protocol_engine_merge_task_user_state(-MergedState,+TaskId,+UserId,+NewState)
%
%	Merge the state of an user for a task.
%
%	@param MergedState the merged state of the user.
%	@param TaskId identifier of the task to change the state.
%	@param UserId identifier of the user to change the state.
%	@param NewState the value to set the task user state.
%
wenet_interaction_protocol_engine_merge_task_user_state(MergedState,TaskId,UserId,NewState) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/states/tasks/',TaskId,'/users/',UserId]),
	wenet_patch_json_to_url(MergedState,Url,NewState)
	.

%!	wenet_interaction_protocol_engine_merge_community_user_state(-State,+UserId)
%
%	Retrieve the state of an user.
%
%	@param State of the user on the community.
%	@param UserId identifier of the user to get the state.
%
wenet_interaction_protocol_engine_get_community_user_state(State,CommunityId,UserId) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/states/users/',UserId]),
	wenet_get_json_from_url(State,Url)
	.
	
%!	wenet_interaction_protocol_engine_merge_user_state(-MergedState,+UserId,+NewState)
%
%	Merge the state of an user.
%
%	@param MergedState the merged state of the user.
%	@param UserId identifier of the user to change the state.
%	@param NewState the value to set the community user state.
%
wenet_interaction_protocol_engine_merge_community_user_state(MergedState,UserId,NewState) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/states/users/',UserId]),
	wenet_patch_json_to_url(MergedState,Url,NewState)
	.

%!	wenet_interaction_protocol_engine_send_event(-Sent,+Event)
%
%	Send an event.
%
%	@param Sent the sent event result.
%	@param Event to send.
%
wenet_interaction_protocol_engine_send_event(Sent,Event) :-
	wenet_interaction_protocol_engine_api_url_to(Url,['/events']),
	wenet_post_json_to_url(Sent,Url,Event)
	.

%!	wenet_new_protocol_event(-Event,+AppId,+CommunityId,+TaskId,+TransactionId,+UserId,+Delay,+Particle,+Content)
%
%	Create a protocol event.
%
%	@param Event that has been created.
%	@param AppId application identifier of the event.
%	@param CommunityId community identifier of the event.
%	@param TaskId task identifier of the event.
%	@param TransactionId transaction identifier of the event.
%	@param UserId of the event.
%	@param Delay of the event.
%	@param Particle of the event.
%	@param Content of the event.
%
wenet_new_protocol_event(Event,AppId,CommunityId,TaskId,TransactionId,UserId,Delay,Particle,@(null)) :-
	wenet_new_protocol_event(Event,AppId,CommunityId,TaskId,TransactionId,UserId,Delay,Particle,json([])).
wenet_new_protocol_event(Event,AppId,CommunityId,TaskId,TransactionId,UserId,Delay,Particle,Content) :-
	Event = json([appId=AppId,communityId=CommunityId,taskId=TaskId,transactionId=TransactionId,userId=UserId,delay=Delay,particle=Particle,content=Content])
	.

%!	wenet_id_of_protocol_event(-Id,+Event)
%
%	Obtain the identifier associated to a protocol event.
%
%	@param Id of the event.
%	@param Event to get the identifier.
%
wenet_id_of_protocol_event(Id, json(Event)) :-
	member(id=Id,Event)
	.

%!	wenet_interaction_protocol_engine_delete_event(+Id)
%
%	Delete an event.
%
%	@param Id identifier of the event to delete.
%
wenet_interaction_protocol_engine_delete_event(Id) :-
	wenet_interaction_proto col_engine_api_url_to(Url,['/events/',Id]),
	wenet_delete_to_url(Url)
	.

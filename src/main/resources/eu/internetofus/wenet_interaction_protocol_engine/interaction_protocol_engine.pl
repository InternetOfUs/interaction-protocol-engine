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
	get_interaction_protocol_engine_url_to/2,
	wenet_interaction_protocol_engine_send_message/2,
	wenet_interaction_protocol_engine_get_community_user_state/3,
	wenet_interaction_protocol_engine_merge_community_user_state/4,
	get_protocol_message_app_id/2,
	get_protocol_message_community_id/2,
	get_protocol_message_task_id/2,
	get_protocol_message_transaction_id/2,
	get_protocol_message_sender/2,
	get_protocol_message_receiver/2,
	get_protocol_message_particle/2,
	get_protocol_message_content/2,
	get_protocol_address_component/2,
	get_protocol_address_user_id/2,
	wenet_create_protocol_message_from_received_message_and_send_to_some/4
	.


%!	get_interaction_protocol_engine_url_to(+Url,-Paths)
%
%	Calculate the URL to interact to the specified path of the interaction protocol engine.
%
%	@param Url to the interaction protocol engine to the specified path.
%	@param Paths to the interaction protocol engine.
%
get_interaction_protocol_engine_url_to(Url,Paths) :-
	wenet_interaction_protocol_engine_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_interaction_protocol_engine_send_message(+Sent,-Message)
%
%	Send a message to the interaction protocol engine of another user.
%
%	@param Sent the sent message result.
%	@param Message to send.
%
wenet_interaction_protocol_engine_send_message(Sent,Message) :-
	get_interaction_protocol_engine_url_to(Url,['/messages']),
	wenet_post_json_to_url(Sent,Url,Message)
	.

%!	wenet_interaction_protocol_engine_merge_community_user_state(+State,-CommunityId,-UserId)
%
%	Retrieve the state of an user for a community.
%
%	@param State of the user on the community.
%	@param CommunityId identifier of the community to get the state.
%	@param UserId identifier of the user to get the state.
%
wenet_interaction_protocol_engine_get_community_user_state(State,CommunityId,UserId) :-
	get_interaction_protocol_engine_url_to(Url,['/states/communities/',CommunityId,'/users/'+UserId]),
	wenet_post_get_to_url(State,Url)
	.

%!	wenet_interaction_protocol_engine_merge_community_user_state(+MergedState,-CommunityId,-UserId,-NewState)
%
%	Merge the state of an user for a community.
%
%	@param MergedState the merged state of the user.
%	@param CommunityId identifier of the community to change the state.
%	@param UserId identifier of the user to change the state.
%	@param NewState the value to set the community user state.
%
wenet_interaction_protocol_engine_merge_community_user_state(MergedState,CommunityId,UserId,NewState) :-
	get_interaction_protocol_engine_url_to(Url,['/states/communities/',CommunityId,'/users/'+UserId]),
	wenet_post_json_to_url(MergedState,Url,NewState)
	.

%!	get_protocol_message_app_id(-AppId,+Message)
%
%	Obtain the application identifier associated to a protocol message.
%
%	@param AppId of the message.
%	@param Message to get the application identifier.
%
get_protocol_message_app_id(AppId, json(Message)) :-
	member(appId=AppId,Message)
	.

%!	get_protocol_message_community_id(-CommunityId,+Message)
%
%	Obtain the community identifier associated to a protocol message.
%
%	@param CommunityId of the message.
%	@param Message to get the community identifier.
%
get_protocol_message_community_id(CommunityId, json(Message)) :-
	member(communityId=CommunityId,Message)
	.

%!	get_protocol_message_task_id(-TaskId,+Message)
%
%	Obtain the task identifier associated to a protocol message.
%
%	@param TaskId of the message.
%	@param Message to get the task identifier.
%
get_protocol_message_task_id(TaskId, json(Message)) :-
	member(taskId=TaskId,Message)
	.

%!	get_protocol_message_transaction_id(-TransactionId,+Message)
%
%	Obtain the task transaction identifier associated to a protocol message.
%
%	@param TransactionId of the message.
%	@param Message to get the task transaction identifier.
%
get_protocol_message_transaction_id(TransactionId, json(Message)) :-
	member(transactionId=TransactionId,Message)
	.

%!	get_protocol_message_sender(-Sender,+Message)
%
%	Obtain the sender associated to a protocol message.
%
%	@param Sender of the message.
%	@param Message to get the sender.
%
get_protocol_message_sender(Sender, json(Message)) :-
	member(sender=Sender,Message)
	.

%!	get_protocol_message_receiver(-Receiver,+Message)
%
%	Obtain the receiver associated to a protocol message.
%
%	@param Receiver of the message.
%	@param Message to get the receiver.
%
get_protocol_message_receiver(Receiver, json(Message)) :-
	member(receiver=Receiver,Message)
	.

%!	get_protocol_message_particle(-Particle,+Message)
%
%	Obtain the particle associated to a protocol message.
%
%	@param Particle of the message.
%	@param Message to get the particle.
%
get_protocol_message_particle(Particle, json(Message)) :-
	member(particle=Particle,Message)
	.

%!	get_protocol_message_content(-Content,+Message)
%
%	Obtain the content associated to a protocol message.
%
%	@param Content of the message.
%	@param Message to get the content.
%
get_protocol_message_content(Content, json(Message)) :-
	member(content=Content,Message)
	.

%!	get_protocol_address_component(-Component,+Address)
%
%	Obtain the component of a protocol address.
%
%	@param Component of the address.
%	@param Address to get the component.
%
get_protocol_address_component(Component, json(Address)) :-
	member(component=Component,Address)
	.

%!	get_protocol_address_user_id(-UserId,+Address)
%
%	Obtain the user identifier of a protocol address.
%
%	@param UserId of the address.
%	@param Address to get the user identifier.
%
get_protocol_address_user_id(UserId, json(Address)) :-
	member(userId=UserId,Address)
	.

%!	wenet_create_protocol_message(-Message,+AppId,+CommunityId,+TaskId,+TransactionId,+Sender,+Receiver,+Particle,+Content)
%
%	Create a protocol message.
%
%	@param Message that has been created.
%	@param AppId application identifier of the message.
%	@param CommunityId community identifier of the message.
%	@param TaskId task identifier of the message.
%	@param TransactionId transaction identifier of the message.
%	@param Sender of the message.
%	@param Receiver of the message.
%	@param Particle of the message.
%	@param Content of the message.
%
wenet_create_protocol_message(Message,AppId,CommunityId,TaskId,TransactionId,Sender,Receiver,Particle,Content) :-
	Message = json([appId=AppId,communityId=CommunityId,taskId=TaskId,transactionId=TransactionId,sender=Sender,receiver=Receiver,particle=Particle,content=Content])
	.

%!	wenet_create_protocol_address(-Address,+Component,+UserId)
%
%	Create a protocol address.
%
%	@param Address that has been created.
%	@param Component of the address.
%	@param UserId user identifier of the address.
%
wenet_create_protocol_address(Address,Component,UserId) :-
	Address = json([component=Component,userId=UserId])
	.

%!	wenet_create_protocol_address_to_interaction_protocol_engine_of(-Address,+UserId)
%
%	Create a protocol address to the interaction protocol engine of the specified user.
%
%	@param Address that has been created.
%	@param UserId user identifier of the interaction protocol engine.
%
wenet_create_protocol_address_to_interaction_protocol_engine_of(Address,UserId) :-
	wenet_create_protocol_address(Address,'INTERACTION_PROTOCOL_ENGINE',UserId)
	.

%!	wenet_create_protocol_message_from_received_message(-Message,+TransactionId,+ReceiverId,+Particle,+Content)
%
%	Create a protocol message to the interaction protocol engine of the specified user.
%
%	@param Message that has been created.
%	@param TransactionId transaction identifier of the message.
%	@param ReceiverId identifier of the receiver user.
%	@param Particle of the message.
%	@param Content of the message.
%
wenet_create_protocol_message_from_received_message(Message,TransactionId,ReceiverId,Particle,Content) :-
	get_received_message(ReceivedMessage),
	get_protocol_message_app_id(AppId,ReceivedMessage),
	get_protocol_message_community_id(CommunityId,ReceivedMessage),
	get_protocol_message_task_id(TaskId,ReceivedMessage),
	get_protocol_message_receiver(Sender,ReceivedMessage),
	wenet_create_protocol_address_to_interaction_protocol_engine_of(Receiver,ReceiverId),
	wenet_create_protocol_message(Message,AppId,CommunityId,TaskId,TransactionId,Sender,Receiver,Particle,Content)
	.

%!	wenet_create_protocol_message_from_received_message_and_send_to_some(+Userse,+TransactionId,+Particle,+Content)
%
%	Create a protocol message whit the data of the received message and send to some users.
%
%	@param Users to receive the message.
%	@param TransactionId transaction identifier of the message.
%	@param Particle of the message.
%	@param Content of the message.
%
wenet_create_protocol_message_from_received_message_and_send_to_some([],_,_,_).
wenet_create_protocol_message_from_received_message_and_send_to_some([ReceiverId|Tail],TransactionId,Particle,Content) :-
	wenet_create_protocol_message_from_received_message(Message,TransactionId,ReceiverId,Particle,Content),
	wenet_interaction_protocol_engine_send_message(_,Message),
	wenet_create_protocol_message_from_received_message_and_send_to_some(Tail,TransactionId,Particle,Content)
	.


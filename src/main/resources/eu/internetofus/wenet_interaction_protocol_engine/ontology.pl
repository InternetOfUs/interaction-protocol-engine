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
	get_received_message_appid/1,
	get_received_message_communityid/1,
	get_received_message_taskid/1,
	get_received_message_transactionid/1,
	get_received_message_sender/1,
	get_received_message_sender_component/1,
	get_received_message_sender_userid/1,
	get_received_message_receiver/1,
	get_received_message_receiver_component/1,
	get_received_message_receiver_userid/1,
	get_received_message_particle/1,
	get_received_message_content/1,
	wenet_now_less_than/1,
	wenet_now_less_than_or_equal_to/1,
	wenet_now_greater_than/1,
	wenet_now_greater_than_or_equal_to/1,
	wenet_now_equal_to/1
	.

:- use_module(library(clpfd)).

%!	get_received_message_appid(+AppId)
%
%	Obtain the application identifier of the received message
%
%	@param AppId of the received message.
%
get_received_message_appid(AppId) :-
	get_received_message(Message),
	AppId = Message.appId
	.

%!	get_received_message_communityid(+CommunityId)
%
%	Obtain the community identifier of the received message
%
%	@param CommunityId of the received message.
%
get_received_message_communityid(CommunityId) :-
	get_received_message(Message),
	CommunityId = Message.communityId
	.

%!	get_received_message_taskid(+TaskId)
%
%	Obtain the task identifier of the received message
%
%	@param TaskId of the received message.
%
get_received_message_taskid(TaskId) :-
	get_received_message(Message),
	TaskId = Message.taskId
	.

%!	get_received_message_transactionid(+TransactionId)
%
%	Obtain the transaction identifier of the received message
%
%	@param TransactionId of the received message.
%
get_received_message_transactionid(TransactionId) :-
	get_received_message(Message),
	TransactionId = Message.transactionId
	.


%!	get_received_message_sender(+Sender)
%
%	Obtain the sender of the received message
%
%	@param Sender of the received message.
%
get_received_message_sender(Sender) :-
	get_received_message(Message),
	Sender = Message.sender
	.

%!	get_received_message_sender_component(+Component)
%
%	Obtain the sender component of the received message
%
%	@param Component that send the received message.
%
get_received_message_sender_component(Component) :-
	get_received_message_sender(Sender),
	Component = Sender.component
	.

%!	get_received_message_sender_userid(+UserId)
%
%	Obtain the sender userId of the received message
%
%	@param UserId identifier of the sender user of the received message.
%
get_received_message_sender_userid(UserId) :-
	get_received_message_sender(Sender),
	UserId = Sender.userId
	.

%!	get_received_message_receiver(+Receiver)
%
%	Obtain the receiver of the received message
%
%	@param Receiver of the received message.
%
get_received_message_receiver(Receiver) :-
	get_received_message(Message),
	Receiver = Message.receiver
	.

%!	get_received_message_receiver_component(+Component)
%
%	Obtain the receiver component of the received message
%
%	@param Component that send the received message.
%
get_received_message_receiver_component(Component) :-
	get_received_message_receiver(Receiver),
	Component = Receiver.component
	.

%!	get_received_message_receiver_userid(+UserId)
%
%	Obtain the receiver userId of the received message
%
%	@param UserId identifier of the receiver user of the received message.
%
get_received_message_receiver_userid(UserId) :-
	get_received_message_receiver(Receiver),
	UserId = Receiver.userId
	.

%!	get_received_message_particle(+Particle)
%
%	Obtain the particle of the received message
%
%	@param Particle of the received message.
%
get_received_message_particle(Particle) :-
	get_received_message(Message),
	Particle = Message.particle
	.

%!	get_received_message_content(+Content)
%
%	Obtain the content of the received message
%
%	@param Content of the received message.
%
get_received_message_content(Content) :-
	get_received_message(Message),
	Content = Message.content
	.

%!	wenet_now_less_than(+Time)
%
%	Check if the wenet time is less than a time.
%
%	@param Actions to execute.
%
wenet_now_less_than(Time) :-
	wenet_now(Now),
	Now #< Time
	.

%!	wenet_now_less_than_or_equal_to(+Time)
%
%	Check if the wenet time is less than or equal to a time.
%
%	@param Actions to execute.
%
wenet_now_less_than_or_equal_to(Time) :-
	wenet_now(Now),
	Now #=< Time
	.

%!	wenet_now_greater_than(+Time)
%
%	Check if the wenet time is greater than a time.
%
%	@param Actions to execute.
%
wenet_now_greater_than(Time) :-
	wenet_now(Now),
	Now #> Time
	.

%!	wenet_now_greater_than_or_equal_to(+Time)
%
%	Check if the wenet time is greater than or equal to a time.
%
%	@param Actions to execute.
%
wenet_now_greater_than_or_equal_to(Time) :-
	wenet_now(Now),
	Now #>= Time
	.

%!	wenet_now_equal_to(+Time)
%
%	Check if the wenet time is equal to a time.
%
%	@param Actions to execute.
%
wenet_now_equal_to(Time) :-
	wenet_now(Now),
	Now #= Time
	.
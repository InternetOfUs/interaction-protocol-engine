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
	is_now_less_than/1,
	is_now_less_than_or_equal_to/1,
	is_now_greater_than/1,
	is_now_greater_than_or_equal_to/1,
	is_now_equal_to/1,
	is_received_do_transaction/1,
	is_received_do_transaction_with_label/2,
	is_received_created_task/0,
	is_received_send_incentive/1
	.

:- use_module(library(clpfd)).

%!	get_received_message_appid(+AppId)
%
%	Obtain the application identifier of the received message
%
%	@param AppId of the received message.
%
get_received_message_appid(AppId) :-
	get_received_message(json(Message)),
	member(appId=AppId,Message)
	.

%!	get_received_message_communityid(+CommunityId)
%
%	Obtain the community identifier of the received message
%
%	@param CommunityId of the received message.
%
get_received_message_communityid(CommunityId) :-
	get_received_message(json(Message)),
	member(communityId=CommunityId,Message)
	.

%!	get_received_message_taskid(+TaskId)
%
%	Obtain the task identifier of the received message
%
%	@param TaskId of the received message.
%
get_received_message_taskid(TaskId) :-
	get_received_message(json(Message)),
	member(taskId=TaskId,Message)
	.

%!	get_received_message_transactionid(+TransactionId)
%
%	Obtain the transaction identifier of the received message
%
%	@param TransactionId of the received message.
%
get_received_message_transactionid(TransactionId) :-
	get_received_message(json(Message)),
	member(transactionId=TransactionId,Message)
	.


%!	get_received_message_sender(+Sender)
%
%	Obtain the sender of the received message
%
%	@param Sender of the received message.
%
get_received_message_sender(Sender) :-
	get_received_message(json(Message)),
	member(sender=Sender,Message)
	.

%!	get_received_message_sender_component(+Component)
%
%	Obtain the sender component of the received message
%
%	@param Component that send the received message.
%
get_received_message_sender_component(Component) :-
	get_received_message_sender(json(Sender)),
	member(component=Component,Sender)
	.

%!	get_received_message_sender_userid(+UserId)
%
%	Obtain the sender userId of the received message
%
%	@param UserId identifier of the sender user of the received message.
%
get_received_message_sender_userid(UserId) :-
	get_received_message_sender(json(Sender)),
	member(userId=UserId,Sender)
	.

%!	get_received_message_receiver(+Receiver)
%
%	Obtain the receiver of the received message
%
%	@param Receiver of the received message.
%
get_received_message_receiver(Receiver) :-
	get_received_message(json(Message)),
	member(receiver=Receiver,Message)
	.

%!	get_received_message_receiver_component(+Component)
%
%	Obtain the receiver component of the received message
%
%	@param Component that send the received message.
%
get_received_message_receiver_component(Component) :-
	get_received_message_receiver(json(Receiver)),
	member(component=Component,Receiver)
	.

%!	get_received_message_receiver_userid(+UserId)
%
%	Obtain the receiver userId of the received message
%
%	@param UserId identifier of the receiver user of the received message.
%
get_received_message_receiver_userid(UserId) :-
	get_received_message_receiver(json(Receiver)),
	member(userId=UserId,Receiver)
	.

%!	get_received_message_particle(+Particle)
%
%	Obtain the particle of the received message
%
%	@param Particle of the received message.
%
get_received_message_particle(Particle) :-
	get_received_message(json(Message)),
	member(particle=Particle,Message)
	.

%!	get_received_message_content(+Content)
%
%	Obtain the content of the received message
%
%	@param Content of the received message.
%
get_received_message_content(Content) :-
	get_received_message(json(Message)),
	member(content=Content,Message)
	.

%!	is_now_less_than(+Time)
%
%	Check if the wenet time is less than a time.
%
%	@param Actions to execute.
%
is_now_less_than(Time) :-
	is_now(Now),
	Now #< Time
	.

%!	is_now_less_than_or_equal_to(+Time)
%
%	Check if the wenet time is less than or equal to a time.
%
%	@param Actions to execute.
%
is_now_less_than_or_equal_to(Time) :-
	is_now(Now),
	Now #=< Time
	.

%!	is_now_greater_than(+Time)
%
%	Check if the wenet time is greater than a time.
%
%	@param Actions to execute.
%
is_now_greater_than(Time) :-
	is_now(Now),
	Now #> Time
	.

%!	is_now_greater_than_or_equal_to(+Time)
%
%	Check if the wenet time is greater than or equal to a time.
%
%	@param Actions to execute.
%
is_now_greater_than_or_equal_to(Time) :-
	is_now(Now),
	Now #>= Time
	.

%!	is_now_equal_to(+Time)
%
%	Check if the wenet time is equal to a time.
%
%	@param Actions to execute.
%
is_now_equal_to(Time) :-
	is_now(Now),
	Now #= Time
	.

%!	is_received_do_transaction(-Transaction)
%
%	Check if received a do transaction from the user.
%
%	@param Transaction that has to do.
%
is_received_do_transaction(Transaction) :-
	get_received_message_particle('doTaskTransaction'),
	get_received_message_sender_component('USER_APP'),
	get_received_message_sender_userid(UserId),
	get_received_message_receiver_component('INTERACTION_PROTOCOL_ENGINE'),
	get_received_message_receiver_userid(UserId),
	get_received_message_content(Transaction)
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
	get_received_message_particle('createdTask'),
	get_received_message_sender_component('USER_APP'),
	get_received_message_sender_userid(UserId),
	get_received_message_receiver_component('INTERACTION_PROTOCOL_ENGINE'),
	get_received_message_receiver_userid(UserId)
	.

%!	is_received_send_incentive(-Incentive)
%
%	Check if teh incentive server try to send an incentive to an user.
%
%	@param Incentive to send to the user.
%
is_received_send_incentive(Incentive) :-
	get_received_message_particle('sendIncentive'),
	get_received_message_sender_component('INCENTIVE_SERVER'),
	get_received_message_receiver_component('INTERACTION_PROTOCOL_ENGINE'),
	get_received_message_content(Incentive)
	.

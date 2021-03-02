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
	is_received_message_from_another_iteraction_protocol_engine_with_particle/2
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
	get_received_message(Message),
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
	get_received_message(Message),
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
	get_received_message(Message),
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
	get_received_message(Message),
	get_protocol_message_particle(Particle,Message),
	get_protocol_message_sender(Sender,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Sender),
	get_protocol_message_receiver(Receiver,Message),
	get_protocol_address_component('INTERACTION_PROTOCOL_ENGINE',Receiver)
	.

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
	send_user_message/2
	.


%!	add_created_transaction()
%
%	Add the first transaction to a created task.
%
add_created_transaction() :-
	get_profile_id(ProfileId),
	get_task_id(TaskId),
	Transaction = json([taskId=TaskId,actioneerId=ProfileId,label='CREATE_TASK']),
	ignore(wenet_task_manager_add_transaction_into_task(InitialTransaction,TaskId,Transaction)),
	wenet_id_of_transaction(InitialTransactionId,InitialTransaction),
	asserta(add_created_transaction()),
	asserta(get_transaction(InitialTransaction)),
	asserta(get_transaction_id(InitialTransactionId))
	.

%!	add_message_transaction()
%
%	Add the transaction of the message into the task.
%
add_message_transaction() :-
	get_message(Message),
	wenet_content_of_protocol_message(Transaction,Message),
	get_task_id(TaskId),
	ignore(wenet_task_manager_add_transaction_into_task(AddedTransaction,TaskId,Transaction)),
	wenet_id_of_transaction(AddedTransactionId,AddedTransaction),
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
	ignore(wenet_service_post_callback(_,Message,Url)),
	get_task_id(TaskId),
	get_transaction_id(TransactionId),
	ignore(wenet_task_manager_add_message_into_transaction(_,TaskId,TransactionId,Message))
	.

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
	send_to_my_user/2
	.


%!	send_to_my_user(+Label,+Attributes)
%
%	Send a message to the user of the interaction protocol engine.
%
%	@param Label of the message.
%	@param Attributes list of the message attributes on the form key=value.
%
send_to_my_user(Label,Attributes) :-
	is_list(Attributes),
	send_to_my_user(Label,json(Attributes)).

%!	send_to_my_user(+Label,+json(Attributes))
%
%	Send a message to the user of the interaction protocol engine.
%
%	@param Label of the message.
%	@param Attributes json attributes of the message.
%
send_to_my_user(Label,json(Attributes)) :-
	get_app_id(AppId),
	get_profile_id(ReceiverId),
	wenet_new_callback(Callback,AppId,ReceiverId,Label,json(Attributes)),
	get_app_message_callback_url(Url),
	wenet_service_post_callback(Callback,Url),
	()->
	.

%!	add_created_transaction()
%
%	Add the first transaction to a created task.
%
add_created_transaction() :-
	get_profile_id(ProfileId),
	get_task_id(TaskId),
	Transaction = json([taskId=TaskId,actioneerId=ProfileId,label='CREATE_TASK']),
	wenet_task_manager_add_transaction_into_task(InitialTransaction,TaskId,Transaction),
	wenet_id_of_transaction(InitialTransactionId,InitialTransaction),
	!,
	asserta(add_created_transaction()),
	asserta(get_transaction(InitialTransaction)),
	asserta(get_transaction_id(InitialTransactionId))
	.

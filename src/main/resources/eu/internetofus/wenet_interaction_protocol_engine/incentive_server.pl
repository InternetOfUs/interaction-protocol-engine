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
	wenet_incentive_server_api_url_to/2,
	wenet_incentive_server_update_task_transaction_status/2,
	wenet_new_task_transaction_status/7,
	wenet_incentive_server_update_task_type_status/2,
	wenet_new_task_type_status/6
	.

%!	wenet_incentive_server_api_url_to(-Url,+Paths)
%
%	Calculate the URL to interact to the specified path of the incentive server.
%
wenet_incentive_server_api_url_to(Url,Paths) :-
	wenet_incentive_server_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_incentive_server_update_task_transaction_status(-Updated,+Status)
%
%	Update the task transaction status.
%
%	@param Updated the updated status.
%	@param Status to update.
%
wenet_incentive_server_update_task_transaction_status(Updated,Status) :-
	wenet_incentive_server_api_url_to(Url,['/Tasks/TaskTransactionStatus/']),
	wenet_post_json_to_url(Updated,Url,Status)
	.

%!	wenet_new_task_transaction_status(-Status,+UserId,+CommunityId,+AppId,+TaskTypeId,+Label,+Count)
%
%	Create a task status.
%
%	@param Status that has been created.
%	@param UserId of the status.
%	@param CommunityId of the status.
%	@param AppId of the status.
%	@param TaskTypeId of the status.
%	@param Label that change the task.
%	@param Count of the action.
%
wenet_new_task_transaction_status(Status,UserId,CommunityId,AppId,TaskTypeId,Label,Count) :-
	Status = json([user_id=UserId,community_id=CommunityId,app_id=AppId,taskTypeId=TaskTypeId,label=Label,count=Count])
	.

%!	wenet_incentive_server_update_task_type_status(-Updated,+Status)
%
%	Update the task type status.
%
%	@param Updated the updated status.
%	@param Status to update.
%
wenet_incentive_server_update_task_type_status(Updated,Status) :-
	wenet_incentive_server_api_url_to(Url,['/Tasks/TaskTypeStatus/']),
	wenet_post_json_to_url(Updated,Url,Status)
	.

%!	wenet_new_task_type_status(-Status,+UserId,+CommunityId,+AppId,+TaskTypeId,+Count)
%
%	Create a task status.
%
%	@param Status that has been created.
%	@param UserId of the status.
%	@param CommunityId of the status.
%	@param AppId of the status.
%	@param TaskTypeId of the status.
%	@param Count of the action.
%
wenet_new_task_type_status(Status,UserId,CommunityId,AppId,TaskTypeId,Count) :-
	Status = json([user_id=UserId,community_id=CommunityId,app_id=AppId,taskTypeId=TaskTypeId,count=Count])
	.

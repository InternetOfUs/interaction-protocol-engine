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

:- dynamic
	wenet_incentive_server_api_url_to/2,
	wenet_incentive_server_update_task_transaction_status/2,
	wenet_new_task_transaction_status/8,
	wenet_incentive_server_update_task_type_status/2,
	wenet_new_task_type_status/7
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
%	@param TaskId of the status.
%	@param Label that change the task.
%	@param Count of the action.
%
wenet_new_task_transaction_status(Status,UserId,CommunityId,AppId,TaskTypeId,TaskId,Label,Count) :-
	Status = json([user_id=UserId,community_id=CommunityId,app_id=AppId,taskTypeId=TaskTypeId,taskId=TaskId,label=Label,count=Count])
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
%	@param TaskId of the status.
%	@param Count of the action.
%
wenet_new_task_type_status(Status,UserId,CommunityId,AppId,TaskTypeId,TaskId,Count) :-
	Status = json([user_id=UserId,community_id=CommunityId,app_id=AppId,taskTypeId=TaskTypeId,taskId=TaskId,count=Count])
	.

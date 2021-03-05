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
	wenet_incentive_server_update_task_status/2,
	create_task_status/8,
	create_task_status/3
	.

%!	wenet_incentive_server_api_url_to(-Url,+Paths)
%
%	Calculate the URL to interact to the specified path of the incentive server.
%
wenet_incentive_server_api_url_to(Url,Paths) :-
	wenet_incentive_server_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_incentive_server_update_task_status(-Updated,+Status)
%
%	Update the task status.
%
%	@param Updated the updated status.
%	@param Status to update.
%
wenet_incentive_server_update_task_status(Updated,Status) :-
	wenet_incentive_server_api_url_to(Url,['/Tasks/TaskStatus/']),
	wenet_post_json_to_url(Updated,Url,Status)
	.

%!	create_task_status(-Status,+AppId,+UserId,+CommunityId,+TaskId,+Action,+Message)
%
%	Create a task status.
%
%	@param Status that has been created.
%	@param AppId of the status.
%	@param UserId of the status.
%	@param CommunityId of the status.
%	@param TaskId of the status.
%	@param Action of the status.
%	@param Message of the status.
%
create_task_status(Status,AppId,UserId,CommunityId,TaskId,Action,Message) :-
	Status = json([app_id=AppId,user_id=UserId,community_id=CommunityId,task_id=TaskId,'Action'=Action,'Message'=Message])
	.

%!	create_task_status(-Status,+Action,+Message)
%
%	Create a task status with the received message information.
%
%	@param Status that has been created.
%	@param Action of the status.
%	@param Message of the status.
%
create_task_status(Status,Action,Message) :-
	env_app_id(AppId),
	env_profile_id(UserId),
	env_community_id(CommunityId),
	env_task_id(TaskId),
	create_task_status(Status,AppId,UserId,CommunityId,TaskId,Action,Message)
	.
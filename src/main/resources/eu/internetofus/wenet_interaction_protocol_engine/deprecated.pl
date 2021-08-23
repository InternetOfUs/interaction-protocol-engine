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
% This file contains predicates that only are mantained for backguard compatibility
% but you must not to use them
%

:- dynamic
	wenet_incentive_server_update_task_status/2,
	wenet_new_task_status/8,
	notify_incentive_server/2
	.
	
%!	wenet_incentive_server_update_task_status(-Updated,+Status)
%
%	Update the task status. Nothing happens because this API point
%	is not more available.
%
%	@param Updated the updated status.
%	@param Status to update.
%
%   @deprecated this function is not more defined on the API.
%
wenet_incentive_server_update_task_status(Updated,Status) :-
	wenet_log_warning('Called deprecated wenet_incentive_server_update_task_status with:',Status),
	Updated = json([])
	.

%!	wenet_new_task_status(-Status,+AppId,+UserId,+CommunityId,+TaskId,+Action,+Message)
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
%   @deprecated this function is not more necessary becaus ethis model is not more used on the API.
%
wenet_new_task_status(Status,AppId,UserId,CommunityId,TaskId,Action,Message) :-
	Status = json([app_id=AppId,user_id=UserId,community_id=CommunityId,task_id=TaskId,'Action'=Action,'Message'=Message])
	.
	
%!	notify_incentive_server(+Action,+Message)
%
%	Send a message to the interaction protocol engine of an user.
%
%	@param Action to receive the message.
%	@param Message .
%
%   @deprecated this function is not more defined on the API.
%
notify_incentive_server(Action,Message) :-
	wenet_log_warning('Called deprecated notify_incentive_server with:',[Action,Message])
	.

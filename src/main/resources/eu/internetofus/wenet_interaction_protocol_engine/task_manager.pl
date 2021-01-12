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

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- dynamic 
	get_task/1,
	get_task/2.

%!	get_task_manager_url_to(+Url,-Paths)
%
%	Calculate the URL from a path	
%
get_task_manager_url_to(Url,Paths) :-
	wenet_configuration(Configuration),
	wenet_build_url(Url,[Configuration.wenetComponents.taskManager|Paths])
	.


%!	get_task(+Task,-Id)
%
%	Return the task associated to an identifier.
%
%	@param Task dictionary with the task information.
%	@param Id string identifeir of the task to obtain.
%
get_task(Task,Id) :-
	get_task_manager_url_to(Url,["/tasks/",Id]),
	wenet_read_json_from_url(Url,Task),
	asserta(get_task(Task,Id)),
	wenet_log_trace("Loaded task",Task)
	.


%!	get_task(+Task)
%
%	Return the task associated to the engine.
%
%	@param Task dictionary with the task information.
%
get_task(Task) :-
	wenet_message(Message),
	get_task(Task,Message.taskId),
	asserta(get_task(Task)) 
	.

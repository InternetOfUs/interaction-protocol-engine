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
	wenet_do_actions/1,
	wenet_do_action/1,
	wenet_do_actions_status/1,
	go/0,
	check_exist_message/0
	.

:- discontiguous
	wenet_do_actions/1,
	wenet_do_action/1,
	wenet_do_actions_status/1,
	go/0,
	check_exist_message/0
	.

go() :-
	check_exist_message(),
	wenet_execute_safetly_once(normengine(Actions)),
	wenet_do_actions(Actions),
	wenet_do_actions_status(Status)->halt(Status);true
	.

%!	check_exist_message()
%
%	Check that is defined the received message.
%
check_exist_message() :-
	get_message(_)->
		true
		;
		(
			wenet_log_error('No message defined.'),
			halt(1)
		)
	.

%!	wenet_do_actions(+Actions)
%
%	Do the specified actions.q
%
%	@param Actions to execute.
%
wenet_do_actions([]).
wenet_do_actions([NormAction|Tail]) :-
	wenet_do_action(NormAction),
	wenet_do_actions(Tail)
	.

wenet_do_action(put(NormAction)) :-
	wenet_execute_safetly_once(NormAction)
	.
wenet_do_action(not(Action)) :-
	wenet_execute_safetly_once(wenet_log_trace("Discarded ",Action))
	.
wenet_do_action(delay(Action,Delay)) :-
	term_string(Action,ActionStr),
	wenet_execute_safetly_once(send_event(_,Delay,'INTERNAL_DELAY_ACTION_EVENT',json([action=ActionStr])))
	.
%wenet_do_action(Action) :-
%	wenet_execute_safetly_once(wenet_log_error("Unknown how to process ",Action))
%	.


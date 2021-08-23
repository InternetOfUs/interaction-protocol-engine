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
	wenet_do_norm_actions/1,
	wenet_do_actions_status/1.

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
wenet_do_actions([NormActions|Tail]) :-
	flatten(NormActions,NormActionstoDo),
	wenet_do_norm_actions(NormActionstoDo),
	wenet_do_actions(Tail)
	.
wenet_do_actions(_).

wenet_do_norm_actions([]).
wenet_do_norm_actions([put(NormAction)|Tail]) :-
	wenet_execute_safetly_once(NormAction),
	wenet_do_norm_actions(Tail).

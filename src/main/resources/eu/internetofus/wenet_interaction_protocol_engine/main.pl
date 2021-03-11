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
	wenet_do_norm_action_error/2,
	wenet_do_actions_status/1.

go() :-
	check_exist_message(),
	wenet_execute_safetly_once(normengine(Actions)),
	wenet_log_trace('The action to do are:',Actions),
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

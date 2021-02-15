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
	wenet_do_actions_status/1,
	wenet_execute_safetly_once/2,
	wenet_execute_safetly_once/1,
	wenet_read_json_from_file/2,
	wenet_get_json_from_url/2,
	wenet_post_json_to_url/3,
	wenet_post_json_to_url/2,
	wenet_log_trace/2,
	wenet_log_trace/1,
	wenet_log_error/3,
	wenet_log_error/2
	.

:- autoload(library(http/json)).
:- autoload(library(http/http_open)).
:- autoload(library(http/http_client)).
:- autoload(library(http/http_ssl_plugin)).
:- autoload(library(prolog_stack)).


%!  wenet_execute_safetly_once(+Term)
%
%	Execute a term only one time and capture any error that happens.
%
%	@param Term to execute.
%
wenet_execute_safetly_once(Term) :-
	catch(Term->wenet_log_trace('Executed',Term);wenet_log_error('Cannot execute',Term),Error,wenet_log_error('Cannot execute',Term,Error)),
	!.

%!  wenet_read_json_from_file(+FilePath, -Json)
%
%	Read a json file and convert into a list.
%
%	@param FilePath string with the path to the JSON file.
%	@param Json dictionary with the data on the JSON file.
%
wenet_read_json_from_file(FilePath, Json) :-
	wenet_execute_safetly_once(wenet_read_json_from_file_(FilePath, Json)).
wenet_read_json_from_file_(FilePath, Json) :-
	open(FilePath, read, Stream),
	json_read_dict(Stream, Json),
	close(Stream)
	.


%!  wenet_get_json_from_url(+Url, -Json)
%
%	Get a json from an URL.
%
%	@param Url string to the resource with the JSON model.
%	@param Json dictionary with the data on the JSON resource.
%
wenet_get_json_from_url(Url, Json) :-
	wenet_execute_safetly_once(wenet_get_json_from_url_(Url, Json)).
wenet_get_json_from_url_(Url, Json) :-
	wenet_component_auth_header(Header),
	http_open(Url,Stream,[Header]),
	json_read_dict(Stream, Json),
  	close(Stream)
  	.

%!  wenet_post_json_to_url(+Url, +Json)
%
%	Post a json into an URL.
%
%	@param Url string to the post the Json.
%	@param Json dictionary to post.
%
wenet_post_json_to_url(Url, Json) :-
	wenet_component_auth_header(Header),
	wenet_post_json_to_url(Url,Json,[Header]).

%!  wenet_post_json_to_url(+Url, +Json,+Headers)
%
%	Post a json into an URL.
%
%	@param Url string to the post the Json.
%	@param Json dictionary to post.
%	@param Headers for the post action.
%
wenet_post_json_to_url(Url, Json, Headers) :-
	wenet_execute_safetly_once(wenet_post_json_to_url_(Url, Json, Headers)).
wenet_post_json_to_url_(Url, Json, Headers) :-
	atom_json_dict(Atom, Json, []),
	append(Headers,[header(content,'application/json')],PostHeaders),
	http_post(Url, Atom, Result, PostHeaders),
	wenet_log_trace('Post result',Result)
	.

%!	wenet_log_trace(-Text)
%
%	Write a trace log message into the output console.
%
%	@param Text string message of the log.
%
wenet_log_trace(Text) :-
	format(string(Lines),'TRACE: ~w',[Text]),
	print_message_lines(current_output,kind(trace),[Lines])
	.

%!	wenet_log_trace(-Text,-Term)
%
%	Write a trace log message into the output console.
%
%	@param Text string message of the log.
%	@param Term Term to show into the log message.
%
wenet_log_trace(Text,Term) :-
	format(string(Lines),'TRACE: ~w ~w',[Text,Term]),
	print_message_lines(current_output,kind(trace),[Lines])
	.

%!	wenet_log_error(-Text,-Terms)
%
%	Write an error log message into the output console.
%
%	@param Text string message of the log.
%	@param Terms arguments to show into the log message.
%
wenet_log_error(Text,Terms) :-
	wenet_print_error('~w ~w',[Text,Terms])
	.

%!	wenet_log_error(-Text,-Terms,-Error)
%
%	Write an error log message into the output console.
%
%	@param Text string message of the log.
%	@param Terms arguments to show into the log message.
%	@param Error to show into the log message.
%
wenet_log_error(Text,Terms,Error) :-
	message_to_string(Error,ErrorMessage),
	wenet_print_error('~w. ~w ~w',[ErrorMessage,Text,Terms])
	.

%!	wenet_print_error(-Lines)
%
%	Print error message lines.
%
%	@param Format to show into the log error message.
%	@param Arguments to show into the log error message.
%
wenet_print_error(Format,Arguments) :-
	format(string(Lines),Format,Arguments),
	print_message_lines(current_output,kind(error),[Lines]),
	asserta(wenet_do_actions_status(1)),
	backtrace(100)
	.

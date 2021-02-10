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
:- use_module(library(http/http_ssl_plugin)).

:- dynamic
	wenet_read_json_from_file/2,
	wenet_get_json_from_url/2,
	wenet_post_json_to_url/2,
	wenet_post_json_to_url/3,
	wenet_log_trace/2,
	wenet_log_error/3
	.


%!  wenet_read_json_from_file(+FilePath, -Json)
%
%	Read a json file and convert into a list.
%
%	@param FilePath string with the path to the JSON file.
%	@param Json dictionary with the data on the JSON file.
%
wenet_read_json_from_file(FilePath, Json) :-
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
	wenet_log_trace("HERE",Json),
	atom_json_dict(Atom, Json, []),
	wenet_log_trace("HERE",Atom),
	append(Headers,[header(content,'application/json')],PostHeaders),
	http_post(Url, Atom, Result, PostHeaders),
	format(string(Log_Text),'Post to ~w the callback message ~w',[Url,Atom]),
	wenet_log_trace(Log_Text,Result).


%!	wenet_log_trace(-Text,-Term)
%
%	Write a trace log message into the output console.
%
%	@param Text string message of the log.
%	@param Term Term to show into the log message.
%
wenet_log_trace(Text,Term) :-
	format(string(Lines),'~w~n~w~n',[Text,Term]),
	print_message_lines(current_output,kind(trace),[Lines])
	.

%!	wenet_log_error(-Text,-Term,-Error)
%
%	Write an error log message into the output console.
%
%	@param Text string message of the log.
%	@param Term to show into the log message.
%	@param Error to show into the log message.
%
wenet_log_error(Text,Term,Error) :-
	message_to_string(Error,ErrorMessage),
	format(string(Lines),'~w~n~w~n~w~n',[Text,Term,ErrorMessage]),
	print_message_lines(current_output,kind(error),[Lines])
	.

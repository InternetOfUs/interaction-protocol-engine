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
	wenet_configuration/1,
	wenet_message/1,
	wenet_user_language/2,
	wenet_user_language/1
	.


%!  wenet_read_json_from_file(+FilePath, -JsonDictionary)
%
%	Read a json file and convert into a dictionary.
%
%	@param FilePath string with the path to the JSON file.
%	@param JsonDictionary dictionary with the data on the JSON file.
%
wenet_read_json_from_file(FilePath, JsonDictionary) :-
	open(FilePath, read, Stream),
	json_read_dict(Stream, JsonDictionary),
	close(Stream)
	.

%!  wenet_read_json_from_url(+Url, -JsonDictionary)
%
%	Read a json from an URL and convert into a dictionary.
%
%	@param Url string to the resource with the JSON model.
%	@param JsonDictionary dictionary with the data on the JSON resource.
%
wenet_read_json_from_url(Url, JsonDictionary) :-
	http_open(Url,Stream,[]),
	json_read_dict(Stream, JsonDictionary),
  	close(Stream)
  	.

%!	wenet_build_url(+Url,-Strings)
%
%	Create the URL from a list of strings.
%
%	@param Url string the created URL.
%	@param Strings list of string to create the url.
%
wenet_build_url("",[]).
wenet_build_url(Url,[H|T]) :-
	wenet_build_url(Partial,T),
	string_concat(H,Partial,Url)
	.

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

%!	wenet_do_actions(+Actions)
%
%	Do the specified actions.
%
%	@param Actions to execute.
%
wenet_do_actions([]).
wenet_do_actions([A|O]) :-
	wenet_log_trace("Try to do action",A),
	(wenet_do_action(A),wenet_log_trace("Done action",A);wenet_log_trace("Cannot do action",A)),
	wenet_do_actions(O)
	.

wenet_do_action(put(msg_to(X,C))) :-
	wenet_user_language(Lang),
	message(C,Lang,Text),
	put_callback(message{type:"textualMessage",recipientId:X,title:'',text:Text},_)
	.

wenet_do_action(A) :-
	wenet_log_trace("Action not defined",A)
	.


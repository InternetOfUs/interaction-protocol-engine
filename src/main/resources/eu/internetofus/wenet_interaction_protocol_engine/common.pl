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
	wenet_execute_safetly_once/1,
	wenet_read_json_from_file/2,
	wenet_get_json_from_url/2,
	wenet_post_json_to_url/3,
	wenet_post_json_to_url/2,
	wenet_put_json_to_url/2,
	wenet_patch_json_to_url/2,
	wenet_log_trace/2,
	wenet_log_trace/1,
	wenet_log_error/3,
	wenet_log_error/2,
	wenet_log_error/1,
	wenet_remove/3,
	wenet_add/3
	.

:- autoload(library(http/json)).
:- autoload(library(http/http_open)).
:- autoload(library(http/http_client)).
:- autoload(library(http/http_ssl_plugin)).
:- autoload(library(ssl)).
:- autoload(library(prolog_stack)).
:- autoload(library(lists)).


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
	print_message_lines(user_output,kind(trace),[Lines])
	.

%!	wenet_log_error(-Text)
%
%	Write an error log message into the output console.
%
%	@param Text string message of the log.
%
wenet_log_error(Text) :-
	wenet_print_error('~w',[Text])
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
	print_message_lines(user_error,kind(error),[Lines]),
	!,
	asserta(wenet_do_actions_status(1))
	.

%!	wenet_execute_safetly_once(+Term)
%
%	Execute a term only one time and capture any error that happens.
%
%	@param Term to execute.
%
wenet_execute_safetly_once(Term) :-
	catch_with_backtrace(
		(
		call(Term)->wenet_log_trace('Executed',[Term]);wenet_log_error('Cannot execute',[Term])
		)
		,Error
		,wenet_log_error('Cannot execute',[Term],Error)
	).

%!	wenet_read_json_from_file(-Json,+FilePath)
%
%	Read a json file and convert into a list.
%
%	@param FilePath string with the path to the JSON file.
%	@param Json term with the data on the JSON file.
%
wenet_read_json_from_file(Json,FilePath) :-
	catch(
	(
		(
			open(FilePath, read, Stream),
			json_read(Stream, Json),
			close(Stream)
		)
			->true
			;(
				wenet_log_error('Cannot READ',[FilePath])
				,false
			)
	)
	,Error
	,(
		wenet_log_error('Cannot READ',[FilePath],Error)
		,false
	)),
	wenet_log_trace('READ',[FilePath,Json])
	.

%!	wenet_get_json_from_url(+Url, -Json)
%
%	Get a json from an URL.
%
%	@param Url string to the resource with the JSON model.
%	@param Json term with the data on the JSON resource.
%
wenet_get_json_from_url(Url, Json) :-
	wenet_component_auth_header(AuthHeader),
	nonvar(Url),
	nonvar(AuthHeader),
	catch(
	(
		http_get(Url,Result,[AuthHeader,request_header('Accept'='application/json; charset=UTF-8')])
			->true
			;(
				wenet_log_error('Cannot GET',[Url])
				,false
			)
	)
	,Error
	,(
		wenet_log_error('Cannot GET',[Url],Error)
		,false
	)),
	wenet_log_trace('GET',[Url,Result]),
	atom_json_term(Result, Json, [])
	.

%!	wenet_post_json_to_url(-Json,+Url,+Body)
%
%	Post a json into an URL.
%
%	@param Url string to the post the Json.
%	@param Body JSON term to post.
%	@param Json JSON term that return the post.
%
wenet_post_json_to_url(Json, Url, Body) :-
	wenet_component_auth_header(AuthHeader),
	nonvar(AuthHeader),
	wenet_post_json_to_url(Json,Url,Body,[AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')])
	.

%!	wenet_post_json_to_url(-Json,+Url,+Body,+Headers)
%
%	Post a json into an URL.
%
%	@param Url string to the post the Json.
%	@param Body JSON term to post.
%	@param Json JSON term that return the post.
%	@param Header for the post action.
%
wenet_post_json_to_url(Json, Url, Body, Header)  :-
	atom_json_term(AtomBody, Body, []),
	nonvar(Url),
	nonvar(AtomBody),
	catch(
	(
		http_post(Url,atom('application/json',AtomBody),Result,Header)
			->true
			;(
				wenet_log_error('Cannot POST',[Url,AtomBody])
				,false
			)
	)
	,Error
	,(
		wenet_log_error('Cannot POST',[Url,AtomBody],Error)
		,false
	)),
	wenet_log_trace('POST',[Url,AtomBody,Result]),
	atom_json_term(Result, Json, [])
	.

%!	wenet_put_json_to_url(-Json,+Url,+Body)
%
%	Put a json into an URL.
%
%	@param Url string to the put the Json.
%	@param Body JSON term to put.
%	@param Json JSON term that return the put.
%
wenet_put_json_to_url(Json, Url, Body) :-
	wenet_component_auth_header(AuthHeader),
	atom_json_term(AtomBody, Body, []),
	nonvar(Url),
	nonvar(AtomBody),
	nonvar(AuthHeader),
	catch(
	(
		http_put(Url,atom('application/json',AtomBody),Result,[AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')])
			->true
			;(
				wenet_log_error('Cannot PUT',[Url,AtomBody])
				,false
			)
	)
	,Error
	,(
		wenet_log_error('Cannot PUT',[Url,AtomBody],Error)
		,false
	)),
	wenet_log_trace('PUT',[Url,AtomBody,Result]),
	atom_json_term(Result, Json, [])
	.

%!	wenet_patch_json_to_url(-Json,+Url,+Body)
%
%	Patch a json into an URL.
%
%	@param Url string to the patch the Json.
%	@param Body JSON term to patch.
%	@param Json JSON term that return the patch.
%
wenet_patch_json_to_url(Json, Url, Body) :-
	wenet_component_auth_header(AuthHeader),
	atom_json_term(AtomBody, Body, []),
	nonvar(Url),
	nonvar(AtomBody),
	nonvar(AuthHeader),
	catch(
	(
		http_patch(Url,atom('application/json',AtomBody),Result,[AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')])
			->true
			;(
				wenet_log_error('Cannot PATCH',[Url,AtomBody])
				,false
			)
	)
	,Error
	,(
		wenet_log_error('Cannot PATCH',[Url,AtomBody],Error)
		,false
	)),
	wenet_log_trace('PATCH',[Url,AtomBody,Result]),
	atom_json_term(Result, Json, [])
	.

%!	wenet_remove(-Result,+Element,+List)
%
%	Remove an element from a list.
%
%	@param Result the list without the element.
%	@param Element to remove.
%	@param List to removed the element of a list.
%
wenet_remove(Result,Element,List) :-
	once(selectchk(Element,List,Result))
	.

%!	wenet_add(-Result,+Element,+List)
%
%	Add an element into a list.
%
%	@param Result the list without the element.
%	@param Element to add.
%	@param List to addd the element of a list.
%
wenet_add(Result,Element,List) :-
	append([List,[Element]], Result)
	.

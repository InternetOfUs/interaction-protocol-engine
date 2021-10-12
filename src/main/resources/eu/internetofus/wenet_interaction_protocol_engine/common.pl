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
	wenet_do_actions_status/1,
	wenet_execute_safetly_once/1,
	wenet_read_json_from_file/2,
	wenet_get_json_from_url/2,
	wenet_post_json_to_url/2,
	wenet_put_json_to_url/2,
	wenet_patch_json_to_url/2,
	wenet_log_trace/2,
	wenet_log_trace/1,
	wenet_log_error/3,
	wenet_log_error/2,
	wenet_log_error/1,
	wenet_remove/3,
	wenet_add/3,
	wenet_format/3,
	wenet_math/2,
	wenet_delete_to_url/1,
	wenet_is_json_null/1,
	wenet_add_query_params_to_url/3,
	wenet_new_normalized_value/3,
	wenet_user_id_from_normalized/2,
	wenet_value_from_normalized/2,
	wenet_initialize_normalized_users/3,
	wenet_negate_normalized/2
	.

:- autoload(library(http/json)).
:- autoload(library(http/http_open)).
:- autoload(library(http/http_client)).
:- autoload(library(http/http_ssl_plugin)).
:- autoload(library(ssl)).
:- autoload(library(prolog_stack)).
:- autoload(library(lists)).
:- autoload(library(pcre)).


%!	wenet_log_trace(-Text)
%
%	Write a trace log message into the output console.
%
%	@param Text string message of the log.
%
wenet_log_trace(Text) :-
	format(string(Lines),'TRACE: ~w',Text),
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
	term_string(Term,Value),
	format(string(Lines),'TRACE: ~w ~w',[Text,Value]),
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
	term_string(Terms,Value),
	wenet_print_error('~w ~w',[Text,Value])
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
	term_string(Terms,Value),
	wenet_print_error('~w. ~w ~w',[ErrorMessage,Text,Value])
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

%!	wenet_log_warning(-Text)
%
%	Write an warning log message into the output console.
%
%	@param Text string message of the log.
%
wenet_log_warning(Text) :-
	wenet_print_warning('~w',[Text])
	.

%!	wenet_log_warning(-Text,-Terms)
%
%	Write an warning log message into the output console.
%
%	@param Text string message of the log.
%	@param Terms arguments to show into the log message.
%
wenet_log_warning(Text,Terms) :-
	term_string(Terms,Value),
	wenet_print_warning('~w ~w',[Text,Value])
	.

%!	wenet_log_warning(-Text,-Terms,-Warning)
%
%	Write an warning log message into the output console.
%
%	@param Text string message of the log.
%	@param Terms arguments to show into the log message.
%	@param Warning to show into the log message.
%
wenet_log_warning(Text,Terms,Warning) :-
	message_to_string(Warning,WarningMessage),
	term_string(Terms,Value),
	wenet_print_warning('~w. ~w ~w',[WarningMessage,Text,Value])
	.

%!	wenet_print_warning(-Lines)
%
%	Print warning message lines.
%
%	@param Format to show into the log warning message.
%	@param Arguments to show into the log warning message.
%
wenet_print_warning(Format,Arguments) :-
	format(string(Lines),Format,Arguments),
	print_message_lines(user_warning,kind(warning),[Lines])
	.

%!	wenet_execute_safetly_once(+Term)
%
%	Execute a term only one time and capture any error that happens.
%
%	@param Term to execute.
%
wenet_execute_safetly_once(Term) :-
	wenet_log_trace('BEGIN execute',[Term]),
	catch_with_backtrace(
		(
		call(Term)->wenet_log_trace('END Executed',[Term]);wenet_log_error('END FAIL execute',[Term])
		)
		,Error
		,wenet_log_error('END FAIL execute',[Term],Error)
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
			->
				true
			;(
				wenet_log_error('Cannot READ',[FilePath])
				,false
			)
		)
		,Error
		,(
			wenet_log_error('Cannot READ',[FilePath],Error)
			,false
		)
	),
	wenet_log_trace('READ',[FilePath,Json])
	.

%!	wenet_get_json_from_url(-Json,+Url)
%
%	Get a json from an URL.
%
%	@param Json term with the data on the JSON resource.
%	@param Url string to the resource with the JSON model.
%
wenet_get_json_from_url(Json,Url) :-
	catch(
		(
			(
				wenet_component_auth_header(AuthHeader),
				Options = [AuthHeader,request_header('Accept'='application/json; charset=UTF-8')],
				http_get(Url,Result,Options),
				atom_json_term(Result, Json, [])
			)
			->
				true
			;(
				wenet_log_error('Cannot GET',Url)
				,false
			)
		)
		,Error
		,(
			wenet_log_error('Cannot GET',Url,Error)
			,false
		)		
	),
	wenet_log_trace('GET',[Url,Json])
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
	catch(
		(
			(
				wenet_component_auth_header(AuthHeader),
				atom_json_term(AtomBody, Body, []),
				Data = atom('application/json',AtomBody),
				Options = [AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')],
				http_post(Url,Data,Result,Options),
				atom_json_term(Result, Json, [])
			)
			->
				true
			;(
				wenet_log_error('Cannot POST',[Url,Body])
				,false
			)
		)
		,Error
		,(
			wenet_log_error('Cannot POST',[Url,Body],Error)
			,false
		)		
	),
	wenet_log_trace('POST',[Url,Body,Json])
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
	catch(
		(
			(
				wenet_component_auth_header(AuthHeader),
				atom_json_term(AtomBody, Body, []),
				Data = atom('application/json',AtomBody),
				Options = [AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')],
				http_put(Url,Data,Result,Options),
				atom_json_term(Result, Json, [])
			)
			->
				true
			;(
				wenet_log_error('Cannot PUT',[Url,Body])
				,false
			)
		)
		,Error
		,(
			wenet_log_error('Cannot PUT',[Url,Body],Error)
			,false
		)		
	),
	wenet_log_trace('PUT',[Url,Body,Json])
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
	catch(
		(
			(
				wenet_component_auth_header(AuthHeader),
				atom_json_term(AtomBody, Body, []),
				Data = atom('application/json',AtomBody),
				Options = [AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')],
				http_patch(Url,Data,Result,Options),
				atom_json_term(Result, Json, [])
			)
			->
				true
			;(
				wenet_log_error('Cannot PATCH',[Url,Body])
				,false
			)
		)
		,Error
		,(
			wenet_log_error('Cannot PATCH',[Url,Body],Error)
			,false
		)		
	),
	wenet_log_trace('PATCH',[Url,Body,Json])
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

%!	wenet_format(-Msg,+Format,+Arguments)
%
%	@param Msg formatted message.
%	@param WeNetFormat for the message.
%	@param Arguments to apply over the format.
%
wenet_format(Msg,WeNetFormat,Arguments) :-
	nonvar(WeNetFormat),
	nonvar(Arguments),
	is_list(Arguments)->
		(
			re_replace('{}', '~w', WeNetFormat,Format),
			format(string(Msg),Format,Arguments)
		)
		;wenet_format(Msg,WeNetFormat,[Arguments])
	.

%!	wenet_math(-Number,+Expr)
%
%	Return the evaluation of a mathematical expression.
%
%	@param Number result of the expression.
%	@param Expr mathematical expression to evaluate.
%
wenet_math(Number,Expr) :-
	Number is Expr
	.
	
%!	wenet_delete_to_url(+Url)
%
%	Delete an URL.
%
%	@param Url string to the delete.
%
wenet_delete_to_url(Url) :-
	catch(
		(
			(
				wenet_component_auth_header(AuthHeader),
				Options = [AuthHeader,request_header('Accept'='application/json; charset=UTF-8'),request_header('Content-Type'='application/json')],
				http_delete(Url,_,Options)
			)
			->
				true
			;(
				wenet_log_error('Cannot DELETE',Url)
				,false
			)
		)
		,Error
		,(
			wenet_log_error('Cannot DELETE',Url,Error)
			,false
		)		
	),
	wenet_log_trace('DELETE',Url)
	.

%!	wenet_is_json_null(+V)
%
%	Check if a json value is a {@code null}.
%
wenet_is_json_null(V) :-
	V = @(null)
	.
	
%!	wenet_add_query_params_to_url(-UrlWithParams,+Url,+Params)
%
%	Create an URL with the query parameters.
%
%	@param UrlWithParams the url with the parameters.
%	@param Url to add the query parameters.
%	@param Params the query parameters.
%
wenet_add_query_params_to_url(UrlWithParams,Url,Params) :-
	exclude(wenet_is_json_null,Params,NoNullParams),
	(length(NoNullParams, 0)
		-> UrlWithParams = Url
		; uri_query_components(QueryParams,NoNullParams),
		 atomics_to_string([Url|['?',QueryParams]],UrlWithParams)
	)
	.

%!	wenet_normalized_value(-Normalized,+UserId,+Value)
%
%	Create a new normlaized value.
%
%	@param Normalized the JSON normalized value.
%	@param UserId identifier of the user.
%	@param Value normalized value of the user.
%
wenet_new_normalized_value(Normalized,UserId,Value) :-
	Normalized = json([userId=UserId,value=Value])
	.
	
%!	wenet_user_id_from_normalized(-UserId,+Normalized)
%
%	Get the normlaized user identifier of the model.
%
%	@param UserId identifier of the user.
%	@param Normalized the JSON normalized value.
%
wenet_user_id_from_normalized(UserId,json(Normalized)) :-
	member(userId=UserId,Normalized)
	.

%!	wenet_value_from_normalized(-Value,+Normalized)
%
%	Get the normlaized value of the model.
%
%	@param Value of the user.
%	@param Normalized the JSON normalized value.
%
wenet_value_from_normalized(Value,json(Normalized)) :-
	member(value=Value,Normalized)
	.

%!	wenet_initialize_normalized_users(-List,+Users,-Value)
%
%	Create a list of normalized values for some users.
%
%	@param List with the normalized values.
%	@param Users to the user.
%	@param Value to set for all the users.
%
wenet_initialize_normalized_users([],[],_).
wenet_initialize_normalized_users([Normalized|List],[UserId|Users],Value) :-
	wenet_new_normalized_value(Normalized,UserId,Value),
	wenet_initialize_normalized_users(List,Users,Value)
	.

%!	wenet_negate_normalized(-Target,+Source)
%
%	REverse the value sof a normalized list.
%
%	@param Target list where the normalized vlaues are negated (1 - value).
%	@param Source list to get the values to negate.
%
wenet_negate_normalized([],[]).
wenet_negate_normalized([Target|TargetRest],[Source|SourceRest]) :-
	wenet_user_id_from_normalized(UserId,Source),
	wenet_value_from_normalized(Value,Source),
	Negate is 1.0 - Value,
	wenet_new_normalized_value(Target,UserId,Negate),
	wenet_negate_normalized(TargetRest,SourceRest)
	.
	

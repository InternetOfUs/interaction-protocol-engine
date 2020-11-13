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
	get_app/1,
	get_app/2,
	get_app_users/1,
	get_app_users/2.

%!	get_service_url_to(+Url,-Paths)
%
%	Calculate the URL from a path	
%
get_service_url_to(Url,Paths) :-
	get_configuration(Configuration),
	create_url(Url,[Configuration.wenetComponents.service|Paths])
	.
	

%!	get_app(+App,-Id)
%
%	Return the app associated to an identifier.
%
%	@param App dictionary with the app information.
%	@param Id string identifeir of the app to obtain.
%
get_app(App,Id) :-
	get_service_url_to(Url,["/app/",Id]),
	get_dict_from_json_url(Url,App),
	asserta(get_app(App,Id)),
	log_trace("Loaded application",App)
	.


%!	get_app(+App)
%
%	Return the app associated to the engine.
%
%	@param App dictionary with the app information.
%
get_app(App) :-
	get_message(Message),
	get_app(App,Message.appId),
	asserta(get_app(App)) 
	.

	
%!	get_app_users(+Service,-Id)
%
%	Return the users of an application.
%
%	@param Users list of string with the user identifiers of the application.
%	@param Id string identifeir of the application to obtain.
%
get_app_users(Users,Id) :-
	get_service_url_to(Url,["/app/",Id,"/users"]),
	get_dict_from_json_url(Url,Users),
	asserta(get_app_users(Users,Id)),
	format(string(Log_Text),'Loaded users of the application ~w',[Id]),
	log_trace(Log_Text,Users)
	.


%!	get_app(+App)
%
%	Return the app associated to the engine.
%
%	@param App dictionary with the app information.
%
get_app_users(Users) :-
	get_message(Message),
	get_app_users(Users,Message.appId),
	asserta(get_app_users(Users)) 
	.
	
%!	put_callback(+App,+Message,-Result)
%
%	Do a callback into an application.
%
%	@param App to do the callback.
%	@param Message to post.
%	@param Result of teh callback.
%
put_callback(App,Message,Result) :-
	atom_json_term(Atom, Message, []),
	http_post(App.messageCallbackUrl, atom(Atom), Result, []),
	format(string(Log_Text),'Post to the app ~w the callback message ~w',[App.appId,Atom]),
	log_trace(Log_Text,Result)
	.
	
%!	put_callback(+Message,-Result)
%
%	Do a callback into the curretn application.
%
%	@param Message to post.
%	@param Result of teh callback.
%
put_callback(Message,Result) :-
	get_app(App),
	put_callback(App,Message,Result)
	.
	
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
	get_configuration/1,
	get_message/1,
	get_language/2,
	get_language/1
	.


%!  get_dict_from_json_file(+FilePath, -JsonDictionary)
%
%	Read a json file and convert into a dictionary.
%
%	@param FilePath string with the path to the JSON file. 
%	@param JsonDictionary dictionary with the data on the JSON file.
%
get_dict_from_json_file(FilePath, JsonDictionary) :-
	open(FilePath, read, Stream), 
	json_read_dict(Stream, JsonDictionary), 
	close(Stream)
	.

%!  get_dict_from_json_url(+Url, -JsonDictionary)
%
%	Read a json from an URL and convert into a dictionary.
%
%	@param Url string to the resource with the JSON model.
%	@param JsonDictionary dictionary with the data on the JSON resource.
%
get_dict_from_json_url(Url, JsonDictionary) :-
	http_open(Url,Stream,[]),
	json_read_dict(Stream, JsonDictionary), 
  	close(Stream)
  	.

%!	get_configuration(+Configuration)
%
%	Return the configuration of the interaction protocol engine.
%
%	@param Configuration dictionary of the interaction protocol engine.
%
get_configuration(Configuration) :-
	configuration_file(File),
	get_dict_from_json_file(File,Configuration),
	asserta(get_configuration(Configuration)),
	log_trace("Loaded configuration",Configuration)
	.

%!	get_message(+Message)
%
%	Return the message of the current engine.
%
%	@param Message dictionary with the message of the current session.
%
get_message(Message) :-
	message_file(File),
	get_dict_from_json_file(File,Message),
	asserta(get_message(Message)),
	log_trace("Loaded message",Message)
	.

%!	create_url(+Url,-Strings)
%
%	Create the URL from a list of strings.
% 
%	@param Url string the created URL.
%	@param Strings list of string to create the url. 
% 
create_url("",[]).
create_url(Url,[H|T]) :-
	create_url(Partial,T),
	string_concat(H,Partial,Url)
	.

%!	get_language(+Lang,-Profile)
%
%	Return the language to use for a profile.
%
%	@param Lang string the language to use for the profile.
%	@param Profile dict to get the language to use.
% 
get_language(Lang,Profile) :-
        catch(sub_atom(Profile.locale,0,2,_,Lang),_,Lang="en"),
        asserta(get_language(Lang,Profile))
        .
get_language("en",_).


%!	get_language(+Lang)
%
%	Return the language to use for the current user.
%
%	@param Lang string the language of the current user.
% 
get_language(Lang) :-
        get_profile(Profile),
        catch(get_language(Lang,Profile),_,Lang="en"),
        asserta(get_language(Lang))
        .
        
%!	log_trace(-Text,-Term)
%
%	Write a trace log message into the output console.
%
%	@param Text string message of the log.
%	@param Term Term to show into the log message.
%        
log_trace(Text,Term) :-
	format(string(Lines),'~w~n~w~n',[Text,Term]),
    print_message_lines(current_output,kind(trace),[Lines])
	.
	
%!	do_actions(+Actions)
%
%	Do the specified actions.
%
%	@param Actions to execute.
%
do_actions([]).
do_actions([A|O]) :-
	log_trace("Do action",A),
	(do_action(A);true),
	do_actions(O)
	.

do_action(put(msg_to(X,C))) :-
	get_language(Lang),
	message(C,Lang,Text),
	put_callback(message{type:"textualMessage",recipientId:X,title:'',text:Text},_)
	.
	
do_action(A) :-
	log_trace("Action not defined",A)
	.
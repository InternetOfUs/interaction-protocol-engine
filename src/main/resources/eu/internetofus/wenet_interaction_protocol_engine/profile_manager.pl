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
	get_profile/1,
	get_profile/2,
	get_community/1,
	get_community/2.

%!	get_profile_manager_url_to(+Url,-Paths)
%
%	Calculate the URL from a path
%
get_profile_manager_url_to(Url,Paths) :-
	wenet_profile_manager_api_url(Api),
	wenet_build_url(Url,[Api|Paths])
	.


%!	get_profile(+Profile,-Id)
%
%	Return the profile associated to an identifier.
%
%	@param Profile dictionary with the profile information.
%	@param Id string identifeir of the profile to obtain.
%
get_profile(Profile,Id) :-
	get_profile_manager_url_to(Url,["/profiles/",Id]),
	wenet_read_json_from_url(Url,Profile),
	asserta(get_profile(Profile,Id)),
	wenet_log_trace("Loaded profile",Profile)
	.


%!	get_profile(+Profile)
%
%	Return the profile associated to the engine.
%
%	@param Profile dictionary with the profile information.
%
get_profile(Profile) :-
	wenet_message(Message),
	(Message.sender.component == "USER_APP" -> get_profile(Profile,Message.sender.userId); get_profile(Profile,Message.receiver.userId)),
	asserta(get_profile(Profile))
	.


%!	get_community(+Community,-Id)
%
%	Return the community associated to an identifier.
%
%	@param Community dictionary with the community information.
%	@param Id string identifeir of the community to obtain.
%
get_community(Community,Id) :-
	get_profile_manager_url_to(Url,["/communities/",Id]),
	wenet_read_json_from_url(Url,Community),
	asserta(get_community(Community,Id)),
	wenet_log_trace("Loaded community",Community)
	.


%!	get_community(+Community)
%
%	Return the community associated to the engine.
%
%	@param Community dictionary with the community information.
%
get_community(Community) :-
	wenet_message(Message),
	get_community(Community,Message.communityId),
	asserta(get_community(Community))
	.

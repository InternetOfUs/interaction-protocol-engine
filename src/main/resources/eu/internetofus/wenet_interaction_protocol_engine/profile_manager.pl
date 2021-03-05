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
	wenet_profile_manager_api_url_to/2,
	wenet_profile_manager_get_profile/2,
	wenet_profile_manager_get_community/2,
	get_profile_id/2,
	get_community_id/2
	.


%!	wenet_profile_manager_api_url_to(+Url,-Paths)
%
%	Calculate the URL from a path
%
wenet_profile_manager_api_url_to(Url,Paths) :-
	wenet_profile_manager_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.


%!	get_profile(+Profile,-Id)
%
%	Return the profile associated to an identifier.
%
%	@param Profile list with the profile information.
%	@param Id string identifeir of the profile to obtain.
%
wenet_profile_manager_get_profile(Profile,Id) :-
	wenet_profile_manager_api_url_to(Url,['/profiles/',Id]),
	wenet_get_json_from_url(Url,Profile),
	asserta(wenet_profile_manager_get_profile(Profile,Id)),
	wenet_log_trace('Loaded profile',Profile)
	.

%!	get_community(+Community,-Id)
%
%	Return the community associated to an identifier.
%
%	@param Community list with the community information.
%	@param Id string identifeir of the community to obtain.
%
wenet_profile_manager_get_community(Community,Id) :-
	wenet_profile_manager_api_url_to(Url,['/communities/',Id]),
	wenet_get_json_from_url(Url,Community),
	asserta(wenet_profile_manager_get_community(Community,Id)),
	wenet_log_trace('Loaded community',Community)
	.

%!	get_profile_id(-Id,+Profile)
%
%	Obtain the id of a profile.
%
%	@param Id of the profile.
%	@param Profile to get the id.
%
get_profile_id(Id, json(Profile)) :-
	member(id=Id,Profile)
	.

%!	get_community_id(-Id,+Community)
%
%	Obtain the id of a community.
%
%	@param Id of the community.
%	@param Community to get the id.
%
get_community_id(Id, json(Community)) :-
	member(id=Id,Community)
	.

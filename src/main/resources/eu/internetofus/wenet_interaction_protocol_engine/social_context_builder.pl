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
	wenet_social_context_builder_url_to/2,
	wenet_social_context_builder_update_preferences/3,
	wenet_social_context_builder_retrieve_social_explanation/3,
	wenet_description_of_social_explanation/2,
	wenet_summary_of_social_explanation/2
	.


%!	get_social_context_builder_url_to(-Url,+Paths)
%
%	Calculate the URL to interact to the specified path of the social context builder.
%
wenet_social_context_builder_url_to(Url,Paths) :-
	wenet_social_context_builder_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_social_context_builder_update_preferences(+UserId,+TaskId,+Users)
%
%	Update the preferences of an user.
%
%	@param UserId identifier of the user.
%	@param TaskId identifier of the task.
%	@param Users the identifier of the volunteers of the task.
%
wenet_social_context_builder_update_preferences(UserId,TaskId,Users) :-
	wenet_social_context_builder_url_to(Url,['/social/preferences/',UserId,'/',TaskId,'/']),
	wenet_post_json_to_url(_,Url,Users)
	.

%!	wenet_social_context_builder_retrieve_social_explanation(-SocialExplanation,+UserId,+TaskId)
%
%	Update the preferences of an user.
%
%	@param SocialExplanation for the user.
%	@param UserId identifier of the user.
%	@param TaskId identifier of the task.
%
wenet_social_context_builder_retrieve_social_explanation(SocialExplanation,UserId,TaskId) :-
	wenet_social_context_builder_url_to(Url,['/social/explanations',UserId,'/',TaskId,'/']),
	wenet_get_json_from_url(SocialExplanation,Url)
	.

%!	wenet_description_of_social_explanation(Description,SocialExplanation)
%
%	Get the description of the social explanation.
%
%	@param Description of the social explanation.
%	@param SocialExplanation to get the description.
%
wenet_description_of_social_explanation(Description,json(SocialExplanation)) :-
	member(description=Description,SocialExplanation)
	.

%!	wenet_summary_of_social_explanation(Summary,SocialExplanation)
%
%	Get the summary of the social explanation.
%
%	@param Summary of the social explanation.
%	@param SocialExplanation to get the summary.
%
wenet_summary_of_social_explanation(Summary,json(SocialExplanation)) :-
	member('Summary'=Summary,SocialExplanation)
	.

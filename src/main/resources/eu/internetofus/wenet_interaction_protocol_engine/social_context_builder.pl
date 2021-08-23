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
	wenet_social_context_builder_url_to(Url,['/social/explanations/',UserId,'/',TaskId,'/']),
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

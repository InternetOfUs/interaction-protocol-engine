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
	wenet_profile_manager_api_url_to/2,
	wenet_profile_manager_get_profile/2,
	wenet_profile_manager_get_community/2,
	wenet_id_of_profile/2,
	wenet_id_of_community/2,
	wenet_relationships_of_profile/2,
	wenet_user_id_of_relationship/2,
	wenet_app_id_of_relationship/2,
	wenet_weight_id_of_relationship/2,
	wenet_profile_manager_operations_calculate_diversity/2,
	wenet_new_diversity_data/3,
	wenet_profile_manager_operations_calculate_similarity/2,
	wenet_new_similarity_data/3
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
%	@param Id string identifier of the profile to obtain.
%
wenet_profile_manager_get_profile(Profile,Id) :-
	wenet_profile_manager_api_url_to(Url,['/profiles/',Id]),
	wenet_get_json_from_url(Profile,Url)
	.

%!	get_community(+Community,-Id)
%
%	Return the community associated to an identifier.
%
%	@param Community list with the community information.
%	@param Id string identifier of the community to obtain.
%
wenet_profile_manager_get_community(Community,Id) :-
	wenet_profile_manager_api_url_to(Url,['/communities/',Id]),
	wenet_get_json_from_url(Community,Url)
	.

%!	wenet_id_of_profile(-Id,+Profile)
%
%	Obtain the identifier of a profile.
%
%	@param Id of a profile.
%	@param Profile to get the identifier.
%
wenet_id_of_profile(Id, json(Profile)) :-
	member(id=Id,Profile)
	.

%!	wenet_id_of_community(-Id,+Community)
%
%	Obtain the identifier of a community.
%
%	@param Id of a community.
%	@param Community to get the identifier.
%
wenet_id_of_community(Id, json(Community)) :-
	member(id=Id,Community)
	.

%!	wenet_relationships_of_profile(-Relationships,+Profile)
%
%	Obtain the relationshipsentifier of a profile.
%
%	@param Relationships of a profile.
%	@param Profile to get the relationshipsentifier.
%
wenet_relationships_of_profile(Relationships, json(Profile)) :-
	member(relationships=Relationships,Profile)
	.

%!	wenet_user_id_of_relationship(-UserId,+Relationship)
%
%	Obtain the user identifier defined on a social relationship.
%
%	@param UserId of a relationship.
%	@param Relationship to get the user identifier.
%
wenet_user_id_of_relationship(UserId, json(Relationship)) :-
	member(userId=UserId,Relationship)
	.

%!	wenet_app_id_of_relationship(-AppId,+Relationship)
%
%	Obtain the app identifier defined on a social relationship.
%
%	@param AppId of a relationship.
%	@param Relationship to get the app identifier.
%
wenet_app_id_of_relationship(AppId, json(Relationship)) :-
	member(appId=AppId,Relationship)
	.

%!	wenet_weight_of_relationship(-Weight,+Relationship)
%
%	Obtain the weight defined on a social relationship.
%
%	@param Weight of a relationship.
%	@param Relationship to get the weight.
%
wenet_weight_of_relationship(Weight, json(Relationship)) :-
	member(weight=Weight,Relationship)
	.

%!	wenet_profile_manager_operations_calculate_diversity(-Diversity,+Data)
%
%	Obtain the diversity form a set of users.
%
%	@param Diversity of the users.
%	@param Data JSON with the information of the users to calculate the diversity.
%
wenet_profile_manager_operations_calculate_diversity(Diversity,Data) :-
	wenet_profile_manager_api_url_to(Url,['/operations/diversity']),
	wenet_post_json_to_url(json(Result),Url,Data),
	member(diversity=Diversity,Result)
	.
	
	
%!	wenet_new_diversity_data(-Data,+UserIds,+AttributeNames)
%
%	reate the data necessary to calculate the diversity for some users.
%
%	@param Data JSON with the information of the users to calculate the diversity.
%	@param UserIds list of strings with the identifier of the users to calculate the diversity.
%	@param AttributeNames list of strings with the names for the attributes to calculate the diversity.
%
wenet_new_diversity_data(Data,UserIds,AttributeNames) :-
	Data = json([userIds=UserIds,attributes=AttributeNames])
	.
	
%!	wenet_profile_manager_operations_calculate_similarity(-Similarity,+Data)
%
%	Obtain the similarity form a set of attributes.
%
%	@param Similarity JSON with the attribute as name and a value its similarity.
%	@param Data JSON with the information of the attributes to calculate the similarity.
%
wenet_profile_manager_operations_calculate_similarity(Similarity,Data) :-
	wenet_profile_manager_api_url_to(Url,['/operations/similarity']),
	wenet_post_json_to_url(json(Result),Url,Data),
	member(attributes=Similarity,Result)
	.

%!	wenet_new_diversity_data(-Data,+UserId,+Source)
%
%	reate the data necessary to calculate the diversity for some attributes.
%
%	@param Data JSON with the information of the attributes to calculate the diversity.
%	@param UserId string with the user identifier.
%	@param Source string with the text to compare.
%
wenet_new_diversity_data(Data,UserId,Source) :-
	Data = json([userId=UserId,source=Source])
	.

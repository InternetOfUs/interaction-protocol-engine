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
	wenet_social_context_builder_post_preferences/4,
	wenet_social_context_builder_retrieve_social_explanation/3,
	wenet_description_of_social_explanation/2,
	wenet_summary_of_social_explanation/2,
	wenet_social_context_builder_post_preferences_answers/4,
	wenet_user_id_of_user_answer/2,
	wenet_answer_of_user_answer/2,
	wenet_new_user_answer/3,
	wenet_social_context_builder_post_social_notification/1,
	wenet_new_user_message/6,
	wenet_social_context_builder_put_preferences_answers_update/4
	.


%!	get_social_context_builder_url_to(-Url,+Paths)
%
%	Calculate the URL to interact to the specified path of the social context builder.
%
wenet_social_context_builder_url_to(Url,Paths) :-
	wenet_social_context_builder_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_social_context_builder_post_preferences(+UserId,+TaskId,+Users)
%
%	Update the preferences of an user.
%
%   @param Ranking list of the volunteers identifiers.
%	@param UserId identifier of the user.
%	@param TaskId identifier of the task.
%	@param Users the identifier of the volunteers of the task.
%
wenet_social_context_builder_post_preferences(Ranking,UserId,TaskId,Users) :-
	wenet_social_context_builder_url_to(Url,['/social/preferences/',UserId,'/',TaskId,'/']),
	wenet_post_json_to_url(Ranking,Url,Users)
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
	
%!	wenet_social_context_builder_post_preferences_answers(-Ranking,+UserId,+TaskId,+UserAnswers)
%
%	Post the preferences answers of an user. This is used to calculate the ranking of the answers.
%
%   @param Ranking of the user answers, sorted from the best to the worst.
%	@param UserId identifier of the user.
%	@param TaskId identifier of the task.
%	@param UserAnswers the list of users answers tuple to rank.
%
wenet_social_context_builder_post_preferences_answers(Ranking,UserId,TaskId,UserAnswers) :-
	wenet_social_context_builder_url_to(Url,['/social/preferences/answers/',UserId,'/',TaskId]),
	wenet_post_json_to_url(Ranking,Url,json([data=UserAnswers]))
	.

%!	wenet_user_id_of_user_answer(-UserId,+UserAnswer)
%
%	Get the user idnetifier of a user answer.
%
%	@param UserId of the user answer.
%	@param UserAnswer to get the user identifier.
%
wenet_user_id_of_user_answer(UserId,json(UserAnswer)) :-
	member(userId=UserId,UserAnswer)
	.

%!	wenet_answer_of_user_answer(-Answer,+UserAnswer)
%
%	Get the answer of a user answer.
%
%	@param Answer of the user answer.
%	@param UserAnswer to get the answer.
%
wenet_answer_of_user_answer(Answer,json(UserAnswer)) :-
	member(answer=Answer,UserAnswer)
	.


%!	wenet_new_user_answer(-UserAnswer, +UserId, +Answer)
%
%	Obtain the id of a task.
%
%	@param UserAnswer 
%	@param UserId user idnetifier fro the model.
%	@param Answer for the model.
%
wenet_new_user_answer(UserAnswer, UserId, Answer) :-
	UserAnswer = json([userId=UserId,answer=Answer])
	.

%!	wenet_social_context_builder_post_social_notification(+Message)
%
%	This predicate is used to notify the social context builder about
%	an interaction between users.
%
%   @param Message JSON model with the interaction user message.
%
wenet_social_context_builder_post_social_notification(Message) :-
	wenet_social_context_builder_url_to(Url,['/social/notification/']),
	wenet_post_json_to_url(_,Url,Message)
	.

%!	wenet_new_user_message(-UserMessage,+TaskId,+TransactionId,+Timestamp,+SenderId,+Message)
%
%	Create a user message that can be used to notify about the interaction between users.
%
%	@param UserMessage the interaction message between the users.
%	@param TaskId the identifier of the task where the interaction is done.
%	@param TransactionId the identifier of the transaction that the message refers.
%	@param Timestamp the UTC epoch timestamp when the interaction is done.
%	@param SenderId the identifier of the user that has started the interaction.
%	@param Message that has sent to the user.
%
wenet_new_user_message(UserMessage,TaskId,TransactionId,Timestamp,SenderId,Message) :-
	UserMessage = json([taskId=TaskId,transactionId=TransactionId,timestamp=Timestamp,senderId=SenderId,message=Message])
	.

%!	wenet_social_context_builder_put_preferences_answers_update(-Updated,+UserId,+TaskId,+Selection,+UserAnswers)
%
%	Put the selected preferences answers of an user. This is used to notify witch
%	answer of the ranking is selected by the user.
%
%   @param Updated the result of the update.
%	@param UserId identifier of the user.
%	@param TaskId identifier of the task.
%	@param Selection identifier of the selected answer.
%	@param UserAnswers the ranked list of users answers where is the selected answer.
%
wenet_social_context_builder_put_preferences_answers_update(Updated,UserId,TaskId,Selection,UserAnswers) :-
	wenet_social_context_builder_url_to(Url,['/social/preferences/answers/',UserId,'/',TaskId,'/',Selection]),
	wenet_put_json_to_url(Updated,Url,json([data=UserAnswers]))
	.

%!	wenet_social_context_builder_put_preferences_answers_update(+UserId,+TaskId,+Selected,+Ranking)
%
%	Update the selected answers of a ranked answers.
%
%	@param UserId identifier of the user.
%	@param TaskId identifier of the task.
%	@param Selected index of the selected answers of the ranking.
%   @param Ranking where is the selected answer.
%
wenet_social_context_builder_put_preferences_answers_update(UserId,TaskId,Selected,Ranking) :-
	wenet_social_context_builder_url_to(Url,['/social/preferences/answers/',UserId,'/',TaskId,'/',Selected,'/update']),
	wenet_put_json_to_url(_,Url,Ranking)
	.
	


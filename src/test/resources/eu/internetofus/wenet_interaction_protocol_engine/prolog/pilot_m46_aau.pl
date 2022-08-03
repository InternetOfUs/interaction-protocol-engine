% Obtain the possible users to ask
whenever
	is_received_created_task()
	and get_app_users_to_ask(Users)
thenceforth
	add_created_transaction()
	and put_task_state_attribute('appUsers',Users)
	and send_event(_,1,'sortUsersByDiversity',json([]))
	and send_expiration_event().

:- use_module(library(random)).
:- dynamic
	get_app_users_to_ask/1,
	send_expiration_event/0.
get_app_users_to_ask(AppUsers) :-
	get_app_users_except_me(UsersExceptMe),
	random_permutation(AppUsers,UsersExceptMe).
send_expiration_event() :-
	get_task_attribute_value(ExpirationDate,'expirationDate'),
	get_now(Now), wenet_math(Delay,ExpirationDate-Now),
	send_event(TimerId,Delay,'notifyQuestionExpirationMessage',json([])),
	put_task_state_attribute('timerId',TimerId).


% Order the users by its physical closeness location
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('nearby','positionOfAnswerer')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	normalized_closeness(PhysicalClosenessUsers,Users,500)
	and put_task_state_attribute('physicalClosenessUsers',PhysicalClosenessUsers).

% Users location is not important
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('anywhere','positionOfAnswerer')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(PhysicalClosenessUsers,Users,@(null))
	and put_task_state_attribute('physicalClosenessUsers',PhysicalClosenessUsers).


% Users social closeness is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(SocialClosenessUsers,Users,@(null))
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Order the users by similar social closeness when domain is 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('academic_skills','domain')
	and get_task_attribute_value('similar','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_social_closeness(Attributes)
thenceforth
	normalized_diversity(Diversity,Users,Attributes,@(null),false)
	and wenet_negate_user_value(SocialClosenessUsers,Diversity)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

:- dynamic get_profile_attribues_by_social_closeness/1.
get_profile_attribues_by_social_closeness(['materials.department','materials.degree_programme']).


% Order the users by different social closeness when domain is 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('academic_skills','domain')
	and get_task_attribute_value('different','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_social_closeness(Attributes)
thenceforth
	normalized_diversity(SocialClosenessUsers,Users,Attributes,@(null),false)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Order the users by similar social closeness when domain is not 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and not(get_task_attribute_value('academic_skills','domain'))
	and get_task_attribute_value('similar','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	normalized_social_closeness(SocialClosenessUsers,Users,@(null))
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Order the users by different social closeness when domain is not 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and not(get_task_attribute_value('academic_skills','domain'))
	and get_task_attribute_value('different','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
    normalized_social_closeness(Socialness,Users,@(null))
	and wenet_negate_user_value(SocialClosenessUsers,Socialness)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).


% Order the users by similar beliefs
and values
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('similar','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_beliefs_and_values(Attributes)
thenceforth
	normalized_diversity(Diversity,Users,Attributes,@(null),false)
	and wenet_negate_user_value(BeliefsAndValuesUsers,Diversity)
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

:- dynamic get_profile_attribues_by_beliefs_and_values/1.
get_profile_attribues_by_beliefs_and_values(['meanings.excitement','meanings.promotion','meanings.existence','meanings.suprapersonal','meanings.interactive','meanings.normative']).

% Order the users by different beliefs and values
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_beliefs_and_values(Attributes)
thenceforth
	normalized_diversity(BeliefsAndValuesUsers,Users,Attributes,@(null),false)
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

% Users beliefs and values is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(BeliefsAndValuesUsers,Users,@(null))
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

% Order the users by domain
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('similar','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
	and get_task_attribute_value(Domain,'domain')
	and domain_attributes(Domain,Attributes)
thenceforth
	normalized_diversity(Diversity,Users,Attributes,@(null),false)
	and wenet_negate_user_value(DomainInterestUsers,Diversity)
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

:- dynamic
	domain_attributes/2.

domain_attributes('basic_needs',['competences.c_food','competences.c_accom']).
domain_attributes('campus_life',['materials.degree_programme']).
domain_attributes('academic_skills',['competences.u_active','competences.u_read','competences.u_essay','competences.u_org','competences.u_balance','competences.u_assess','competences.u_theory','competences.u_pract']).
domain_attributes('appreciating_culture',['competences.c_lit','competences.c_app_mus','competences.c_plays','competences.c_musgall']).
domain_attributes('performing_producing_culture',['competences.c_creatlit','competences.c_perf_mus','competences.c_perf_plays','competences.c_perf_art']).
domain_attributes('physical_activities_sports',['competences.c_watch_sp','competences.c_ind_sp','competences.c_team_sp']).
domain_attributes('things_to_do_about_town',['competences.c_eating','competences.c_locfac','materials.degree_programme']).
domain_attributes('random_thoughts',['competences.c_food','competences.c_eating','competences.c_lit','competences.c_createlit','competences.c_app_mus','competences.c_perf_mus','competences.c_plays','competences.c_perf_plays','competences.c_musgall','competences.c_perf_art','competences.c_watch_sp','competences.c_ind_sp','competences.c_team_sp','competences.c_accom','competences.c_locfac','competences.u_active','competences.u_read','competences.u_essay','competences.u_org','competences.u_balance','competences.u_assess','competences.u_theory','competences.u_pract']).
domain_attributes('sensitive_issues',[]).
domain_attributes(_,[]).


% Order the users by different domain
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
	and get_task_attribute_value(Domain,'domain')
	and domain_attributes(Domain,Attributes)
thenceforth
	normalized_diversity(DomainInterestUsers,Users,Attributes,@(null),false)
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

% Domain is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(DomainInterestUsers,Users,@(null))
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

% After sorting users by eack diversity dimension aggregate them
whenever
	is_received_event('sortUsersByDiversity',_)
thenceforth
	send_event(_,1,'aggregateUsersByDiversity',json([])).

% If all the dimension are indifferent the match value is 1
whenever
	is_received_event('aggregateUsersByDiversity',_)
	and are_all_dimensions_indifferent()
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(MatchUsers,Users,1.0)
	and initialize_users_to_group_0(GroupsUsers,Users)
	and put_task_state_attribute('matchUsers',MatchUsers)
	and put_task_state_attribute('groupsUsers',GroupsUsers)
	.

:- dynamic are_all_dimensions_indifferent/0, initialize_users_to_group_0/2.

are_all_dimensions_indifferent() :-
	get_task_attribute_value('indifferent','domainInterest'),
	get_task_attribute_value('indifferent','beliefsAndValues'),
	get_task_attribute_value('indifferent','socialCloseness'),
	get_task_attribute_value('anywhere','positionOfAnswerer').

initialize_users_to_group_0([],[]).
initialize_users_to_group_0([json([userId=UserId,group=0,explanationType=group_0])|GroupsUsers],[UserId|Users]) :-
	initialize_users_to_group_0(GroupsUsers,Users).


% Calculate match value for at least one requirement
whenever
	is_received_event('aggregateUsersByDiversity',_)
	and not(are_all_dimensions_indifferent())
thenceforth
	calculate_match_degree_for(MatchUsers,GroupsUsers)
	and put_task_state_attribute('matchUsers',MatchUsers)
	and put_task_state_attribute('groupsUsers',GroupsUsers)
	.

:- dynamic
	calculate_match_degree_for/2.

calculate_match_degree_for(ReverseSortedMatchUsers,GroupsUsers) :-
	get_task_state_attribute(Users,'appUsers'),
	wenet_initialize_user_values(MatchUsers,Users,1.0),
	wenet_sort_user_values_by_value(SortedMatchUsers,MatchUsers),
	reverse(ReverseSortedMatchUsers,SortedMatchUsers),
	initialize_users_to_group_0(GroupsUsers,Users)
	.

% After caluclated the matching go to rank them
whenever
	is_received_event('aggregateUsersByDiversity',_)
thenceforth
	send_event(_,1,'rankMatchUsers',json([])).

% Call the social context builder to rank the user by its match
whenever
	is_received_event('rankMatchUsers',_)
	and get_task_state_attribute(MatchUsers,'matchUsers',[])
thenceforth
	wenet_user_values_to_user_ids(UserIds,MatchUsers)
	and social_ranking(RankedUsers,UserIds)
	and put_task_state_attribute('unaskedUserIds',RankedUsers)
	and put_task_state_attribute('rankedUsers',RankedUsers)
	and send_event(_,1,'askUsersByDiversity',json([])).

% When the task is created filter the posible users to ask about and ask them
whenever
	is_received_event('askUsersByDiversity',_)
	and who_to_ask(Users)
thenceforth
	send_messages(Users,'notifyNewQuestionAndAnswer',json([])).

:- dynamic who_to_ask/1.
who_to_ask(Users) :-
	get_task_state_attribute(Unasked,'unaskedUserIds'),
	get_task_attribute_value(MaxUsers,'maxUsers'),
	( ( append(Users,NewUnasked,Unasked), length(Users,MaxUsers) ) -> true; ( Users = Unasked, NewUnasked = [] ) ),
	!,
	put_task_state_attribute('unaskedUserIds',NewUnasked),
	!,
	retractall(who_to_ask(_)),
	asserta(who_to_ask(Users)).

% Notify user if it can help with a question
whenever
	is_received(_,'notifyNewQuestionAndAnswer',_)
	and get_task_id(TaskId)
	and get_task_goal_name(Question)
	and get_task_requester_id(RequesterId)
	and get_task_attribute_value(Sensitive,'sensitive')
	and get_task_attribute_value(Anonymous,'anonymous')
	and get_task_attribute_value(PositionOfAnswerer,'positionOfAnswerer').
thenceforth
	send_user_message('QuestionToAnswerMessage',json([taskId=TaskId,question=Question,userId=RequesterId,sensitive=Sensitive,anonymous=Anonymous,positionOfAnswerer=PositionOfAnswerer])).

% Provide an answer to a question
whenever
	is_received_do_transaction('answerTransaction',Attributes)
	and not(is_task_closed()) and get_attribute(Answer,answer,Attributes)
	and get_attribute(Anonymous,anonymous,Attributes)
	and get_task_requester_id(RequesterId)
thenceforth
	add_message_transaction()
	and send_message(RequesterId,'notifyAnswerTransaction',json([answer=Answer,anonymous=Anonymous])).

% Notify the questioneer about the answer
whenever
	is_received(SenderId,'notifyAnswerTransaction',Attributes)
	and get_task_attribute_value(ExpirationDate,'expirationDate')
	and is_now_less_than(ExpirationDate)
	and get_task_attribute_value(MaxAnswers,'maxAnswers')
	and get_task_state_attribute(AnswersTransactionIds,'answersTransactionIds',[])
	and length(AnswersTransactionIds,AnswersCount)
	and <(AnswersCount,MaxAnswers)
	and get_attribute(Answer,answer,Attributes)
	and get_attribute(Anonymous,anonymous,Attributes)
	and get_task_goal_name(Question)
	and get_task_id(TaskId)
	and get_transaction_id(TransactionId)
	and get_task_state_attribute(GroupsUsers,'groupsUsers')
thenceforth
	send_user_message('AnsweredQuestionMessage',json([taskId=TaskId,question=Question,transactionId=TransactionId,answer=Answer,userId=SenderId,anonymous=Anonymous]))
	and wenet_add(NewAnswersTransactionIds,TransactionId,AnswersTransactionIds)
	and put_task_state_attribute('answersTransactionIds',NewAnswersTransactionIds)
	and send_event(_,1,'checkMaxAnswers',json([]))
	and explanation(ExplanationTitle,ExplanationText,SenderId,GroupsUsers)
	and send_user_message('TextualMessage',json([title=ExplanationTitle,text=ExplanationText])).

:- dynamic
	explanation/4,
	explanation/5,
	explanation_title/2,
	explanation_text/3.
explanation(ExplanationTitle,ExplanationText,UserId,GroupsUsers) :-
	get_profile_language(Lang),
	explanation(ExplanationTitle,ExplanationText,UserId,GroupsUsers,Lang).
explanation(ExplanationTitle,ExplanationText,UserId,GroupsUsers,Lang) :-
	explanation_title(ExplanationTitle,Lang),
	(
		( wenet_json_element_with(json(Group),GroupsUsers,userId=UserId,json([explanationType=group_0])), member(explanationType=Type,Group))
		-> true
		; Type = group_0
	),
	explanation_text(ExplanationText,Type,Lang).

explanation_title('Why is this user chosen?',_).
explanation_text('Recall that there were no requirements set w.r.t domains, values, social or physical closeness. Nevertheless, we tried to increase the gender diversity of selected users.',group_0,_).
explanation_text('This user fulfils all requirements. While searching for users, we tried to increase the gender diversity of selected users.',group_11,_).
explanation_text('Not enough members fulfil the requirements. To find some answers, we had to choose some that don\'t fulfil any, like this user. While doing so, we also tried to increase the gender diversity of selected users.',group_12,_).
explanation_text('This user fulfils the physical and social closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_a,_).
explanation_text('This user fulfils the social closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_b,_).
explanation_text('This user fulfils the physical closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_c,_).
explanation_text('This user does not fulfil neither the physical and social closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_a,_).
explanation_text('This user does not fulfil neither the social closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_b,_).
explanation_text('This user does not fulfil neither the physical closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_c,_).
explanation_text('This user does not fulfil the physical closeness requirement. To find some answers, we had to relax this requirement. We also tried to increase the gender diversity of selected users.',group_5_a,_).
explanation_text('This user does not fulfil the social closeness requirement. To find some answers, we had to relax this requirement. We also tried to increase the gender diversity of selected users.',group_5_b,_).
explanation_text('This user fulfils the social closeness requirement, but neither the physical closeness requirement nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_a,_).
explanation_text('This user fulfils the physical closeness requirement, but neither the social closeness requirement nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_b,_).

% Nothing to do with this transaction only store it
whenever
	is_received_do_transaction('notAnswerTransaction',_)
	and get_profile_id(Me)
	and not(get_task_requester_id(Me))
	and not(is_task_closed())
thenceforth
	add_message_transaction().

% Nothing to do with this transaction only store it
whenever
	is_received_do_transaction('reportQuestionTransaction',_)
	and get_profile_id(Me)
	and not(get_task_requester_id(Me))
	and not(is_task_closed())
thenceforth
	add_message_transaction().

% Notify to the users about best answer
whenever
	is_received_do_transaction('bestAnswerTransaction',Attributes)
	and get_profile_id(Me)
	and get_task_requester_id(Me)
	and not(is_task_closed())
	and get_attribute(TransactionId,transactionId,Attributes)
	and get_transaction(Transaction,TransactionId)
	and wenet_actioneer_id_of_transaction(ActioneerId,Transaction)
thenceforth
	add_message_transaction()
	and close_task()
	and send_message(ActioneerId,'bestAnswerTransaction',json([transactionId=TransactionId])).

% Notify the user that its answer is picked
whenever
	is_received(_,'bestAnswerTransaction',Attributes)
	and get_attribute(TransactionId,transactionId,Attributes)
	and get_task_goal_name(Question)
	and get_task_id(TaskId)
thenceforth
	send_user_message('AnsweredPickedMessage',json([taskId=TaskId,question=Question,transactionId=TransactionId])).

% Ask more users
whenever
	is_received_do_transaction('moreAnswerTransaction',Attributes)
	and get_profile_id(Me)
	and get_task_requester_id(Me)
	and not(is_task_closed())
	and who_to_ask(Users)
	and get_task_attribute_value(ExpirationDate,'expirationDate')
	and is_now_less_than(ExpirationDate)
	and get_task_state_attribute(AnswersTransactionIds,'answersTransactionIds',[])
	and length(AnswersTransactionIds,AnswersCount)
	and get_task_attribute_value(MaxAnswers,'maxAnswers')
	and <(AnswersCount,MaxAnswers)
	and get_attribute(NewExpirationDate,expirationDate,Attributes)
thenceforth
	add_message_transaction()
	and send_messages(Users,'notifyNewQuestionAndAnswer',json([]))
	and cancel_expiration_event()
	and put_task_attribute('expirationDate',NewExpirationDate)
	and send_expiration_event().

:- dynamic cancel_expiration_event/0.
cancel_expiration_event() :-
	get_task_state_attribute(TimerId,'timerId',''),
	( TimerId = ''
		-> wenet_log_error('No previous event to cancel')
	 	; ( wenet_interaction_protocol_engine_delete_event(TimerId) -> true ; wenet_log_error('Cannot cancel previous event'))
	).

% Nothing to do with this transaction only store it
whenever
	is_received_do_transaction('reportAnswerTransaction',_)
	and get_profile_id(Me)
	and get_task_requester_id(Me)
	and not(is_task_closed())
thenceforth
	add_message_transaction().

% Send expiration message if received max answers
whenever
	is_received(_,'checkMaxAnswers',_)
	and get_task_state_attribute(AnswersTransactionIds,'answersTransactionIds',[])
	and length(AnswersTransactionIds,AnswersCount)
	and get_task_attribute_value(MaxAnswers,'maxAnswers')
	and =<(MaxAnswers,AnswersCount)
thenceforth
	send_event(_,1,'notifyQuestionExpirationMessage',json([])).

% Notify user of the expiration message
whenever
	is_received_event('notifyQuestionExpirationMessage',_)
	and get_task_state_attribute(AnswersTransactionIds,'answersTransactionIds',[])
	and get_task_id(TaskId)
	and get_task_goal_name(Question)
thenceforth
	send_user_message('QuestionExpirationMessage',json([taskId=TaskId,question=Question,listOfTransactionIds=AnswersTransactionIds]))
	and cancel_expiration_event().

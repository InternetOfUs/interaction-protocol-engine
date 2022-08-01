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


% Order the users by similar social closeness
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('similar','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	normalized_social_closeness(SocialClosenessUsers,Users,@(null))
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Order the users by different social closeness
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
    normalized_social_closeness(Socialness,Users,@(null))
	and wenet_negate_user_value(SocialClosenessUsers,Socialness)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Users social closeness is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(SocialClosenessUsers,Users,@(null))
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
get_profile_attribues_by_beliefs_and_values(['meanings.excitement','meanings.promotion','meanings.existence','meanings.suprapersonal','meanings.interactive','meanings.normative','meanings.extraversion','meanings.agreeableness','meanings.consientiousness','meanings.neuroticism','meanings.openness']).

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
	domain_attributes/2,
	attributes_similars_to_question/1.

domain_attributes('studying_career',['competences.u_active','competences.u_read','competences.u_essay','competences.u_org','competences.u_balance','competences.u_assess','competences.u_theory','competences.u_pract']).
domain_attributes('local_university',['competences.c_locfac','competences.c_accom']).
domain_attributes('local_things',['competences.c_accom']).
domain_attributes('physical_activity',['competences.c_team_sp','competences.c_ind_sp','competences.c_watch_sp']).
domain_attributes('cultural_interests',['competences.c_lit','competences.c_creatlit','competences.c_perf_art','competences.c_musgall']).
domain_attributes('food_and_cooking',['competences.c_food','competences.c_eating']).
domain_attributes('cinema_theatre',['competences.c_plays','competences.c_perf_plays']).
domain_attributes('music',['competences.c_app_mus','competences.c_perf_mus']).
domain_attributes('arts_and_crafts',['competences.c_perf_art','competences.c_musgall']).
domain_attributes('life_ponders',Attributes) :-
	attributes_similars_to_question(Attributes).
domain_attributes('varia_misc',Attributes) :-
	attributes_similars_to_question(Attributes).
attributes_similars_to_question(Attributes) :-
	(  ( get_task_goal_name(Question),  my_profile_attributes_similars_to(SimAttributes,Question,0.4) ) -> true  ; SimAttributes = [] ),
	!,
	(  length(SimAttributes,0)
		-> Attributes = ['competences.u_active','competences.u_read','competences.u_essay','competences.u_org','competences.u_balance','competences.u_assess','competences.u_theory','competences.u_pract','competences.c_locfac','competences.c_accom','competences.c_team_sp','competences.c_ind_sp','competences.c_watch_sp','competences.c_lit','competences.c_creatlit','competences.c_perf_art','competences.c_musgall','competences.c_food','competences.c_eating','competences.c_plays','competences.c_perf_plays','competences.c_app_mus','competences.c_perf_mus']
		; Attributes = SimAttributes
	),
    retractall(attributes_similars_to_question(_)),
    asserta(attributes_similars_to_question(Attributes)).


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
	and put_task_state_attribute('matchUsers',MatchUsers).

:- dynamic are_all_dimensions_indifferent/0.
are_all_dimensions_indifferent() :-
	get_task_attribute_value('indifferent','domainInterest'),
	get_task_attribute_value('indifferent','beliefsAndValues'),
	get_task_attribute_value('indifferent','socialCloseness').

% Calculate match value for ‘basic needs’, ‘appreciating culture’, ‘performing/producing culture’, ‘physical activities/sports’, or ‘things to do about town’ domains
whenever
	is_received_event('aggregateUsersByDiversity',_)
	and not(are_all_dimensions_indifferent())
	and get_task_attribute_value(Domain,'domain')
	and member(Domain,['food_and_cooking','cultural_interests','arts_and_crafts','physical_activity','local_things'])
thenceforth
	calculate_match_degree_for_basic_needs_and_other_domains(MatchUsers)
	and put_task_state_attribute('matchUsers',MatchUsers).

:- dynamic
	calculate_match_degree_for_basic_needs_and_other_domains/1,
	calculate_match_degree_for_basic_needs_and_other_domains_/5,
	calculate_user_match_degree_for_basic_needs_and_other_domains/5.

calculate_match_degree_for_basic_needs_and_other_domains(ReverseSortedMatchUsers) :-
	get_task_state_attribute(Users,'appUsers'),
	get_task_state_attribute(SocialClosenessUsers,'socialClosenessUsers'),
	get_task_state_attribute(BeliefsAndValuesUsers,'beliefsAndValuesUsers'),
	get_task_state_attribute(DomainInterestUsers,'domainInterestUsers'),
	calculate_match_degree_for_basic_needs_and_other_domains_(MatchUsers,Users,SocialClosenessUsers,DomainInterestUsers,BeliefsAndValuesUsers),
	wenet_sort_user_values_by_value(SortedMatchUsers,MatchUsers),
	reverse(ReverseSortedMatchUsers,SortedMatchUsers)
	.

calculate_match_degree_for_basic_needs_and_other_domains_([],[],_,_,_).
calculate_match_degree_for_basic_needs_and_other_domains_([MatchUser|MatchUsers],[UserId|UserIds],SocialClosenessUsers,DomainInterestUsers,BeliefsAndValuesUsers) :-
	calculate_user_match_degree_for_basic_needs_and_other_domains(Value,UserId,SocialClosenessUsers,DomainInterestUsers,BeliefsAndValuesUsers),
	wenet_new_user_value(MatchUser,UserId,Value),
	calculate_match_degree_for_basic_needs_and_other_domains_(MatchUsers,UserIds,SocialClosenessUsers,DomainInterestUsers,BeliefsAndValuesUsers)
	.

calculate_user_match_degree_for_basic_needs_and_other_domains(Value,UserId,SocialClosenessUsers,DomainInterestUsers,BeliefsAndValuesUsers) :-
	wenet_value_of_user_id_from_user_values(DomainInterest,UserId,DomainInterestUsers,@(null)),
	wenet_value_of_user_id_from_user_values(BeliefsAndValues,UserId,BeliefsAndValuesUsers,@(null)),
	wenet_value_of_user_id_from_user_values(SocialCloseness,UserId,SocialClosenessUsers,@(null)),
	( number(DomainInterest) -> X = 1; X = 0 ),
	( number(BeliefsAndValues) -> Y = 1; Y = 0 ),
	( number(SocialCloseness) -> Z = 1; Z = 0 ),
	( number(DomainInterest) -> MdX = DomainInterest; MdX = 0 ),
	( number(BeliefsAndValues) -> MdV = BeliefsAndValues; MdV = 0 ),
	( number(SocialCloseness) -> MdSC = SocialCloseness; MdSC = 0 ),
	( (X = 0 , Y = 0, Z = 0) -> Value = 0 ; Value is (3*X*MdX + Y*MdV + Z*MdSC)/(3*X + Y + Z) )
	.


% Calculate match value for ‘campus life’ domain
whenever
	is_received_event('aggregateUsersByDiversity',_)
	and not(are_all_dimensions_indifferent())
	and get_task_attribute_value('local_university','domain')
thenceforth
	calculate_match_degree_for_campus_life_domain(MatchUsers)
	and put_task_state_attribute('matchUsers',MatchUsers).

:- dynamic calculate_match_degree_for_campus_life_domain/1.
calculate_match_degree_for_campus_life_domain(MatchUsers) :-
	MatchUsers = [].


% Calculate match value for ‘academic skills’ domain
whenever
	is_received_event('aggregateUsersByDiversity',_)
	and not(are_all_dimensions_indifferent())
	and get_task_attribute_value('studying_career','domain')
thenceforth
	calculate_match_degree_for_academic_skills_domain(MatchUsers)
	and put_task_state_attribute('matchUsers',MatchUsers).

:- dynamic calculate_match_degree_for_academic_skills_domain/1.
calculate_match_degree_for_academic_skills_domain(MatchUsers) :-
	MatchUsers = [].


% Calculate match value for ‘random thoughts’ or ‘sensitive issues’ domains
whenever
	is_received_event('aggregateUsersByDiversity',_)
	and not(are_all_dimensions_indifferent())
	and get_task_attribute_value(Domain,'domain')
	and member(Domain,['varia_misc','life_ponders'])
thenceforth
	calculate_match_degree_for_random_thougs_and_other_domains(MatchUsers)
	and put_task_state_attribute('matchUsers',MatchUsers).

:- dynamic calculate_match_degree_for_random_thougs_and_other_domains/1.
calculate_match_degree_for_random_thougs_and_other_domains(MatchUsers) :-
	MatchUsers = [].


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
	and volunteers_ranking(RankedUsers,UserIds)
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
	and get_transaction_id(TransactionId)
	and get_task_state_attribute(SocialClosenessUsers,'socialClosenessUsers')
	and get_task_state_attribute(BeliefsAndValuesUsers,'beliefsAndValuesUsers')
	and get_task_state_attribute(DomainInterestUsers,'domainInterestUsers')
thenceforth
	send_user_message('AnsweredQuestionMessage',json([taskId=TaskId,question=Question,transactionId=TransactionId,answer=Answer,userId=SenderId,anonymous=Anonymous]))
	and wenet_add(NewAnswersTransactionIds,TransactionId,AnswersTransactionIds)
	and put_task_state_attribute('answersTransactionIds',NewAnswersTransactionIds)
	and send_event(_,1,'checkMaxAnswers',json([]))
	and explanation(ExplanationTitle,ExplanationText,SenderId,SocialClosenessUsers,BeliefsAndValuesUsers,DomainInterestUsers)
	and send_user_message('TextualMessage',json([title=ExplanationTitle,text=ExplanationText])).

:- dynamic
	explanation/6,
	explanation/7,
	explanation_title/2,
	explanation_text/3.
explanation(ExplanationTitle,ExplanationText,UserId,SocialClosenessUsers,BeliefsAndValuesUsers,DomainInterestUsers) :-
	get_profile_language(Lang),
	explanation(ExplanationTitle,ExplanationText,UserId,SocialClosenessUsers,BeliefsAndValuesUsers,DomainInterestUsers,Lang).
explanation(ExplanationTitle,ExplanationText,UserId,SocialClosenessUsers,BeliefsAndValuesUsers,DomainInterestUsers,Lang) :-
	explanation_title(ExplanationTitle,Lang),
	( are_all_dimensions_indifferent()
		-> Type = type1 ;
		(
			wenet_value_of_user_id_from_user_values(SocialCloseness,UserId,SocialClosenessUsers,@(null)),
		 	wenet_value_of_user_id_from_user_values(BeliefsAndValue,UserId,BeliefsAndValuesUsers,@(null)),
		 	wenet_value_of_user_id_from_user_values(DomainInterest,UserId,DomainInterestUsers,@(null)),
		 	( (number(SocialCloseness);number(BeliefsAndValue);number(DomainInterest)) -> Type = type2 ; Type = type3)
		 )
	),
	explanation_text(ExplanationText,Type,Lang).

explanation_title('Why is this user chosen?',_).
explanation_text('Recall that no requirements were set w.r.t domains, values and social closeness. Nevertheless, we tried to increase the gender diversity of selected users.',type1,_).
explanation_text('This user fits the requirements to a certain extent. While choosing whom to ask, we also tried to increase the gender diversity of selected users.',type2,_).
explanation_text('Not enough members in the community fit the requirements. We had to relax the requirements in order to find some answers, which is how this user was chosen. While choosing whom to ask, we also tried to increase the gender diversity of selected users.',type3,_).


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


[{"userId":"62e40ba71f209b3c24ca6d7d","value":0.9099999999999999},{"userId":"62e40ba71f209b3c24ca6d78","value":0.818},{"userId":"62e40ba81f209b3c24ca6d7f","value":0.808},{"userId":"62e40ba71f209b3c24ca6d7a","value":0.716},{"userId":"62e40ba71f209b3c24ca6d7e","value":0.7060000000000001},{"userId":"62e40ba71f209b3c24ca6d7b","value":0.614},{"userId":"62e40ba71f209b3c24ca6d7c","value":0.512},{"userId":"62e40ba71f209b3c24ca6d6f","value":0.0},{"userId":"62e40ba61f209b3c24ca6d6d","value":0.0},{"userId":"62e40ba61f209b3c24ca6d6e","value":0.0}]
[{"userId":"62e40ba71f209b3c24ca6d7d","value":0.9099999999999999},{"userId":"62e40ba71f209b3c24ca6d78","value":0.818},{"userId":"62e40ba81f209b3c24ca6d7f","value":0.808},{"userId":"62e40ba71f209b3c24ca6d7a","value":0.716},{"userId":"62e40ba71f209b3c24ca6d7e","value":0.7060000000000001},{"userId":"62e40ba71f209b3c24ca6d7b","value":0.614},{"userId":"62e40ba61f209b3c24ca6d6d","value":0.52},{"userId":"62e40ba71f209b3c24ca6d7c","value":0.512},{"userId":"62e40ba61f209b3c24ca6d6e","value":0.51},{"userId":"62e40ba71f209b3c24ca6d6f","value":0.5}]
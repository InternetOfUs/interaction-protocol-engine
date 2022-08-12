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


% Calculate domain dimension if it is similar
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

domain_attributes('exact_sciences',['competences.c_logic','competences.c_math','competences.c_fisics','competences.c_probability','competences.c_trigonometry','competences.c_arithmetic','competences.c_algebra','competences.c_analytical_geometry','competences.c_calculus']).
domain_attributes('computer_s_science',['competences.c_informatics','competences.c_soft_engineering','competences.c_programing','competences.c_networks','competences.c_operating_systems','competences.c_databases','competences.c_automata_theory','competences.c_data_science','competences.c_compilers']).
domain_attributes('health',['competences.c_chemistry','competences.c_biology','competences.c_psicology','competences.c_kinesiology','competences.c_physiotherapy','competences.c_biochemistry','competences.c_odontology','competences.c_nutrition','competences.c_pharmacology','competences.c_anatomy','competences.c_phonoaudiology','competences.c_histology ','competences.c_gynecology','competences.c_oncology','competences.c_psychiatry','competences.c_pediatrics','competences.c_otorhin','competences.c_traumatology','competences.c_dermatology']).
domain_attributes('administrative_and_accounting',['competences.c_administration','competences.c_accounting','competences.c_economy','competences.c_marketing','competences.c_int_commerce']).
domain_attributes('social_sciences',['competences.c_filosofy','competences.c_history','competences.c_sociology']).
domain_attributes('legal',['competences.c_diplomacy','competences.c_politics','competences.c_law','competences.c_notary']).
domain_attributes('environmental',['competences.c_environmental_sciences']).
domain_attributes('design_and_construction',['competences.c_visual_design','competences.c_digital_design','competences.c_sanitary_installations','competences.c_electrical_installations','competences.c_contr_management','competences.c_structure_design']).
domain_attributes('electronic_sciences',['competences.c_anan_electronics','competences.c_digit_electronics','competences.c_telecommunications','competences.c_automation','competences.c_electronics']).
domain_attributes('academic_life',['competences.u_active','competences.u_read','competences.u_essay','competences.u_org','competences.u_balance','competences.u_assess','competences.u_theory','competences.u_pract']).
domain_attributes(_,[]).


% Calculate domain dimension if it is different
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
	and get_task_attribute_value(Domain,'domain')
	and domain_attributes(Domain,Attributes)
thenceforth
	normalized_diversity(DomainInterestUsers,Users,Attributes,@(null),false)
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

% Calculate domain dimension if it is indiferrent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(DomainInterestUsers,Users,@(null))
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).


% Calculate believe and values dimension if it is similar
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
get_profile_attribues_by_beliefs_and_values(['meanings.neuroticism','meanings.extraversion','meanings.openness','meanings.agreeableness','meanings.conscientiousness']).

% Calculate believe and values dimension if it is different
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_beliefs_and_values(Attributes)
thenceforth
	normalized_diversity(BeliefsAndValuesUsers,Users,Attributes,@(null),false)
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

% Calculate believe and values dimension if it is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(BeliefsAndValuesUsers,Users,@(null))
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

% Calculate competences dimenstion if it is relevant
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('relevant','competences')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	calculate_competences_users(CompetencesUsers,Users)
	and put_task_state_attribute('competencesUsers',CompetencesUsers).

:- dynamic calculate_competences_users/2, calculate_competences_value/2, calculate_competences_value_/4, uc_competence_names/1.


calculate_competences_users([],[]).
calculate_competences_users([CompetencesUser|CompetencesUsers],[User|Users]) :-
	calculate_competences_value(Value,User),
	wenet_new_user_value(CompetencesUser,User,Value),
	calculate_competences_users(CompetencesUsers,Users)
	.
calculate_competences_value(Value,User) :-
	uc_competence_names(Competences),
	( wenet_profile_manager_get_profile(Profile,User)
		->
		(
			calculate_competences_value_(Sum,Total,Profile,Competences),
			( Total =:= 0.0 -> Value = 0.0 ; Value is Sum/Total)
		)
		;
		Value = 0.0
	)
	.
uc_competence_names(['c_logic'
,'c_math'
,'c_informatics'
,'c_fisics'
,'c_probability'
,'c_trigonometry'
,'c_arithmetic'
,'c_algebra'
,'c_analytical_geometry'
,'c_chemistry'
,'c_calculus'
,'c_biology'
,'c_administration'
,'c_accounting'
,'c_economy'
,'c_marketing'
,'c_int_commerce'
,'c_filosofy'
,'c_history'
,'c_sociology'
,'c_psicology'
,'c_soft_engineering'
,'c_programing'
,'c_networks'
,'c_operating_systems'
,'c_databases'
,'c_automata_theory'
,'c_data_science'
,'c_compilers'
,'c_diplomacy'
,'c_politics'
,'c_law'
,'c_notary'
,'c_kinesiology'
,'c_physiotherapy'
,'c_biochemistry'
,'c_odontology'
,'c_nutrition'
,'c_pharmacology'
,'c_anatomy'
,'c_phonoaudiology'
,'c_histology'
,'c_gynecology'
,'c_oncology'
,'c_psychiatry'
,'c_pediatrics'
,'c_otorhin'
,'c_traumatology'
,'c_dermatology'
,'c_environmental_sciences'
,'c_visual_design'
,'c_digital_design'
,'c_anan_electronics'
,'c_digit_electronics'
,'c_telecommunications'
,'c_automation'
,'c_electronics'
,'c_sanitary_installations'
,'c_electrical_installations'
,'c_contr_management'
,'c_structure_design']).

calculate_competences_value_(0.0,0.0,_,[]).
calculate_competences_value_(Sum,Total,Profile,[Name|Competences]):-
	calculate_competences_value_(PrevSum,PrevTotal,Profile,Competences),
	get_profile_competence(Value,Profile,Name,@(null)),
	(number(Value)->(Sum is PrevSum + Value,Total is PrevTotal + 1.0);(Sum=PrevSum,Total=PrevTotal))
	.

% Calculate competences dimenstion if it is irrelevant
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('irrelevant','competences')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(CompetencesUsers,Users,@(null))
	and put_task_state_attribute('competencesUsers',CompetencesUsers).



% Calculate physical closeness dimension if it is nearby
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('nearby','positionOfAnswerer')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	normalized_closeness(PhysicalClosenessUsers,Users,500)
	and put_task_state_attribute('physicalClosenessUsers',PhysicalClosenessUsers).

% Calculate physical closeness if it is anywhere
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('anywhere','positionOfAnswerer')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(PhysicalClosenessUsers,Users,@(null))
	and put_task_state_attribute('physicalClosenessUsers',PhysicalClosenessUsers).


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
	and put_task_state_attribute('groupsUsers',GroupsUsers).

:- dynamic are_all_dimensions_indifferent/0, initialize_users_to_group_0/2.
are_all_dimensions_indifferent() :-
	get_task_attribute_value('indifferent','domainInterest'),
	get_task_attribute_value('indifferent','beliefsAndValues'),
	get_task_attribute_value('irrelevant','competences'),
	get_task_attribute_value('anywhere','positionOfAnswerer')
	.

initialize_users_to_group_0([],[]).
initialize_users_to_group_0([json([userId=UserId,group=0,explanationType=group_0])|GroupsUsers],[UserId|Users]) :-
	initialize_users_to_group_0(GroupsUsers,Users).

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
explanation_text('Recall that there were no requirements set w.r.t academic life domain, values, competences or physical closeness. Nevertheless, we tried to increase the gender diversity of selected users.',group_0,_).
explanation_text('This user fulfils all requirements. While searching for users, we tried to increase the gender diversity of selected users.',group_1,_).
explanation_text('Not enough members fulfil the requirements. To find some answers, we had to choose some that do not fulfil any, like this user. While doing so, we also tried to increase the gender diversity of selected users.',group_9,_).
explanation_text('This user fulfils the physical closeness and competence requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_a,_).
explanation_text('This user fulfils the competence requirement, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_b,_).
explanation_text('This user fulfils the physical closeness requirement, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_c,_).
explanation_text('This user does not fulfil neither the physical closeness and competence requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_7_8_a,_).
explanation_text('This user does not fulfil neither the competence requirement, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_7_8_b,_).
explanation_text('This user does not fulfil neither the physical closeness requirement, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_7_8_c,_).
explanation_text('This user does not fulfil the physical closeness requirement. To find some answers, we had to relax this requirement. We also tried to increase the gender diversity of selected users.',group_4_a,_).
explanation_text('This user does not fulfil the competence requirement. To find some answers, we had to relax this requirement. We also tried to increase the gender diversity of selected users.',group_4_b,_).
explanation_text('This user fulfils the competence requirement, but neither the physical closeness requirement nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_6_a,_).
explanation_text('This user fulfils the physical closeness requirement, but neither the competence requirement nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_6_b,_).

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

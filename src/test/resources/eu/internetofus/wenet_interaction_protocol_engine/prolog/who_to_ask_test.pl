:- use_module(library(random)).

:- dynamic who_to_ask/1,
	users_by_closeness/2,
	users_by_social_closeness/2,
	users_by_beliefs_and_values/2,
	users_by_domain_interest/2,
	get_profile_attribues_by_beliefs_and_values/1,
	get_profile_attribues_by_domain_interest/1,
	domain_attributes/2,
	attributes_similars_to_question/1
	.
	
who_to_ask(Users) :-
	(
		get_task_state_attribute(Unasked,'unaskedUserIds')
		-> true
		; (
			get_app_users_except_me(UsersExceptMe),
			random_permutation(AppUsers,UsersExceptMe),
			put_task_state_attribute('appUsers',AppUsers),
			users_by_closeness(ClosenessUsers,AppUsers),
			users_by_social_closeness(SocialClosenessUsers,AppUsers),
			wenet_product_user_values(Tmp1,ClosenessUsers,SocialClosenessUsers),
			users_by_beliefs_and_values(BeliefsAndValuesUsers,AppUsers),
			wenet_product_user_values(Tmp2,Tmp1,BeliefsAndValuesUsers),
			users_by_domain_interest(DomainInterestUsers,AppUsers),
			wenet_product_user_values(Tmp3,Tmp2,DomainInterestUsers),
			wenet_sort_user_values_by_value(Sorted,Tmp3),
			reverse(Sorted,WhoToAsk),
			put_task_state_attribute('whoToAskUsers',WhoToAsk),
			wenet_user_values_to_user_ids(Unasked,WhoToAsk)
		)
	),
	!,
	get_task_attribute_value(MaxUsers,'maxUsers'),
	(
		( append(Users,NewUnasked,Unasked), length(Users,MaxUsers) )
		-> true
		; ( Users = Unasked, NewUnasked = [] ) 
	),
	!,
	put_task_state_attribute('unaskedUserIds',NewUnasked),
	!,
	retractall(who_to_ask(_)),
	asserta(who_to_ask(Users)).

users_by_closeness(ClosenessUsers,Users) :-
	(
		( get_task_attribute_value(PositionOfAnswerer,'positionOfAnswerer'), =(PositionOfAnswerer,'nearby') )
		-> normalized_closeness(ClosenessUsers,Users,1000000) 
		; wenet_initialize_user_values(ClosenessUsers,Users,1.0)
	),
	put_task_state_attribute('closenessUsers',ClosenessUsers).

users_by_social_closeness(SocialClosenessUsers,Users) :-
	(
		( get_task_attribute_value(SocialClosenessAttr,'socialCloseness'), not(=(SocialClosenessAttr,'indifferent')) )
		-> (
			normalized_social_closeness(Socialness,Users),
			(
				=(SocialClosenessAttr,'similar')
				-> SocialClosenessUsers = Socialness
				; wenet_negate_user_value(SocialClosenessUsers,Socialness)
			)
		) 
		; wenet_initialize_user_values(SocialClosenessUsers,Users,1.0)
	),
	put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

users_by_beliefs_and_values(BeliefsAndValuesUsers,Users) :-
	(
		( get_task_attribute_value(BeliefsAndValuesAttr,'beliefsAndValues'), not(=(BeliefsAndValuesAttr,'indifferent')) )
		-> (
			get_profile_attribues_by_beliefs_and_values(Attributes),
			normalized_diversity(Diversity,Users,Attributes),
			(
				=(BeliefsAndValuesAttr,'similar')
				-> wenet_negate_user_value(BeliefsAndValuesUsers,Diversity)
				; BeliefsAndValuesUsers = Diversity
			)
		) 
		; wenet_initialize_user_values(BeliefsAndValuesUsers,Users,1.0)
	),
	put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).
	
get_profile_attribues_by_beliefs_and_values(['meanings.excitement','meanings.promotion','meanings.existence','meanings.suprapersonal','meanings.interactive','meanings.normative','meanings.extraversion','meanings.agreeableness','meanings.consientiousness','meanings.neuroticism','meanings.openness']).
	
users_by_domain_interest(DomainInterestUsers,Users) :-
	(
		( get_task_attribute_value(DomainInterestAttr,'domainInterest'), not(=(DomainInterestAttr,'indifferent')) )
		-> (
			get_profile_attribues_by_domain_interest(Attributes),
			normalized_diversity(Diversity,Users,Attributes),
			(
				=(DomainInterestAttr,'similar')
				-> wenet_negate_user_value(DomainInterestUsers,Diversity)
				; DomainInterestUsers = Diversity
			)
		) 
		; wenet_initialize_user_values(DomainInterestUsers,Users,1.0)
	),
	put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

	
get_profile_attribues_by_domain_interest(Attributes) :-
	get_task_attribute_value(Domain,'domain'),
	domain_attributes(Domain,Attributes),
	!,
	retractall(get_profile_attribues_by_domain_interest(_)),
	asserta(get_profile_attribues_by_domain_interest(Attributes))
	.
	
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
	attributes_similars_to_question(Attributes)
	.
domain_attributes('varia_misc',Attributes) :-
	attributes_similars_to_question(Attributes)
	.

attributes_similars_to_question(Attributes) :-
	(
		get_task_goal_name(Question),
		my_profile_attributes_similars_to(Attributes,Question,0.4)
		-> true
		; Attributes = []
	),
	!,
	retractall(attributes_similars_to_question(_)),
	asserta(attributes_similars_to_question(Attributes))
	.
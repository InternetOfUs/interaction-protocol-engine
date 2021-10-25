:- use_module(library(random)).

:- dynamic who_to_ask/1,
	users_by_closeness/2,
	users_by_social_closeness/2,
	users_by_beliefs_and_values/2,
	users_by_domain_interest/2,
	get_profile_attribues_by_beliefs_and_values/1,
	get_profile_attribues_by_domain_interest/1,
	attributes_by_domain/2
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
	
get_profile_attribues_by_beliefs_and_values(['gender','nationality','occupation']).
	
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
	attributes_by_domain(Attributes,Domain).
	
attributes_by_domain('studying_career',['gender','nationality','occupation']).
attributes_by_domain('local_university',['gender','nationality','occupation']).
attributes_by_domain('local_things',['gender','nationality','occupation']).
attributes_by_domain('physical_activity',['gender','nationality','occupation']).
attributes_by_domain('cultural_interests',['gender','nationality','occupation']).
attributes_by_domain('food_and_cooking',['gender','nationality','occupation']).
attributes_by_domain('cinema_theatre',['gender','nationality','occupation']).
attributes_by_domain('music',['gender','nationality','occupation']).
attributes_by_domain('arts_and_crafts',['gender','nationality','occupation']).
attributes_by_domain('life_ponders',['gender','nationality','occupation']).
attributes_by_domain('varia_misc',Attributes) :-
	get_task_goal_name(Question),
	my_profile_attributes_similars_to(Attributes,Question,0.4)
	.

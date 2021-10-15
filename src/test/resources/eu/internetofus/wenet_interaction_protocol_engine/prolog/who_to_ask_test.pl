:- use_module(library(random)).

:- dynamic who_to_ask/1,
	users_by_closeness/2,
	users_by_social_closeness/2,
	users_by_belief_and_values/2,
	users_by_domain_interest/2.
	
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
			users_by_belief_and_values(BeliefsAndValuesUsers,AppUsers),
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

users_by_belief_and_values(BeliefsAndValuesUsers,Users) :-
	wenet_initialize_user_values(BeliefsAndValuesUsers,Users,1.0),
	put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).
	
users_by_domain_interest(DomainInterestUsers,Users) :-
	wenet_initialize_user_values(DomainInterestUsers,Users,1.0),
	put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

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


% Calculate domain dimension if it is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(DomainInterestUsers,Users,@(null))
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

% Calculate domain dimension if it is similar and the domain is 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('similar','domainInterest')
	and get_task_attribute_value('academic_skills','domain')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(DomainInterestUsers,Users,@(null))
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

% Calculate domain dimension if it is similar for the rest of domains
whenever
	is_received_event('sortUsersByDiversity',_)
	and not(get_task_attribute_value('academic_skills','domain'))
	and get_task_attribute_value('similar','domainInterest')
	and get_task_state_attribute(Users,'appUsers')
	and get_task_attribute_value(Domain,'domain')
	and domain_attributes(Domain,Attributes)
thenceforth
	normalized_diversity(Diversity,Users,Attributes,1.0,false)
	and wenet_negate_user_value(DomainInterestUsers,Diversity)
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

:- dynamic
	domain_attributes/2.

domain_attributes('basic_needs',['competences.c_food','competences.c_accom']).
domain_attributes('campus_life',['materials.study_year']).
domain_attributes('academic_skills',['competences.c_u_active','competences.c_read','competences.c_essay','competences.c_org','competences.c_balance','competences.c_assess','competences.c_theory','competences.c_pract','competences.course_fa','competences.course_plc','competences.course_oop','materials.program_study']).
domain_attributes('appreciating_culture',['competences.c_lit','competences.c_app_mus','competences.c_plays','competences.c_musgall']).
domain_attributes('producing_culture',['competences.c_creatlit','competences.c_perf_mus','competences.c_perf_plays','competences.c_perf_art']).
domain_attributes('physical_activity',['competences.c_wath_sp','competences.c_ind_sp','competences.c_team_sp']).
domain_attributes('leisure_activities',['competences.c_eating','competences.c_locfac']).
domain_attributes('random_thoughts',[]).
domain_attributes('sensitive',[]).
domain_attributes(_,[]).


% Calculate domain dimension if it is different and the domain is 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','domainInterest')
	and get_task_attribute_value('academic_skills','domain')
	and get_task_state_attribute(Users,'appUsers')
	and domain_attributes('academic_skills',Attributes)
thenceforth
	calculate_domain_by_attribute(DomainInterestUsers,Users,Attributes)
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

:- dynamic calculate_domain_by_attribute/3,
	get_domain_attribute_values_for/3,
	calculate_domain_by_attribute_/4,
	get_domain_competence_value/3,
	get_domain_material_value/3,
	calculate_domain_by_attribute_values/3,
	are_domain_attribute_values_greater/2,
	are_domain_attribute_values_le/2.

calculate_domain_by_attribute(DomainInterestUsers,Users,Attributes) :-
	get_profile(Profile),
	get_domain_attribute_values_for(Values,Profile,Attributes),
	calculate_domain_by_attribute_(DomainInterestUsers,Users,Values,Attributes)
	.

calculate_domain_by_attribute_([],[],_,_).
calculate_domain_by_attribute_([DomainInterestUser|DomainInterestUsers],[User|Users],Values,Attributes) :-
	(wenet_profile_manager_get_profile(Profile,User)->true;Profile = json([competences=[],materials=[]])),
	get_domain_attribute_values_for(UserValues,Profile,Attributes),
	calculate_domain_by_attribute_values(Value,Values,UserValues),
	wenet_new_user_value(DomainInterestUser,User,Value),
	calculate_domain_by_attribute_(DomainInterestUsers,Users,Values,Attributes)
	.

get_domain_attribute_values_for([],_,[]).
get_domain_attribute_values_for([Value|Values],Profile,[Attribute|Attributes]) :-
	(get_domain_competence_value(Value,Profile,Attribute);get_domain_material_value(Value,Profile,Attribute);Value = 0.0),
	get_domain_attribute_values_for(Values,Profile,Attributes)
	.

get_domain_competence_value(Value,Profile,Attribute) :-
	string_concat('competences.',CompetenceStr,Attribute),
	atom_string(CompetenceName, CompetenceStr),
	get_profile_competence(Competence,Profile,CompetenceName,json([level=0.0])),
	get_attribute(Value,level,0.0,Competence)
	.

get_domain_material_value(Value,Profile,Attribute) :-
	string_concat('materials.',MaterialStr,Attribute),
	atom_string(MaterialName, MaterialStr),
	get_profile_material(Material,Profile,MaterialName,json([description=''])),
	get_attribute(Value,description,'',Material)
	.

calculate_domain_by_attribute_values(Value,[Requester|RequesterValues],[User|UserValues]):-
	( Requester @> User ->
		( are_domain_attribute_values_greater(RequesterValues,UserValues) -> Value = 0.0;Value = 0.5)
		; ( are_domain_attribute_values_le(RequesterValues,UserValues) -> Value = 1.0;Value = 0.5)
	).
are_domain_attribute_values_greater([],[]).
are_domain_attribute_values_greater([Requester|RequesterValues],[User|UserValues]) :-
	Requester @> User,
	are_domain_attribute_values_greater(RequesterValues,UserValues).
are_domain_attribute_values_le([],[]).
are_domain_attribute_values_le([Requester|RequesterValues],[User|UserValues]) :-
	Requester @=< User,
	are_domain_attribute_values_le(RequesterValues,UserValues).


% Calculate domain dimension if it is different for the rest of domains
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','domainInterest')
	and not(get_task_attribute_value('academic_skills','domain'))
	and get_task_state_attribute(Users,'appUsers')
	and get_task_attribute_value(Domain,'domain')
	and domain_attributes(Domain,Attributes)
thenceforth
	normalized_diversity(DomainInterestUsers,Users,Attributes,0.0,false)
	and put_task_state_attribute('domainInterestUsers',DomainInterestUsers).

% Calculate believe and values if it is similar
and values
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('similar','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_beliefs_and_values(Attributes)
thenceforth
	normalized_diversity(Diversity,Users,Attributes,1.0,false)
	and wenet_negate_user_value(BeliefsAndValuesUsers,Diversity)
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

:- dynamic get_profile_attribues_by_beliefs_and_values/1.
get_profile_attribues_by_beliefs_and_values(['meanings.excitement', 'meanings.promotion', 'meanings.existence', 'meanings.suprapersonal', 'meanings.interactive', 'meanings.normative', 'meanings.neuroticism', 'meanings.extraversion', 'meanings.openness', 'meanings.agreeableness', 'meanings.conscientiousness']).

% Calculate believe and values if it is different
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('different','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_beliefs_and_values(Attributes)
thenceforth
	normalized_diversity(BeliefsAndValuesUsers,Users,Attributes,0.0,false)
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

% Calculate believe and values if it is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','beliefsAndValues')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(BeliefsAndValuesUsers,Users,@(null))
	and put_task_state_attribute('beliefsAndValuesUsers',BeliefsAndValuesUsers).

% Calculate social closeness if it is indifferent
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('indifferent','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	wenet_initialize_user_values(SocialClosenessUsers,Users,@(null))
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Calculate social closeness if it is similar and domain is 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('academic_skills','domain')
	and get_task_attribute_value('similar','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_social_closeness(Attributes)
thenceforth
	normalized_diversity(Diversity,Users,Attributes,1.0,false)
	and wenet_negate_user_value(SocialClosenessUsers,Diversity)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

:- dynamic get_profile_attribues_by_social_closeness/1.
get_profile_attribues_by_social_closeness(['materials.department','materials.study_year','materials.program_study']).


% Calculate social closeness if it is different and domain is 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('academic_skills','domain')
	and get_task_attribute_value('different','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
	and get_profile_attribues_by_social_closeness(Attributes)
thenceforth
	normalized_diversity(SocialClosenessUsers,Users,Attributes,0.0,false)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Calculate social closeness if it is similar and domain is not 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and not(get_task_attribute_value('academic_skills','domain'))
	and get_task_attribute_value('similar','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
	normalized_social_closeness(SocialClosenessUsers,Users,0.0)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Calculate social closeness if it is different and domain is not 'academic skills'
whenever
	is_received_event('sortUsersByDiversity',_)
	and not(get_task_attribute_value('academic_skills','domain'))
	and get_task_attribute_value('different','socialCloseness')
	and get_task_state_attribute(Users,'appUsers')
thenceforth
    normalized_social_closeness(Socialness,Users,1.0)
	and wenet_negate_user_value(SocialClosenessUsers,Socialness)
	and put_task_state_attribute('socialClosenessUsers',SocialClosenessUsers).

% Calculate physical closeness if it is nearby
whenever
	is_received_event('sortUsersByDiversity',_)
	and get_task_attribute_value('nearby','positionOfAnswerer')
	and get_task_state_attribute(Users,'appUsers')
	and normalized_closeness_and_raw(PhysicalClosenessUsers,PhysicalClosenessRaw,Users,500)
thenceforth
	put_task_state_attribute('physicalClosenessUsers',PhysicalClosenessUsers)
	and put_task_state_attribute('physicalClosenessRaw',PhysicalClosenessRaw).

:- dynamic  normalized_closeness_and_raw/4, normalized_closeness_and_raw_/5.

normalized_closeness_and_raw(Closeness,Locations,Users,MaxDistance) :-
	(
		get_profile_id(UserId),
		wenet_personal_context_builder_locations(Locations,[UserId|Users]),
		!,
		member(SourceLocation,Locations),
		wenet_user_id_of_location(UserId,SourceLocation),
		!,
		normalized_closeness_and_raw_(Closeness,Users,MaxDistance,Locations,SourceLocation)
	)
	-> true
	; (
		wenet_initialize_user_values(Closeness,Users,0.0),
		Locations = []
	)
	.
normalized_closeness_and_raw_([],[],_,_,_).
normalized_closeness_and_raw_([UserCloseness|ClosenessRest],[UserId|Users],MaxDistance,Locations,SourceLocation) :-
	(
		(
			member(TargetLocation,Locations),
			wenet_user_id_of_location(UserId,TargetLocation),
			!,
			wenet_distance_between_locations(DistanceInMeters,SourceLocation,TargetLocation)
		)
		-> Distance is 1.0 - min(DistanceInMeters,MaxDistance) / MaxDistance
		; Distance = 0.0
	),
	!,
	wenet_new_user_value(UserCloseness,UserId,Distance),
	normalized_closeness_and_raw_(ClosenessRest,Users,MaxDistance,Locations,SourceLocation)
	.

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
	calculate_match_degree_for/2,
	calculate_match_degree_for_/7,
	calculate_user_match_degree_for/7,
	explanation_type_for/6,
	group_indexes_for_domain/8,
	group_indexes_for_value/7,
	group_indexes_for_social/12,
	group_indexes_for_physical/7,
	group_for/5.

calculate_match_degree_for(ReverseSortedMatchUsers,GroupsUsers) :-
	get_task_state_attribute(Users,'appUsers'),
	get_task_attribute_value(Domain,'domain'),
	get_task_state_attribute(DomainInterestUsers,'domainInterestUsers'),
	get_task_state_attribute(BeliefsAndValuesUsers,'beliefsAndValuesUsers'),
	get_task_state_attribute(SocialClosenessUsers,'socialClosenessUsers'),
	get_task_state_attribute(PhysicalClosenessUsers,'physicalClosenessUsers'),
	calculate_match_degree_for_(MatchUsers,GroupsUsers,Users,Domain,DomainInterestUsers,BeliefsAndValuesUsers,SocialClosenessUsers,PhysicalClosenessUsers),
	wenet_sort_user_values_by_value(SortedMatchUsers,MatchUsers),
	reverse(ReverseSortedMatchUsers,SortedMatchUsers)
	.
calculate_match_degree_for_([],[],[],_,_,_,_,_).
calculate_match_degree_for_([MatchUser|MatchUsers],[GroupUser|GroupUsers],[UserId|UserIds],Domain,DomainInterestUsers,BeliefsAndValuesUsers,SocialClosenessUsers,PhysicalClosenessUsers) :-
	calculate_user_match_degree_for(MatchUser,GroupUser,UserId,Domain,DomainInterestUsers,BeliefsAndValuesUsers,SocialClosenessUsers,PhysicalClosenessUsers),
	calculate_match_degree_for_(MatchUsers,GroupUsers,UserIds,Domain,DomainInterestUsers,BeliefsAndValuesUsers,SocialClosenessUsers,PhysicalClosenessUsers)
	.

calculate_user_match_degree_for(MatchUser,GroupUser,UserId,Domain,DomainInterestUsers,BeliefsAndValuesUsers,SocialClosenessUsers,PhysicalClosenessUsers) :-
	wenet_value_of_user_id_from_user_values(DomainInterest,UserId,DomainInterestUsers,@(null)),
	wenet_value_of_user_id_from_user_values(BeliefsAndValues,UserId,BeliefsAndValuesUsers,@(null)),
	wenet_value_of_user_id_from_user_values(SocialCloseness,UserId,SocialClosenessUsers,@(null)),
	wenet_value_of_user_id_from_user_values(PhysicalCloseness,UserId,PhysicalClosenessUsers,@(null)),
	group_indexes_for_domain(MdX,X,SS,SB,HS,HB,Domain,DomainInterest),
	group_indexes_for_value(MdV,Y,SS1,SB1,SS,SB,BeliefsAndValues),
	group_indexes_for_social(MdSC,Z,SS2,SB2,HS1,HB1,SS1,SB1,HS,HB,Domain,SocialCloseness),
	group_indexes_for_physical(MdPC,W,HS2,HB2,HS1,HB1,PhysicalCloseness),
	( (X = 0 , Y = 0, Z = 0, W = 0) -> Value = 0 ; Value is (X*MdX + Y*MdV + Z*MdSC + W*MdPC )/(X + Y + Z + W) ),
	wenet_new_user_value(MatchUser,UserId,Value),
	group_for(Group,SS2,SB2,HS2,HB2),
	explanation_type_for(ExplanationType,Group,PhysicalCloseness,SocialCloseness,DomainInterest,Domain),
  	GroupUser = json([userId=UserId,group=Group,explanationType=ExplanationType])
	.

group_indexes_for_domain(DomainInterest,1,0,0,1,0,'academic_skills',DomainInterest) :-
	number(DomainInterest),
	>(DomainInterest,0.0),
	!.
group_indexes_for_domain(DomainInterest,1,1,0,0,0,Domain,DomainInterest) :-
	number(DomainInterest),
	>(DomainInterest,0.0),
	not(Domain = 'academic_skills'),
	!.
group_indexes_for_domain(0.0,0,0,0,0,1,'academic_skills',DomainInterest) :-
	number(DomainInterest),
	DomainInterest =:= 0.0,
	!.
group_indexes_for_domain(0.0,0,0,1,0,0,_,DomainInterest) :-
	number(DomainInterest),
	DomainInterest =:= 0.0,
	!.
group_indexes_for_domain(0.0,0,0,0,0,0,_,_) :-
	!.

group_indexes_for_value(BeliefsAndValues,1,SS1,SB,SS,SB,BeliefsAndValues) :-
	number(BeliefsAndValues),
	>(BeliefsAndValues,0.0),
	!,
	SS1 is SS + 1.
group_indexes_for_value(0.0,0,SS,SB1,SS,SB,BeliefsAndValues) :-
	number(BeliefsAndValues),
	BeliefsAndValues =:= 0.0,
	!,
	SB1 is SB + 1.
group_indexes_for_value(0.0,0,SS,SB,SS,SB,_) :-
	!.

group_indexes_for_social(SocialCloseness,1,SS1,SB1,HS1,HB,SS1,SB1,HS,HB,'academic_skills',SocialCloseness) :-
	number(SocialCloseness),
	>(SocialCloseness,0.0),
	!,
	HS1 is HS +1
	.
group_indexes_for_social(SocialCloseness,1,SS2,SB1,HS,HB,SS1,SB1,HS,HB,Domain,SocialCloseness) :-
	number(SocialCloseness),
	>(SocialCloseness,0.0),
	not(Domain = 'academic_skills'),
	!,
	SS2 is SS1 + 1
	.
group_indexes_for_social(0.0,0,SS1,SB1,HS,HB1,SS1,SB1,HS,HB,'academic_skills',SocialCloseness) :-
	number(SocialCloseness),
	SocialCloseness =:= 0.0,
	!,
	HB1 is HB + 1.
group_indexes_for_social(0.0,0,SS1,SB2,HS,HB,SS1,SB1,HS,HB,Domain,SocialCloseness) :-
	not(Domain = 'academic_skills'),
	number(SocialCloseness),
	SocialCloseness =:= 0,
	!,
	SB2 is SB1 + 1.
group_indexes_for_social(0.0,0,SS1,SB1,HS,HB,SS1,SB1,HS,HB,_,_) :-
	!.

group_indexes_for_physical(PhysicalCloseness,1,HS1,HB,HS,HB,PhysicalCloseness) :-
	number(PhysicalCloseness),
	>(PhysicalCloseness,0.0),
	!,
	HS1 is HS + 1.
group_indexes_for_physical(0.0,0,HS,HB1,HS,HB,PhysicalCloseness) :-
	number(PhysicalCloseness),
	PhysicalCloseness =:= 0.0,
	!,
	HB1 is HB + 1.
group_indexes_for_physical(0.0,0,HS,HB,HS,HB,_) :-
	!.

group_for(Group,SS,SB,_,0):-
	>(SS,0),
	!,
	Group is 1 + SB.
group_for(4,_,_,_,0):-
	!.
group_for(Group,SS,SB,HS,HB):-
	>(HB,0),
	>(HS,0),
	>(SS,0),
	!,
	Group is 5 + SB.
group_for(8,_,_,HS,HB):-
	>(HB,0),
	>(HS,0),
	!.
group_for(Group,SS,SB,_,_):-
	>(SS,0),
	!,
	Group is 9 + SB.
group_for(12,_,_,_,_):-
	!.

explanation_type_for(group_0,0,_,_,_,_) :- !.
explanation_type_for(group_1,1,_,_,_,_) :- !.
explanation_type_for(group_2_3_4_a,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 2; Group = 3;  Group = 4),
	number(MdPC),
	number(MdSC),
	number(MdX),
	!.
explanation_type_for(group_2_3_4_b,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 2; Group = 3;  Group = 4),
	number(MdPC),
	number(MdSC),
	not(number(MdX)),
	!.
explanation_type_for(group_2_3_4_c,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 2; Group = 3;  Group = 4),
	number(MdPC),
	not(number(MdSC)),
	number(MdX),
	!.
explanation_type_for(group_2_3_4_d,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 2; Group = 3;  Group = 4),
	not(number(MdPC)),
	number(MdSC),
	number(MdX),
	!.
explanation_type_for(group_2_3_4_e,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 2; Group = 3;  Group = 4),
	not(number(MdPC)),
	not(number(MdSC)),
	number(MdX),
	!.
explanation_type_for(group_2_3_4_f,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 2; Group = 3;  Group = 4),
	not(number(MdPC)),
	number(MdSC),
	not(number(MdX)),
	!.
explanation_type_for(group_2_3_4_g,Group,_,_,_,_) :-
	(Group = 2; Group = 3;  Group = 4),
	!.
explanation_type_for(group_5_a,5,MdPC,MdSC,MdX,'academic_skills') :-
	number(MdPC),
	MdPC =:= 0.0,
	number(MdSC),
	MdSC =:= 0.0,
	(not(number(MdX));(number(MdX),not(MdX =:= 0.0))),
	!.
explanation_type_for(group_5_b,5,MdPC,MdSC,MdX,'academic_skills') :-
	number(MdPC),
	MdPC =:= 0.0,
	(not(number(MdSC));(number(MdSC),not(MdSC =:= 0.0))),
	number(MdX),
	MdX =:= 0.0,
	!.
explanation_type_for(group_5_c,5,MdPC,MdSC,MdX,'academic_skills') :-
	(not(number(MdPC));(number(MdPC),not(MdPC =:= 0.0))),
	number(MdSC),
	MdSC =:= 0.0,
	number(MdX),
	MdX =:= 0.0,
	!.
explanation_type_for(group_5_d,5,MdPC,MdSC,MdX,'academic_skills') :-
	number(MdPC),
	MdPC =:= 0.0,
	(not(number(MdSC));(number(MdSC),not(MdSC =:= 0.0))),
	(not(number(MdX));(number(MdX),not(MdX =:= 0.0))),
	!.
explanation_type_for(group_5_e,5,MdPC,MdSC,MdX,'academic_skills') :-
	(not(number(MdPC));(number(MdPC),not(MdPC =:= 0.0))),
	number(MdSC),
	MdSC =:= 0.0,
	(not(number(MdX));(number(MdX),not(MdX =:= 0.0))),
	!.
explanation_type_for(group_5_f,5,_,_,_,_) :-
	!.
explanation_type_for(group_6_7_8_a,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 6; Group = 7;  Group = 8),
	number(MdPC),
	MdPC =:= 0.0,
	number(MdSC),
	MdSC =:= 0.0,
	(not(number(MdX));(number(MdX),not(MdX =:= 0.0))),
	!.
explanation_type_for(group_6_7_8_b,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 6; Group = 7;  Group = 8),
	number(MdPC),
	MdPC =:= 0.0,
	(not(number(MdSC));(number(MdSC),not(MdSC =:= 0.0))),
	number(MdX),
	MdX =:= 0.0,
	!.
explanation_type_for(group_6_7_8_c,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 6; Group = 7;  Group = 8),
	(not(number(MdPC));(number(MdPC),not(MdPC =:= 0.0))),
	number(MdSC),
	MdSC =:= 0.0,
	number(MdX),
	MdX =:= 0.0,
	!.
explanation_type_for(group_6_7_8_d,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 6; Group = 7;  Group = 8),
	number(MdPC),
	MdPC =:= 0.0,
	(not(number(MdSC));(number(MdSC),not(MdSC =:= 0.0))),
	(not(number(MdX));(number(MdX),not(MdX =:= 0.0))),
	!.
explanation_type_for(group_6_7_8_e,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 6; Group = 7;  Group = 8),
	(not(number(MdPC));(number(MdPC),not(MdPC =:= 0.0))),
	number(MdSC),
	MdSC =:= 0.0,
	(not(number(MdX));(number(MdX),not(MdX =:= 0.0))),
	!.
explanation_type_for(group_6_7_8_f,Group,_,_,_,_) :-
	(Group = 6; Group = 7;  Group = 8),
	!.
explanation_type_for(group_9_10_11_a,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 9; Group = 10;  Group = 11),
	number(MdPC),
	number(MdPC),
	number(MdSC),
	number(MdX),
	!.
explanation_type_for(group_9_10_11_b,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 9; Group = 10;  Group = 11),
	number(MdPC),
	number(MdPC),
	number(MdSC),
	not(number(MdX)),
	!.
explanation_type_for(group_9_10_11_c,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 9; Group = 10;  Group = 11),
	number(MdPC),
	not(number(MdSC)),
	number(MdX),
	!.
explanation_type_for(group_9_10_11_d,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 9; Group = 10;  Group = 11),
	not(number(MdPC)),
	number(MdSC),
	number(MdX),
	!.
explanation_type_for(group_9_10_11_e,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 9; Group = 10;  Group = 11),
	not(number(MdPC)),
	not(number(MdSC)),
	number(MdX),
	!.
explanation_type_for(group_9_10_11_f,Group,MdPC,MdSC,MdX,'academic_skills') :-
	(Group = 9; Group = 10;  Group = 11),
	not(number(MdPC)),
	number(MdSC),
	not(number(MdX)),
	!.
explanation_type_for(group_9_10_11_g,Group,_,_,_,_) :-
	(Group = 9; Group = 10;  Group = 11),
	!.
explanation_type_for(group_12,12,_,_,_,_) :- !.
explanation_type_for(@(null),_,_,_,_,_) :- !.


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
	and get_profile_language(Lang)
	and get_task_state_attribute(Unasked,'unaskedUserIds')
thenceforth
	send_user_message('AnsweredQuestionMessage',json([taskId=TaskId,question=Question,transactionId=TransactionId,answer=Answer,userId=SenderId,anonymous=Anonymous]))
	and wenet_add(NewAnswersTransactionIds,TransactionId,AnswersTransactionIds)
	and put_task_state_attribute('answersTransactionIds',NewAnswersTransactionIds)
	and send_event(_,1,'checkMaxAnswers',json([]))
	and explanation(ExplanationTitle,ExplanationText,SenderId,Unasked,GroupsUsers,Lang)
	and send_user_message('TextualMessage',json([title=ExplanationTitle,text=ExplanationText])).

:- dynamic
	explanation/6,
	explanation_title/2,
	explanation_text/3.

explanation(ExplanationTitle,ExplanationText,UserId,Unasked,GroupsUsers,Lang) :-
	explanation_title(ExplanationTitle,Lang),
	( member(UserId,Unasked)
		-> Type = group_unexpected ;
		(
			(wenet_json_element_with(json(Group),GroupsUsers,userId=UserId,json([explanationType=group_0])), member(explanationType=Type,Group))
			-> true
			; Type = group_unexpected
		)
	),
	explanation_text(ExplanationText,Type,Lang).

explanation_title('Why is this user chosen?',_).
explanation_text('Recall that there were no requirements set w.r.t domains, values, social or physical closeness. Nevertheless, we tried to increase the gender diversity of selected users.',group_0,_).
explanation_text('This user fulfils all requirements. While searching for users, we tried to increase the gender diversity of selected users.',group_1,_).
explanation_text('Not enough members fulfil the requirements. To find some answers, we had to choose some that do not fulfil any, like this user. While doing so, we also tried to increase the gender diversity of selected users.',group_12,_).
explanation_text('This user fulfils the physical closeness, social closeness, and academic skills requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_a,_).
explanation_text('This user fulfils the physical and social closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_b,_).
explanation_text('This user fulfils the academic skills and physical closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_c,_).
explanation_text('This user fulfils the academic skills and social closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_d,_).
explanation_text('This user fulfils the academic skills requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_e,_).
explanation_text('This user fulfils the social closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_f,_).
explanation_text('This user fulfils the physical closeness requirements, but not all of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_2_3_4_g,_).
explanation_text('This user does not fulfil neither the physical closeness, social closeness, and academic skills requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_a,_).
explanation_text('This user does not fulfil neither the physical and social closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_b,_).
explanation_text('This user does not fulfil neither the academic skills and physical closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_c,_).
explanation_text('This user does not fulfil neither the academic skills and social closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_d,_).
explanation_text('This user does not fulfil neither the academic skills requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_e,_).
explanation_text('This user does not fulfil neither the social closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_f,_).
explanation_text('This user does not fulfil neither the physical closeness requirements, nor some of the other requirements. To find some answers, we had to relax some of the other requirements. We also tried to increase the gender diversity of selected users.',group_9_10_11_g,_).
explanation_text('This user does not fulfil the physical and social closeness requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_a,_).
explanation_text('This user does not fulfil the academic skills and physical closeness requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_b,_).
explanation_text('This user does not fulfil the academic skills and social closeness requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_c,_).
explanation_text('This user does not fulfil the physical closeness requirement. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_d,_).
explanation_text('This user does not fulfil the social closeness requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_e,_).
explanation_text('This user does not fulfil the academic skills requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_5_f,_).
explanation_text('This user does not fulfil neither the physical and social closeness requirements nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_a,_).
explanation_text('This user does not fulfil neither the academic skills and physical closeness requirements nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_b,_).
explanation_text('This user does not fulfil neither the academic skills and social closeness requirements nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_c,_).
explanation_text('This user does not fulfil neither the physical closeness requirement nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_d,_).
explanation_text('This user does not fulfil neither the social closeness requirements nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_e,_).
explanation_text('This user does not fulfil neither the academic skills requirements nor some of the other requirements. To find some answers, we had to relax these requirements. We also tried to increase the gender diversity of selected users.',group_6_7_8_f,_).
explanation_text('This answer does not match your original criteria but maybe you will still find it interesting.',_,_).

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

% Notify the user that its answer is picked
whenever
	is_received(_,'closeQuestionTransaction',_)
	and get_profile_id(Me)
	and get_task_requester_id(Me)
	and not(is_task_closed())
thenceforth
	add_message_transaction()
	and close_task()
	.

% Nothing to do with this transaction only store it
whenever
	is_received_do_transaction('reportAnswerTransaction',_)
	and get_profile_id(Me)
	and get_task_requester_id(Me)
	and not(is_task_closed())
thenceforth
	add_message_transaction().

% Nothing to do with this transaction only store it
whenever
	is_received_do_transaction('likeAnswerTransaction',Attributes)
	and get_profile_id(Me)
	and get_task_requester_id(Me)
	and not(is_task_closed())
	and get_attribute(TransactionId,transactionId,Attributes)
	and get_transaction(_,TransactionId)
thenceforth
	add_message_transaction().

% Nothing to do with this transaction only store it
whenever
	is_received_do_transaction('followUpTransaction',Attributes)
	and get_profile_id(Me)
	and not(get_task_requester_id(Me))
	and not(is_task_closed())
	and get_attribute(TransactionId,transactionId,Attributes)
	and get_transaction(_,TransactionId)
thenceforth
	add_message_transaction().

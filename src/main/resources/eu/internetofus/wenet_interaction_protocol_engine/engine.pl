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

:- discontiguous
	normengine/1,
	assert_todo_actions/1,
	need_to_repeat/3,
	remove_duplicates/3,
	merge_lists/3,
	remove_duplicates2/2,
	check_condition/1,
	recursive_norm_check/3,
	check_norms/2,
	execute_conclusion/2
	.
:- dynamic fact/1.
:- dynamic todo/1.
:- op(800, fx, whenever).
:- op(100, fy, isfalse).
:- op(100, fy, istrue).
:- op(700, xfx, thenceforth).
:- op(300, xfy, or).
:- op(200, xfy, and).


% normengine(- Output) will be called by Bruno to get the norm engine started.
% normengine will be called for ONE incomming message only (and not a set),
% 		which is connected to ONE community only,
% 		and friends ONLY in that community.
normengine(Output) :-
    % We assume the user id is loaded as myuser(IdNUmber)
    % We assume the user profile is already loaded (as the normengine may need to access some parameters)
    % We also assume the norms and knowledge base of the user and the community are loaded as predicates
    % (and not hidden within the hideous profile structure)
    % For now we only deal with community norms, in the future we need to distinguish community norms from user norms
    getnorms(AllNorms),
    recursive_norm_check([],AllNorms,Output1),
    remove_conflicts([],Output1,Output2),
    print(Output2),
    remove_negations(Output2,Output).

getnorms(AllNorms) :-
  findall([Condition, Conclusion], whenever Condition thenceforth Conclusion, AllNorms), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     NORM ASSESSMENT     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%recursive_norm_check(Output1,AllNorms,Output) :-
%    assert_todo_actions(Output1),
%    check_norms(AllNorms,Output2),
%    need_to_repeat(Output1,Output2,X),
%    remove_duplicates(Output1,Output2,Output3),
%    (   (   X==0, !, Output=Output3	) ;
%    	(   X==1, recursive_norm_check(Output3,AllNorms,Output)	)	).

recursive_norm_check(_Output1,AllNorms,Output) :- check_norms(AllNorms,Output).

assert_todo_actions([]).
assert_todo_actions([todo(X)|T]) :-
    (   (   todo(X),!	)	;		   % NARDINE: what about variables??
    	(   assertz(todo(X))	)	),
    assert_todo_actions(T).
assert_todo_actions([H|T]) :-			% NARDINE: should we raise an error if not of type tod?
    \+functor(H, todo, _),
    assert_todo_actions(T).

need_to_repeat(_List,[],0).
need_to_repeat(List,[H|T],X) :-
    member(H,List),		% Nardine: Is this enough?! What if we have uninstantiated variables then get instantiated when checking membership?
    need_to_repeat(List,T,X).
need_to_repeat(List,[H|_],1) :-
    \+member(H,List).

remove_duplicates(List1,List2,Output) :-
    merge_lists(List1,List2,List3),	% Nardine: Merge is not needed if List2 is really a subset of List1.
    remove_duplicates2(List3,Output).

merge_lists([],List,List).
merge_lists([H|T],List,[H|List2]):-
    merge_lists(T,List,List2).

remove_duplicates2([],[]).
remove_duplicates2([H|T],List) :-
     member(H,T),		% Nardine: To assess the use of member with ungrounded variables.
     remove_duplicates2(T,List).
remove_duplicates2([H|T],[H|List]) :-
      \+member(H,T),
      remove_duplicates2(T,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CORE OF NORM ASSESSMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_norms([],[]).
check_norms([[Condition,Conclusion]|Norms],[OutputH|OutputT]) :-
    check_condition(Condition), !,
    execute_conclusion(Conclusion,OutputH),
    check_norms(Norms,OutputT).
check_norms([[_Condition,_Conclusion]|Norms],[OutputT]) :-
    % \+check_condition(Condition),
    check_norms(Norms,OutputT).

check_condition(not(C)) :- !,
    (
      ( call(C) , ! , fail ) ;
      ( ! , true )
    ).
check_condition(C1 and C2) :- !,
	check_condition(C1),
	check_condition(C2), !.
check_condition(C1 or C2) :- !,
	( check_condition(C1) , ! ) ;
	( check_condition(C2) , ! ) .
check_condition(C) :-
   call(C).

execute_conclusion(Conclusion1 and Conclusion2,Output) :- !,
	execute_conclusion(Conclusion1,C1),
	execute_conclusion(Conclusion2,C2),
    append(C1,C2,Output).
execute_conclusion(Conclusion,[put(Conclusion)])  :- !,
    assertz(Conclusion).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CORE OF CONFLICT RESOLUTION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_conflicts(ConfirmedList,[],ConfirmedList) :- !.
remove_conflicts(ConfirmedList,[H|T],Output) :-
    		no_conflict(H,ConfirmedList), !,
    		append(ConfirmedList,[H],ConfirmedList2),
    		remove_conflicts(ConfirmedList2,T,Output).
remove_conflicts(ConfirmedList,[H|T],Output) :-
    				\+ no_conflict(H,ConfirmedList), !,
    				remove_conflicts(ConfirmedList,T,Output).

no_conflict([put(not(X))],List) :- !,
    		\+ member([put(X)],List),
    		\+ member([put(delay(X,_))],List).
no_conflict([put(delay(X,_))],List) :- !,
    		\+ member([put(X)],List),
    		\+ member([put(not(X))],List).
no_conflict([put(X)],List) :- !,
    		\+ member([put(delay(X,_))],List),
    		\+ member([put(not(X))],List).

remove_negations([],[]).
remove_negations([[put(not(_))]|T],Result) :- !,
    remove_negations(T,Result).
remove_negations([[put(X)]|T],[[put(X)]|Result]) :- !,
        remove_negations(T,Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NARDINES INPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

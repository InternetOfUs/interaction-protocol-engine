%
% Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.
%


:- dynamic fact/1.
:- dynamic todo/1.
:- op(800, fx, whenever).
:- op(100, fy, isfalse).
:- op(100, fy, istrue).
:- op(700, xfx, thenceforth).
:- op(300, xfy, or).
:- op(200, xfy, and).


normengine(Output) :-
    % We assume the user id is loaded as myuser(IdNUmber)
    % We assume the user profile is already loaded (as the normengine may need to access some parameters)
    % We also assume the norms and knowledge base of the user and the community are loaded as predicates
    % (and not hidden within the hideous profile structure)
    % For now we only deal with community norms, in the future we need to distinguish community norms from user norms
    findall([Condition, Conclusion], whenever Condition thenceforth Conclusion, AllNorms),
    recursive_norm_check([],AllNorms,Output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     NORM ASSESSMENT     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%recursive_norm_check(Output1,AllNorms,Output) :-
%    assert_towenet_do_actions(Output1),
%    check_norms(AllNorms,Output2),
%    need_to_repeat(Output1,Output2,X),
%    remove_duplicates(Output1,Output2,Output3),
%    (   (   X==0, !, Output=Output3	) ;
%    	(   X==1, recursive_norm_check(Output3,AllNorms,Output)	)	).

recursive_norm_check(_Output1,AllNorms,Output) :- check_norms(AllNorms,Output).

assert_towenet_do_actions([]).
assert_towenet_do_actions([todo(X)|T]) :-
    (   (   todo(X),!	)	;		   % NARDINE: what about variables??
    	(   assertz(todo(X))	)	),
    assert_towenet_do_actions(T).
assert_towenet_do_actions([H|T]) :-			% NARDINE: should we raise an error if not of type tod?
    \+functor(H, todo, _),
    assert_towenet_do_actions(T).

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

check_condition(not(C)) :-
    ( call(C) , ! , fail );
    ( ! , true ).
check_condition(C1 and C2) :-
	check_condition(C1),
	check_condition(C2), !.
check_condition(C1 or C2) :-
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NARDINE'S INPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

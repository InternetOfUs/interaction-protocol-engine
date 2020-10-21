
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


% Input initialise receives the profiles of friends. Bruno at initialization time will import 
% two files, (1) with instances of friend(X,[c1, ...,cl]), a friend id and the list of 
% communities we both belong to, and (2) instances of norms whenever X henceforth Y.
% then 
% normengine(-input, +[com1, ..., comn]) will be called by Bruno. 
% com are send(X, phi) to others or notify(info) or save(data) set_alarm(Time) 
% info can be show(Stuff), per(Action), for(Action), permission for an action and prohibition for an action and input is 
% receive(X,phi) or wake_up.
%TODO
% 1) Adapt the input/output to what discussed with Bruno. A predicate normengine has to be defined.
% 2) Make the norms of a community a file that is read into the interpreter. The name can be an argument
% 1 bis) normengine(-input, +[com1, ..., comn],[normfile1, ... normfilem]) this should an ellaboration of the current forward.
% 3) Improve the metainterpreter to keep the proof tree of every msg, action, etc.



% support code

:- dynamic fact/1.
:- op(800, fx, whenever).                     
:- op(100, fy, isfalse).                    
:- op(100, fy, istrue).                     
:- op(700, xfx, thenceforth).
:- op(300, xfy, or).
:- op(200, xfy, and).

% Simple forward chaining in Prolog. This control has to change in order to saturate. 
% We need to see if the language is enough

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START NARDINE's INPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These allow us to modify the input types (message, action, etc.) 
% wihtout having to play with the code below.
% The first argument specifies the functor, 
% the second specifies the clause that contains the details of the action 
% (and most importantly, the community), and the third specifies the number of the argument
% of the clause that holds the community Id.
% E.G. 'act(me, open_door(_,_,comId1,_))' will have the input type: 'input_type(act,2,3)'. 
% NOTE: These cannot be part of the community norms files 
% 		because we use these to know which community norms to load.
% 		If we want to generalise, each community can have a norms file, and another just
% 		specifying these actions (to address the security of norms). 
% 		But this will require further changes to this norm engine to get those files.
input_type(act,2,1).
input_type(msg,3,1).


% Input: these are the set of actions that have been performed, 
% 		that the norm engine needs to deal with.
% 		Bruno to send these to the norm engine, and they take the form:
% 		'act(User,Action)' or 'msg(Sender,Receiver,Msg)'.
% 		The 'User' and 'Sender' should be known to the norm engine, 
% 		but we are keeping them here for now.
% Output: this is the set of actions to be performed. 
% 		This is the output of the Norm Engine that Bruno is interested in. They take the form: 
% 		'send(FId,Msg)', 'notify(Info)', 'save(Data)', or set_alarm('Time'). 
% 		'Info' in 'notify(Info)' can be: 'show(Stuff)', 'per(Action)', 'for(Action)', 
% 		permission for an action and prohibition for an action and input is 
% 		receive(X,phi) or wake_up.
% Profile: this is the user's profile (a file). It includes the list of friends, 
% 		which is *currently* the only thing we are interested in. 
% 		But this will change with time.
% 		Friends are specified through instances of 'friend(fId,[comId1, ...,comIdN])', 
% 		that is, through a friend id 'fId' and the list of communities 
% 		that both the user and the friend belong to.
% 		BRUNO: this will always be a file sent to the norm engine? Or can it be a fixed file
% 		known to the norm engine that it automatically loads? 
% 		This means one instance of a norm engine per user!
% IndividualNorms: this is the file compiling the user's individual norms. 
% 		These are not currently used, but are here for future use.
% 		BRUNO: this will always be a file sent to the norm engine? Or can it be a fixed file
% 		known to the norm engine that it automatically loads? 
% 		This means one instance of a norm engine per user!
% Communities: these are the files compiling a community's norms. 
% 		The users gets the norms of all the communities he is a member of. 
% 		BRUNO: this will always be a file sent to the norm engine? Or can it be a fixed file
% 		known to the norm engine that it automatically loads? 
% 		This means one instance of a norm engine per user!
% IMPORTANT: ProfileFile, IndividualNormsFile, and ComNormFiles are sent to 
% 		the norm engine everytime, because people's profiles (and their contact list) changes, 
% 		and so does their norms and the community membership. See question to Bruno above
%		for deciding whether to have these as fixed files or not.

% If the profile file, individual norms file, and communities files are automatically obtained, 
% then the arguments may be dropped. To discuss with BRUNO
normengine([],_,_,_,[]) :- !.
normengine(Input,Profile,IndividualNorms,Communities,Output) :-
    initialise(Input,Profile,IndividualNorms,Communities),
    findall([Condition, Conclusion], whenever Condition thenceforth Conclusion, AllNorms),
    check_norms(AllNorms,OutputX),
    filter_output(OutputX,Output).

check_norms([],[]).
check_norms([NormH|NormT],[OutputH|OutputT]) :-
    check_norm(NormH,OutputH),
    check_norms(NormT,OutputT).

check_norm([Condition, Conclusion],Output) :- 
    check_condition(Condition), !,
    execute_conclusion(Conclusion,Output).

check_condition(C) :-
    call(C).
check_condition(not(C)) :-
    ( call(C) , ! , fail );
    ( ! , true ).
check_condition(C1 and C2) :-
	check_condition(C1),
	check_condition(C2), !. 
check_condition(C1 or C2) :-
	( check_condition(C1) , ! ) ;
	( check_condition(C2) , ! ).

execute_conclusion(Conclusion1 and Conclusion2,Output) :-
	execute_conclusion(Conclusion1,C1),
	execute_conclusion(Conclusion2,C2), !,
    append(C1,C2,Output). 
execute_conclusion(Conclusion,Output)  :- !, 
    assertz(Conclusion). 

filter_output([],[]).
filter_output([H|T],Output) :-
    functor(H,msg,_).

initialise(_,_,_,[]) :-
    write("ERROR: You cannot be receiving actions from your user, or messages from others, if you are not in any community."),
    nl, write("ERROR: You have to be in a community to interact with its members."), nl, !.
	% THIS ERROR NEEDS TO BE RELAYED TO BRUNO... DO I NEED TO GET INTO THE EXACT CASE OF THE ERROR?
initialise(Input,Profile,IndividualNorms,Communities) :-
    consult(Profile), consult(IndividualNorms), 
    assert_new_actions(Communities,Input,RelevantCommunities), 
    sort(RelevantCommunities,RelevantCommunities2),	% remove duplicates
    consultCommunityNorms(RelevantCommunities2).
    
% Recursively load all Input, and keep track of their relevant communities,
% only if they are associated to a community that the user is a member of.
assert_new_actions(_Communities,[],[]) :- !.
assert_new_actions(Communities, [H | T], [ComH|ComT]) :-
    community(H,ComH), 
    member(ComH,Communities),!,
    assertz(input(H)), 
    assert_new_actions(Communities,T,ComT).
assert_new_actions(Communities, [H | T], [ComT]) :-
    community(H,ComH), 
    \+ member(ComH,Communities),!,
    write("ERROR: Received an input associated to the community '"), 
    write(ComH), write("' which the user is not a member of!"), nl,
    write("As such, neglicting this input: "), write(H), write("'"), nl,
    assert_new_actions(Communities,T,ComT).

% Find the community that an action is associated with.
community(Input,ComId) :- 
    functor(Input,F,A),
    input_type(F,X,Y), 
    (   (   A>= Y, !, arg(X,Input,Details), arg(Y,Details,ComId));
    	(   write("ERROR: The arities specified and used for the predicate: "), 
        	write(F), write(" do not match!"), nl )	). % BRUNO send error to bruno?

% Recursively load all relevant communities' norms, one by one.
consultCommunityNorms([]) :- !.
consultCommunityNorms([ComIdH|ComIdT]) :-
    consult(ComIdH), % BRUNO: we need to agree on the naming of files
    consultCommunityNorms(ComIdT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NARDINE'S INPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forward :-
   whenever Cond thenceforth Concl, 
 %  write( Cond),nl,				 % A rule   
    composed_fact( Cond),
    not(solve(Concl)),
    write('Conclude: '),write( Concl), nl,
    add_composed_fact(Concl), forward.
forward:-
    write( 'No more deduced facts'),nl,write('Final DB: '),nl,write_all_facts.             % Rule's conclusion not yet a fact


composed_fact( Cond)  :- fact(Cond).                         % Simple fact 
composed_fact( isfalse Cond)     :- solve(Cond),!,fail;true. % Forcing the evaluación of the predicate 
composed_fact( istrue Cond)      :- solve(Cond).             % Forcing the evaluación of the predicate 
composed_fact( Cond1 and Cond2)  :-
%    write(Cond1),nl, write(Cond2),nl,
   composed_fact( Cond1),
   composed_fact( Cond2).            % Both conjuncts true 
composed_fact( Cond1 or Cond2)  :-
   composed_fact( Cond1)
   ;
   composed_fact( Cond2).




add_composed_fact( Concl1 and Concl2)  :-
%    write(Cond1),nl, write(Cond2),nl,
   add_composed_fact( Concl1),
   add_composed_fact( Concl2), !.            % Both conjuncts added 
add_composed_fact( Concl)  :- !, assertz(fact(Concl)).                         % Simple fact 


write_all_facts :-fact(X), write(X),nl,fail;true.

% Meta interpreter

solve(fail)  :-!,fail.
solve(true)  :-!.
solve((A,B)) :-!, solve(A), solve(B). 
solve(A)     :- clause(A, B), solve(B).
solve(A)	 :- fact(A).
solve( A and B)	:- solve(A), solve(B).

%%===================Hello world norms===============================================================
%whenever
%msg(X,me,Phi) and friend(Y,L) and isfalse X == Y and istrue community(Phi,C) and istrue member(C,L)
%thenceforth
%msg(me,Y,Phi).
%
%
%%==================uHELP NORMS, plus auxiliar code. Should be a file===================================
%
%% Predicates 
%
%deadline_volunteers_reached(_) :- false.
%
%% rules from actions to state of mind
%
%% I desire to get the tasks I create completed.
%whenever
%act(create_task(Args))
%thenceforth
%des(me,task(Args)).
%
%
%% I don't desire to solve the tasks I decide to cancel.
%whenever
%act(cancel_task(Args))
%thenceforth
%neg(des(me,task(Args))).
%
%% I intend that the volunteer I select for a task intends to solve the task.
%whenever
%act(select_volunteer(X,task(Args))) and bel(me,des(X,task(Args)))
%thenceforth
%int(me,int(X,task(Args))).
%
%% rules from messages to state of mind
%
%% Someone declinig to solve a task does not desire to solve it.
%whenever
%msg(X,me,decline(Task))
%thenceforth
%bel(me,neg(des(X,Task))).
%
%% A volunteer for a task, that volunteers on time desires to solve the task.
%whenever
%msg(X,me,accept(Task)) and isfalse deadline_volunteers_reached(Task)
%thenceforth
%bel(me,des(X,Task)).
%
%% If someone says the task is completed, means the task was successful, and I'm no longer desiring to vomplete the task.
%whenever
%msg(X,me,done(Taskid)) 
%thenceforth
%bel(me,success(X,task(Taskid))) and neg(des(me,task(Taskid))).
%
%% When the deadline of a task is reached, the intentin to reach it decays.
%whenever
%des(me,task(Taskid)) and istrue deadline_task_reached(Taskid)
%thenceforth
%neg(int(me,task(Taskid))).
%
%% If an agent declines to perform a task, I assume it does not want to do it.
%whenever
%msg(X,me,decline(Taskid)) 
%thenceforth
%bel(me,neg(des(X,task(Taskid)))).
%
%% If an agent declines to perform a task it intended to do, I assume it failed to do it.
%whenever
%msg(X,me,decline(Taskid)) and bel(me,int(X,task(Taskid)))
%thenceforth
%bel(me,failed(X,task(Taskid))).
%
%
%%facts
%fatc(act(create_task(kissme))).
%fact(act(create_task(kickme))).                                   
%fact(msg(nardine,me,accept(kissme))).
%fact(msg(carmeta,me,accept(kickme))).
%fact(msg(nardine,me,done(kissme))).
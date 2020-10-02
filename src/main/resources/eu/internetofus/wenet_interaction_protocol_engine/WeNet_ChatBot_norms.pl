% Task is represented as a list of attributes in this order:
% [ ID,
% 	Name,
% 	[Description, Location, Eventa Date, Number of people, ...]
% 	Deadline for voliunteering,
% 	Unanswered,
% 	Declined,
% 	Volunteered,
% 	Chosen Volunteers,
% 	...
% 	]


% Notes from Bruno: for now, the rules only focus on managing the tasks. 
% If we want to manage the app behaviour, we need another set of rules.
% Now, one norm engine sends messages directly to other users... 
% Soon, we should split between the norm engine and the user's app. 
% A user's norm engine first should decide what to do with received messages.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required Predicates for this community's norms:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BRUNO, as such, I need the whole file to load it as is, and not just the set of norms
task_deadline(Task,TD) :-
    nth1(4, Task, TD).

task_volunteers(Task,V) :-	% final numbers to be fixed, when finalised with Bruno
    nth1(7, Task, V).

task_unanswered(Task,NotReplied) :-	
    nth1(5, Task, NotReplied).

task_chosen_volunteers(Task,ChosenVolunteers) :-	
    nth1(8, Task, ChosenVolunteers).
    
update_unanswered(Task,X,Task2) :-
	replace_element_at(5,Task,X,Task2).

add_volunteered(X,Task,Task2) :-
    nth1(7, Task, V),
    append(V,[X],V2),
    replace_element_at(7,Task,V2,Task1),
    nth1(5,Task1,NotReplied),
    delete(NotReplied,X,NotReplied2),
    replace_element_at(5,Task1,NotReplied2,Task2).

add_declined(X,Task,Task2) :-
    nth1(6, Task, V),
    append(V,[X],V2),
    replace_element_at(6,Task,V2,Task1),
    nth1(5,Task1,NotReplied),
    delete(NotReplied,X,NotReplied2),
    replace_element_at(5,Task1,NotReplied2,Task2).

add_chosen_volunteer(X,Task,Task2) :-
    nth1(8, Task, V),
    append(V,[X],V2),
    replace_element_at(8,Task,V2,Task1),
    nth1(7,Task1,Volunteers),
    delete(Volunteers,X,Volunteers2),
    replace_element_at(7,Task1,Volunteers2,Task2).

replace_element_at(1, [_|L], E, [E|L]).
replace_element_at(N, [X|L], E, [X|R]) :-
  succ(M, N),
  replace_element_at(M, L, E, R).

msg_new_volunteer_found(V,Msg) :-
    term_string(V,Volunteer),
	string_concat("The WeNet user, ", Volunteer, Part1),
	string_concat(Part1, ", has volunteered!", Msg).

msg_volunteer_chosen(Task,Msg) :-
    nth1(2, Task, TaskName),
    term_string(TaskName,TN),
	string_concat("You've been chosen for: ", TN, Msg).

msg_help_not_needed(Task,Msg) :-
    nth1(2, Task, TaskName),
    term_string(TaskName,TN),
	string_concat("Your volunteering is no longer required for: ", TN, Msg).

msg_task_closed(Task,Msg) :-
    nth1(2, Task, TaskName),
    term_string(TaskName,TN),
	string_concat("The following task you're involved in has been closed: ", TN, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 1: creating a new task
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(MyUser,me,create_task(Task)) and % me should be defined
	user(me,MyUser) and 
	app_users(AllUsers) and 
	incentive_server_id(IS) and
	task_manager(TM)	% BRUNO, is my understanding of Task Manager correct?
thenceforth
	select(me, AllUsers, AllUsers2) and 	% Removes "me" from "AllUsers"
	update_unanswered(Task,AllUsers2,Task2) and	% Updates the unanswered attribute of "Task"
	msg(me,TM,new_task(Task2)) and
	msg(me,AllUsers2,new_task(Task2)) and 	% BRUNO, Is the task ID needed?
	msg(me,IS,new_task(Task, "taskCreated", "A task is created")).
		% BRUNO, not sure the middle parameter above is still needed given the "new_task" message type?
		% BRUNO, is the task ID needed?
% CARELS, when to use obligations? Shall all message sending actions be changed to obligations? Seems not!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 2a: someone volunteers 
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(X,me,volunteer_for(Task)) and 
	task_deadline(Task,TD) and
	TD =< now								% BRUNO, in your code it is "now < TD"!
thenceforth
	msg(me,X,deadline_reached(Task,"It's too late to be a volunteer")).
		% BRUNBO, shall I use the ID above?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 2b: someone volunteers
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(X,me,volunteer_for(Task)) and 
	task_deadline(Task,TD) and
	now < TD and							% BRUNO, in your code it is "TD < now"!
	task_unanswered(Task,NotReplied) and
	\+ member(X,V)
thenceforth
	msg(me,x,already_volunteered(Task,"Cannot volunteer, as you have already replied to this task request!")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 2c: someone volunteers
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(X,me,volunteer_for(Task)) and 
	task_deadline(Task,TD) and
	now < TD and							% BRUNO, in your code it is "TD < now"!
	task_unanswered(Task,U) and
	member(X,U) and
	task_id(Task, Id) and
	incentive_server_id(IS) and
	task_manger(TM) and
	user(me,MyUser)
thenceforth
	add_volunteered(X,Task,Task2) and
	msg(me,TM,update_task(Task2)) and
	retrieveSocialExplanation(X,Id,Explanation) and		% BRUNO, How to call the social builder to get an explanation? No messages sent, but a direct connection that waits for an explanation! I am also adding the explanation here.
	msg_new_volunteer_found(X,Msg) and 					% BRUNO, do we need to send the Volunteer Id and Explanation in seperate fields? Is this info saved on the phone? Or just need to be printed to the user to read?
	msg(me,MyUser,Msg) and								% CARLES, BRUNO, are there messages sent back to the user different than notifications? Do I need to specify this is a notification? 
	msg(me,MyUser,Explanation) and
	msg(me,IS,task_updated(Task, "New Volunteer", "")). 	% BRUNO, the format does not follow the previous notification to the incentive server, let us make them uniform?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 3a: someone declines
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(X,me,decline(Task)) and 
	task_deadline(Task,TD) and
	TD =< now											% BRUNO, in your code it is "now < TD"!
thenceforth
	msg(me,X,deadline_reached(Task,"Deadline to react to this task has already passed.")).
		% BRUNBO, shall I use the ID above?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 3b: someone declines
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(X,me,decline(Task)) and 
	task_deadline(Task,TD) and
	now < TD and										% BRUNO, in your code it is "TD < now"!
	task_unanswered(Task,NotReplied) and
	\+ member(X,V)
thenceforth
	msg(me,x,already_volunteered(Task,"Cannot decline, as you have already replied to this task request!")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 3c: someone declines
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(X,me,decline(Task)) and 
	task_deadline(Task,TD) and
	now < TD and										% BRUNO, in your code it is "TD < now"!
	task_unanswered(Task,U) and
	member(X,U) and
	task_id(Task, Id) and
	incentive_server_id(IS) and
	task_manger(TM)
thenceforth
	add_declined(X,Task,Task2) and
	msg(me,TM,update_task(Task2)) and
	msg(me,IS,task_updated(Task, "Someone declined", "")). 	% BRUNO, the format does not follow the previous notification to the incentive server, let us make them uniform?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 4a: choosing a volunteer
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(MyUser,me,choose_volunteer(Task,V)) and 
	user(me,MyUser) and 
	task_volunteers(VList) and
	\+ member(V,VList)
thenceforth
	msg(me,MyUser,"Cannot choose this person, as they have not volunteered for this task.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 4b: choosing a volunteer
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(MyUser,me,choose_volunteer(Task,V)) and 
	user(me,MyUser) and 
	task_volunteers(VList) and
	member(V,VList) and
	incentive_server_id(IS) and
	task_manger(TM)
thenceforth
	add_chosen_volunteer(V,Task,Task2) and
	msg(me,TM,update_task(Task2)) and
	msg_volunteer_chosen(Task,Msg)
	msg(me, V, Msg) and
	msg(me,IS,task_updated(Task, "A volunteer was chosen", "")). % BRUNO, the format does not follow the previous notification to the incentive server, let us make them uniform?
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 5a: dismissing a volunteer
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(MyUser,me,dismiss_volunteer(Task,V)) and 
	user(me,MyUser) and 
	task_volunteers(VList) and
	\+ member(V,VList)
thenceforth
	msg(me,MyUser,"Cannot dismiss this person, as they have never volunteered for this task.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 5b: dismissing a volunteer
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(MyUser,me,dismiss_volunteer(Task,V)) and 
	user(me,MyUser) and 
	task_volunteers(VList) and
	member(V,VList) and
	incentive_server_id(IS) and
	task_manger(TM)
thenceforth
	add_chosen_volunteer(V,Task,Task2) and
	msg(me,TM,update_task(Task2)) and
	msg_help_not_needed(Task,Msg)
	msg(me, V, Msg) and
	msg(me,IS,task_updated(Task, "A volunteer was dismissed", "")). % BRUNO, the format does not follow the previous notification to the incentive server, let us make them uniform?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RULE 6: closing a task
%%% In plain English:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
whenever
	msg(MyUser,me,close_task(Task)) and 
	user(me,MyUser) and 
	task_unanswered(Task,NotReplied) and
	task_volunteers(Task,Volunteers) and
	task_chosen_volunteers(Task,ChosenVolunteers)  and
	incentive_server_id(IS) and
	task_manger(TM)
thenceforth
	msg(me,TM,task_closed(Task)) and 						% BRUNO, also informing task manager
	msg_help_not_needed(Task,Msg) and
	append(NotReplied,Volunteers,List) and
	msg(me,List,Msg) and									% BRUNO, shouldn't we seperate between the cases of contacting the chosen volunteers versus the volunteers and potential volunteers?
	msg_task_closed(Task,Msg2) and
	msg(me,ChosenVolunteers,Msg2) and
	msg(me,IS,task_closed(Task, "The task has been closed", "")).% BRUNO, the format does not follow the previous notification to the incentive server, let us make them uniform?

	
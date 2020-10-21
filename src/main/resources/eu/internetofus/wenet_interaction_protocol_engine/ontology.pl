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
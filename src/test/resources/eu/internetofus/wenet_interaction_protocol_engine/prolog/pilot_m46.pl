:- use_module(library(random)).

:- dynamic
	get_app_users_to_ask/1,
	send_expiration_event/0.

get_app_users_to_ask(AppUsers) :- get_app_users_except_me(UsersExceptMe), random_permutation(AppUsers,UsersExceptMe).
send_expiration_event() :- get_task_attribute_value(ExpirationDate,'expirationDate'), get_now(Now), wenet_math(Delay,ExpirationDate-Now), send_event(TimerId,Delay,'notifyQuestionExpirationMessage',json([])), put_task_state_attribute('timerId',TimerId).
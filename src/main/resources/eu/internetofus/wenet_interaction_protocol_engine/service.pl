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

:- dynamic
	wenet_service_url/2,
	wenet_service_get_app/1,
	wenet_service_get_app/2,
	wenet_service_get_app_users/1,
	wenet_service_get_app_users/2,
	wenet_service_post_callback/1,
	wenet_create_callback_message/3,
	wenet_create_callback_message/5.

%!	wenet_service_url(+Url,-Paths)
%
%	Calculate the URL from a path
%
wenet_service_url(Url,Paths) :-
	wenet_service_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.


%!	get_app(+App,-Id)
%
%	Return the app associated to an identifier.
%
%	@param App list with the app information.
%	@param Id string identifier of the app to obtain.
%
wenet_service_get_app(App,Id) :-
	string(Id),
	wenet_service_url(Url,['/app/',Id]),
	wenet_get_json_from_url(Url,App),
	is_dict(App),
	asserta(get_app(App,Id)),
	wenet_log_trace('Loaded application',App)
	.

%!	get_app(+App)
%
%	Return the app associated to the received message.
%
%	@param App list with the app information.
%
wenet_service_get_app(App) :-
	get_received_message(Message),
	wenet_service_get_app(App,Message.appId)
	.


%!	wenet_service_get_app_users(+Users,-Id)
%
%	Return the users of an application.
%
%	@param Users list of string with the user identifiers of the application.
%	@param Id string identifier of the application to obtain.
%
wenet_service_get_app_users(Users,Id) :-
	string(Id),
	wenet_service_url(Url,['/app/',Id,'/users']),
	wenet_get_json_from_url(Url,Users),
	asserta(get_app_users(Users,Id)),
	format(string(Log_Text),'Loaded users of the application ~w',[Id]),
	wenet_log_trace(Log_Text,Users)
	.

%!	wenet_service_get_app_users(+Users)
%
%	Return the users of an application.
%
%	@param Users list of string with the user identifiers of the application.
%
wenet_service_get_app_users(Users) :-
	get_received_message(Message),
	wenet_service_get_app_users(Users,Message.appId)
	.

%!	wenet_service_post_callback(+Callback)
%
%	Post a callback message to the application of the received message.
%
%	@param Callback to post.
%
wenet_service_post_callback(Callback) :-
	get_received_message(Message),
	AppId = Message.appId,
	wenet_service_get_app(App,AppId),
	Url = App.messageCallbackUrl,
	wenet_post_json_to_url(Url,Callback,[])
	.


%!	wenet_create_callback_message(-Callback,+Label,+Attributes)
%
%	Create an callback message.
%
%	@param Callback_message the create callback message.
%	@param Label the label for the callback message.
%	@param Attributes the attributes for the callback message.
%
wenet_create_callback_message(Callback,Label,Attributes) :-
	get_received_message(Message),
	wenet_create_callback_message(Callback,Message.appId,Message.receiver.userId,Label,Attributes)
	.

%!	wenet_create_callback_message(-Callback,+AppId,+ReceiverId,+Label,+Attributes)
%
%	Create an callback message.
%
%	@param Callback_message the create callback message.
%	@param AppId the application identifier for the callback message.
%	@param ReceiverId the identifier of the receiver for the callback message.
%	@param Label the label for the callback message.
%	@param Attributes the attributes for the callback message.
%
wenet_create_callback_message(Callback,AppId,ReceiverId,Label,Attributes) :-
	string(AppId),
	string(ReceiverId),
	string(Label),
	wenet_log_trace("Attributes:",Attributes),
	is_dict(Attributes),
	Callback = callback{appId:AppId,receiverId:ReceiverId,label:Label,attributes:Attributes},
	wenet_log_trace("Callback:",Callback)
	.

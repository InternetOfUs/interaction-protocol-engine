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
	wenet_service_api_url_to/2,
	wenet_service_get_app/2,
	wenet_id_of_app/2,
	wenet_message_callback_url_of_app/2,
	wenet_service_get_app_users/2,
	wenet_service_post_callback/3,
	wenet_new_message/5,
	wenet_app_id_of_message/2,
	wenet_receiver_id_of_message/2,
	wenet_label_of_message/2,
	wenet_attributes_of_message/2
	.


%!	wenet_service_api_url_to(+Url,-Paths)
%
%	Calculate the URL from a path
%
wenet_service_api_url_to(Url,Paths) :-
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
	wenet_service_api_url_to(Url,['/app/',Id]),
	wenet_get_json_from_url(Url,App)
	.

%!	wenet_id_of_app(-Id,+App)
%
%	Obtain the id of a application.
%
%	@param Id of the application.
%	@param App to get the id.
%
wenet_id_of_app(Id,json(App)) :-
	member(appId=Id,App)
	.

%!	wenet_message_callback_url_of_app(-Url,+App)
%
%	Obtain the URL to post the callback messages for the application.
%
%	@param URL to post the callback messages. of the application.
%	@param App to get the id.
%
wenet_message_callback_url_of_app(Url,json(App)) :-
	member(messageCallbackUrl=Url,App)
	.

%!	wenet_service_get_app_users(-Users,+json(App))
%
%	Return the users of an application.
%
%	@param Users list of string with the user identifiers of the application.
%	@param App json with the application to obtain the users.
%
wenet_service_get_app_users(Users,json(App)) :-
	wenet_id_of_app(Id,json(App)),
	wenet_service_get_app_users(Users,Id)
	.

%!	wenet_service_get_app_users(-Users,+Id)
%
%	Return the users of an application.
%
%	@param Users list of string with the user identifiers of the application.
%	@param Id string identifier of the application to obtain.
%
wenet_service_get_app_users(Users,Id) :-
	wenet_service_api_url_to(Url,['/app/',Id,'/users']),
	wenet_get_json_from_url(Url,Users)
	.

%!	wenet_service_post_callback(-Response,+Callback,+json(App))
%
%	Post a callback message to an application.
%
%	@param Response of the post.
%	@param Callback to post.
%	@param App json application to post the message.
%
wenet_service_post_callback(Response,Callback,App) :-
	wenet_message_callback_url_of_app(Url,App),
	wenet_service_post_callback(Response,Callback,Url)
	.

%!	wenet_service_post_callback(+Callback,+Url)
%
%	Post a callback message to an Url.
%
%	@param Response of the post.
%	@param Callback to post.
%	@param Url to post the callback message.
%
wenet_service_post_callback(Response,Callback,Url) :-
	wenet_post_json_to_url(Response,Url,Callback,[])
	.

%!	wenet_new_message(-Callback,+AppId,+ReceiverId,+Label,+Attributes)
%
%	Create an callback message.
%
%	@param Callback_message the create callback message.
%	@param AppId the application identifier for the callback message.
%	@param ReceiverId the identifier of the receiver for the callback message.
%	@param Label the label for the callback message.
%	@param Attributes the attributes for the callback message.
%
wenet_new_message(Callback,AppId,ReceiverId,Label,@(null)) :-
	wenet_new_message(Callback,AppId,ReceiverId,Label,json([]))
	.
wenet_new_message(Callback,AppId,ReceiverId,Label,json(Attributes)) :-
	Callback = json([appId=AppId,receiverId=ReceiverId,label=Label,attributes=json(Attributes)])
	.

%!	wenet_app_id_of_message(-AppId,+Message)
%
%	Obtain the app identifier of a message.
%
%	@param AppId of a message.
%	@param Message to get the app identifier.
%
wenet_app_id_of_message(AppId, json(Message)) :-
	member(appId=AppId,Message)
	.

%!	wenet_receiver_id_of_message(-ReceiverId,+Message)
%
%	Obtain the receiver identifier of a message.
%
%	@param ReceiverId of a message.
%	@param Message to get the receiver identifier.
%
wenet_receiver_id_of_message(ReceiverId, json(Message)) :-
	member(receiverId=ReceiverId,Message)
	.

%!	wenet_label_of_message(-Label,+Message)
%
%	Obtain the label of a message.
%
%	@param Label of the message.
%	@param Message to get the label.
%
wenet_label_of_message(Label, json(Message)) :-
	member(label=Label,Message)
	.

%!	wenet_attributes_of_message(-Attributes,+Message)
%
%	Obtain the attributes of a message.
%
%	@param Attributes of the message.
%	@param Message to get the attributes.
%
wenet_attributes_of_message(Attributes, json(Message)) :-
	member(attributes=Attributes,Message)
	.

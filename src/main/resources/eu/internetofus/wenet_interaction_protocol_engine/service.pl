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

:- discontiguous
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
	wenet_get_json_from_url(App,Url)
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
	wenet_get_json_from_url(Users,Url)
	.

%!	wenet_new_message(-Callback,+AppId,+ReceiverId,+Label,+Attributes)
%
%	Create an callback message.
%
%	@param Callback the create callback message.
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

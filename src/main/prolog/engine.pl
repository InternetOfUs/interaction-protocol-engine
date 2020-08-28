:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).

:- json_object
        textual_message(recipientId:string, title:string, text:string) + [type=textualMessage].

%% read_model (+Path:string, -Model:predicate)
%
% Load a model from a JSON file
%
% @param Path to the file that contains the model
% @param Model the loaded model as predictae

read_model(Path,Model) :- open(Path,read,Stream),json_read_dict(Stream,Model).


%% load_message (-Message: dictionary)
%
% Load a the received JSON message
%
% @param Message that has started the engine

load_message(Message) :- message_file(MessageFilePath),read_model(MessageFilePath,Message).


%% load_environment (-Environment: dictionary)
%
% Load a the received JSON message
%
% @param Environment obtained from the has started the engine

load_environment(Environment) :- environment_file(EnvironmentFilePath),read_model(EnvironmentFilePath,Environment).


%% load_configuration (-Configuration: dictionary)
%
% Load a the engine configuration
%
% @param Message that has started the engine

load_configuration(Configuration) :- configuration_file(ConfigurationFilePath),read_model(ConfigurationFilePath,Configuration).


%% load_data (-Message:predicate, -Environment:predicate, -Configuration:predicate)
%
% Load a the received JSON message
%
% @param Message that has started the engine

load_data(Message,Environment,Configuration) :- load_message(Message),load_environment(Environment),load_configuration(Configuration). 

%% get_app_users (+AppId: string, +Configuration: dictionary, -UserIds: string list)
%
% Obtains the users that are defined in an application.
%
% @param AppId identifier of the aplication
% @param Configuration of the engine
% @param UserIds the identifier of the users defined on the application 

get_app_users(AppId,Configuration,UserIds) :- 
	ServiceApi=Configuration.get(wenetComponents).get(serviceApi),
	atomics_to_string([ServiceApi,'/app',AppId,'/users'],URL),
	setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json')]),
        json_read_dict(In, UserIds),
        close(In)
    ).

store_actions(Actions) :- open('actions.json',write,Stream),json_write(Stream,Actions).

calculate_actions(Environment,Message,Configuration, Actions) :- 
	AppId=Message.get(appId),
	get_app_users(AppId,Configuration,Actions).
 
go :- debugging,load_data(Message,Environment,Configuration),calculate_actions(Environment,Message,Configuration,Actions),store_actions(Actions).
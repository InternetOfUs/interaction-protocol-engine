%
%
%
%

:- use_module(library(http/json)).

%!  get_dict_from_json_file(+FilePath, -JsonDictionary)
%
%	Read a json file and convert into a dictionary.
%
%	@param FilePath string with the path to the JSON file. 
%	@param JsonDictionary dictionary with the data on the JSON file.
%
get_dict_from_json_file(FilePath, JsonDictionary) :-
  open(FilePath, read, Stream), json_read_dict(Stream, JsonDictionary), close(Stream).


%!  msg_to_input(+Input, -Message)
%
%	Convert the read Message to the input required bu the norm engine.
%
%	@param Input the term taht represents the message. 
%	@param Message to convert.
%   @param Profile of the current user.
%
msg_to_input(Input,Message,Profile) :-
	atom_string(Atom,Message.particle),
	Content =.. [Atom,Message.content.task],
	normalize_user_id(Sender,Message.senderId,Profile.id),
	normalize_user_id(Receiver,Message.senderId,Profile.id),
	Input =.. [msg,Sender,Receiver,Content].

%!  normalize_user_id(+Id,-UserId,-Me)
%
%	Convert an user id to an identifier used on the norm engine.
%
%	@param Id teh normalized identifier. 
%	@param UserId identifier of teh user to convert.
%   @param Me the user identifier of me.
%
normalize_user_id(Id,UserId,Me) :- UserId = Me -> atom_string(Id,me); Id is UserId.

trace.

go :-
	configuration_file(ConfFile),
	get_dict_from_json_file(ConfFile,Configuration),
	environment_file(EnvFile),
	get_dict_from_json_file(EnvFile,Environment),
	%message_file(MsgFile),
	%get_dict_from_json_file(MsgFile,Message),
	%norm_interpreter_file(NormInterpreterFile),
	%consult(NormInterpreterFile),
	%msg_to_input(Input,Message,Profile),
	%individual_norms_file(IndividualNormsFile),
	%community_norms_file(CommunityNormsFile),
	%friends_file(FriendsFile),
	%normengine(Input,FriendsFile,IndividualNormsFile,CommunityNormsFile,Output),
	%actions_file(ActionsFile),
	%open(ActionsFile, write, Stream),
	%json_write(Stream,Output),
	%close(Stream)
	!.
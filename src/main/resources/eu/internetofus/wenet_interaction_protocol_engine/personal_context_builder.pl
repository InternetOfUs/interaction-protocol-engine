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
	wenet_personal_context_builder_url_to/2,
	wenet_personal_context_builder_locations/2,
	wenet_user_id_of_location/2,
	wenet_users_of_locations/2,
	wenet_longitude_of_location/2,
	wenet_latitude_of_location/2,
	wenet_personal_context_builder_closest/4,
	wenet_user_id_of_closest/2,
	wenet_distance_of_closest/2,
	wenet_users_of_closest/2,
	wenet_distance_between_locations/3,
	wenet_distance_between_locations/5,
	wenet_filter_locations_by_distance/5
	.


%!	wenet_personal_context_builder_url_to(-Url,+Paths)
%
%	Calculate the URL to interact to the specified path of the personal context builder.
%
wenet_personal_context_builder_url_to(Url,Paths) :-
	wenet_personal_context_builder_api_url(Api),
	atomics_to_string([Api|Paths],Url)
	.

%!	wenet_personal_context_builder_locations(-Locations,+Users)
%
%	Obtain the locations of a set of users.
%
%	@param Locations list of users locations of the users.
%	@param Users list of string identifiers of the users to obtain its locations.
%
wenet_personal_context_builder_locations(Locations,Users) :-
	wenet_personal_context_builder_url_to(Url,['/locations/']),
	wenet_post_json_to_url(json(Data),Url,json([userids=Users])),
	member(locations=Locations,Data)
	.

%!	wenet_user_id_of_location(UserId,Location)
%
%	Get the user identifier of a location.
%
%	@param UserId identifier of the user of the location.
%	@param Location to get the user identifier.
%
wenet_user_id_of_location(UserId,json(Location)) :-
	member(userId=UserId,Location)
	.

%!	wenet_users_of_locations(UserId,Locations)
%
%	Get the user identifier of a locations user.
%
%	@param UserId identifier of the locations user.
%	@param Locations to obtain the user identifier.
%
wenet_users_of_locations([],[]).
wenet_users_of_locations(Users,[Locations|Tail]) :-
	wenet_user_id_of_location(UserId,Locations),
	wenet_users_of_locations(TailUsers,Tail),
	Users = [UserId|TailUsers]
	.

%!	wenet_longitude_of_location(Longitude,Location)
%
%	Get the longitude of a location.
%
%	@param Longitude of the location.
%	@param Location to get the longitude.
%
wenet_longitude_of_location(Longitude,json(Location)) :-
	member(longitude=Longitude,Location)
	.

%!	wenet_latitude_of_location(Latitude,Location)
%
%	Get the latitude of a location.
%
%	@param Latitude of the location.
%	@param Location to get the latitude.
%
wenet_latitude_of_location(Latitude,json(Location)) :-
	member(latitude=Latitude,Location)
	.

%!	wenet_personal_context_builder_closest(-CloseUser,+Longitude,Latitude,NumUsers)
%
%	Obtain the closest users into a location.
%
%	@param ClosestUsers list of closest users to the location.
%	@param Latitude of the location.
%	@param Longitude of the location.
%	@param NumUsers number maximum of users to return.
%
wenet_personal_context_builder_closest(ClosestUsers,Latitude,Longitude,NumUsers) :-
	wenet_personal_context_builder_url_to(Url,['/closest/?latitude=',Latitude,'&longitude=',Longitude,'&nb_user_max=',NumUsers]),
	wenet_get_json_from_url(ClosestUsers,Url)
	.

%!	wenet_user_id_of_closest(UserId,Closest)
%
%	Get the user identifier of a closest user.
%
%	@param UserId identifier of the closest user.
%	@param Closest to obtain the user identifier.
%
wenet_user_id_of_closest(UserId,json(Closest)) :-
	member(userId=UserId,Closest)
	.

%!	wenet_distance_of_closest(Distance,Closest)
%
%	Get the distance of a closest user.
%
%	@param Distance of the closest user.
%	@param Closest to obtain the distance.
%
wenet_distance_of_closest(Distance,json(Closest)) :-
	member(distance=Distance,Closest)
	.

%!	wenet_users_of_closest(UserId,Closest)
%
%	Get the user identifier of a closest user.
%
%	@param UserId identifier of the closest user.
%	@param Closest to obtain the user identifier.
%
wenet_users_of_closest([],[]).
wenet_users_of_closest(Users,[Closest|Tail]) :-
	wenet_user_id_of_closest(UserId,Closest),
	wenet_users_of_closest(TailUsers,Tail),
	Users = [UserId|TailUsers]
	.

%!	wenet_distance_between_locations(-Distance,+Source,+Target)
%
%	Calculate the distance between two locations.
%
%	@param Distance in meters between the locations.
%	@param Source location to calculate the distance.
%	@param Target location to calculate the distance.
%
wenet_distance_between_locations(Distance,Source,Target) :-
	wenet_latitude_of_location(SourceLatitude,Source),
	wenet_longitude_of_location(SourceLongitude,Source),
	wenet_latitude_of_location(TargetLatitude,Target),
	wenet_longitude_of_location(TargetLongitude,Target),
	wenet_distance_between_locations(Distance,SourceLatitude,SourceLongitude,TargetLatitude,TargetLongitude)
	.

%!	wenet_distance_between_locations(-Distance,+SourceLatitude,+SourceLongitude,+TargetLatitude,+TargetLongitude)
%
%	Calculate the distance between two locations.
%
%	@param Distance in meters between the locations.
%	@param SourceLatitude latitude to calculate the distance.
%	@param SourceLongitude longitude to calculate the distance.
%	@param TargetLatitude latitude to calculate the distance.
%	@param TargetLongitude longitude to calculate the distance.
%
wenet_distance_between_locations(Distance,SourceLatitude,SourceLongitude,TargetLatitude,TargetLongitude) :-
	Fi1 is SourceLatitude * pi/180,
	Fi2 is TargetLatitude * pi/180,
	Delta1 is (TargetLatitude-SourceLatitude) * pi/180,
	Delta2 is (TargetLongitude-SourceLongitude) * pi/180,
	A is sin(Delta1/2) * sin(Delta1/2) + cos(Fi1) * cos(Fi2) * sin(Delta2/2) * sin(Delta2/2),
	C is 2 * atan2(sqrt(A),sqrt(1-A)),
	Distance is 6371e3 * C
	.
	
%!	wenet_filter_locations_by_distance(-Filtered,+Source,+Locations,+Min,+Max)
%
%	Calculate the distance between two locations.
%
%	@param Filtered the locations of the users that are on the range.
%	@param Source location to calculate the distance.
%	@param Locations to filter.
%	@param Min minimum distance (inclusive) in meters between the source and any locations. 
%	@param Max maximum distance (inclusive) in meters between the source and any locations.
%
wenet_filter_locations_by_distance([],_,[],_,_).
wenet_filter_locations_by_distance(Filtered,Source,[Target|Tail],Min,Max) :-
	wenet_filter_locations_by_distance(FilteredTail,Source,Tail,Min,Max),
	wenet_distance_between_locations(Distance,Source,Target),
	(
		(
			>=(Distance,Min),
			=<(Distance,Max)
		) -> Filtered = [Target|FilteredTail] ; Filtered = FilteredTail
	)
	.

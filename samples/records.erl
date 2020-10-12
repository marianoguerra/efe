-module(records).

-export([new/0, new/2, get_field/1, get_field_index/0, update_field/1,
         update_no_fields/1]).

-export([new_g/0, new_g/2, get_field_g/1, get_field_index_g/0, update_field_g/1,
         update_no_fields_g/1]).

-record(user, {username = <<"meg">>, age = 25, team}).
-record('Group', {username = <<"meg">>, age = 25, team}).

new() ->
    #user{}.

new(Username, Age) ->
    #user{username = Username, age = Age}.

get_field(Usr = #user{}) ->
    Usr#user.username.

get_field_index() ->
    #user.age.

update_field(Usr = #user{}) ->
    Usr#user{age = Usr#user.age + 1}.

update_no_fields(Usr) ->
    Usr#user{}.

new_g() ->
    #'Group'{}.

new_g(Username, Age) ->
    #'Group'{username = Username, age = Age}.

get_field_g(Usr = #'Group'{}) ->
    Usr#'Group'.username.

get_field_index_g() ->
    #'Group'.age.

update_field_g(Usr = #'Group'{}) ->
    Usr#'Group'{age = Usr#'Group'.age + 1}.

update_no_fields_g(Usr) ->
    Usr#'Group'{}.

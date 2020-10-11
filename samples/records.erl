-module(records).
-export([new/0, new/2, get_field/1, get_field_index/0, update_field/1, update_no_fields/1]).

-record(user, {username = <<"meg">>, age = 25, team}).

new() -> #user{}.
new(Username, Age) -> #user{username = Username, age = Age}.

get_field(Usr = #user{}) ->
    Usr#user.username.

get_field_index() ->
    #user.age.

update_field(Usr = #user{}) ->
    Usr#user{age = Usr#user.age + 1}.

update_no_fields(Usr) ->
    Usr#user{}.

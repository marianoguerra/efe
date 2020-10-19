-module(records).

-export([new/0, new/2, get_field/1, get_field_index/0, update_field/1,
         update_fields/1, update_no_fields/1]).
-export([new_g/0, new_g/2, get_field_g/1, get_field_index_g/0, update_field_g/1,
         update_no_fields_g/1]).
-export([quote_record/0, record_info_fn/0, record_call/0, is_record_guard/2]).

-record(empty, {}).
-record(user, {username = <<"meg">>, age = 25, team = #empty{}}).
-record('Group', {username = <<"meg">>, age = 25, team, 'type-id' = 1}).
-record('A-B-c', {a = quote_record()}).
-record(state, {function}).

quote_record() ->
    R = #'A-B-c'{},
    R#'A-B-c'{a = 1}.

new() ->
    #user{}.

record_info_fn() ->
    {record_info(fields, user), record_info(size, user)}.

new(Username, Age) ->
    #user{username = Username, age = Age}.

get_field(Usr = #user{}) ->
    Usr#user.username.

get_field_index() ->
    #user.age.

update_field(Usr = #user{}) ->
    Usr#user{age = Usr#user.age + 1}.

update_fields(Usr = #user{}) ->
    Usr#user{age =
                 if Usr#user.age < 0 ->
                        1;
                    true ->
                        Usr#user.age + 1
                 end,
             username = <<"bob">>,
             team = "sharks"}.

update_no_fields(Usr) ->
    Usr#user{}.

new_g() ->
    #'Group'{}.

new_g(Username, Age) ->
    #'Group'{username = Username, age = Age, 'type-id' = 2}.

get_field_g(Usr = #'Group'{}) ->
    Usr#'Group'.'type-id'.

get_field_index_g() ->
    #'Group'.'type-id'.

update_field_g(Usr = #'Group'{}) ->
    Usr#'Group'{'type-id' = Usr#'Group'.'type-id' + 1}.

update_no_fields_g(Usr) ->
    Usr#'Group'{}.

record_call() ->
    R =
        #state{function =
                   fun () ->
                           ok
                   end},
    (R#state.function)().

is_record_guard(R, _N) when is_record(R, state) ->
    is_record(R, state);
is_record_guard(_R, _N) ->
    no.

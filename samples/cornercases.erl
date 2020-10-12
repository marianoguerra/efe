-module(cornercases).

-export([varnames/12]).

varnames(When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else) ->
    {When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else}.

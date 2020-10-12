-module(bincomp).
-export([simple/0]).

simple() ->
    [Red || <<Red:2/binary, _Blue:2/binary>> <= <<1, 2, 3, 4, 5, 6, 7, 8>>].

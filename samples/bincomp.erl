-module(bincomp).

-export([simple/0, bc/0, bc1/0, bsg/0]).

simple() ->
    [Red || <<Red:2/binary, _Blue:2/binary>> <= <<1, 2, 3, 4, 5, 6, 7, 8>>].

bc() ->
    [ 1-Bit || <<Bit:1>> <= <<240>> ].

bc1() ->
    [ <<(1-Bit):1>> || <<Bit:1>> <= <<240>> ].

bsg() ->
    << <<(1-Bit):1>> || <<Bit:1>> <= <<240>> >>.

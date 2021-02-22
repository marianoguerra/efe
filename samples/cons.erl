-module(cons).
-export([cons/1]).

cons([]) -> ok;
cons([1]) -> ok;
cons([1, 2]) -> ok;
cons([1 | 2]) -> ok;
cons([[1, 2], 3]) -> ok;
cons([[1, 2] | 3]) -> ok;
cons([[1 | 2] | 3]) -> ok;
cons([0, [1, 2]]) -> ok;
cons([0, [1 | 2]]) -> ok;
% equivalent to [1, 2, 3]
cons([0 | [1, 2]]) -> ok;
cons([[1, [2, [3]]]]) -> ok;
cons([[-1, 0] | [1, 2]]) -> ok.

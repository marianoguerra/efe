-module(ueval).
-export([match/1, match/3]).

match(+1) -> ok;
match(-1) -> ok;
match(1 + 1) -> ok;
match(1 - 1) -> ok;
match(1 * 2) -> ok;
match(1 / 1) -> ok;
match(bnot 	1) -> ok;
match(1 div 	1) -> ok;
match(1 rem 	1) -> ok;
match(1 band 	1) -> ok;
match(1 bor 	1) -> ok;
match(1 bxor 	1) -> ok;
match(1 bsl 	1) -> ok;
match(1 bsr 	1) -> ok;

match(1 + 1 band 	1) -> ok;
match((1 band 	1) + 1) -> ok;

match(_) -> error.

match(_, 1 bsl 32 - 1, _) -> ok;
match(_, {1, 1 bsl 32 - 1}, _) -> match(0, 1 bsl 32 - 1, 2).

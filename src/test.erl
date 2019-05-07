-module(test).

-compile([export_all]).


myfunc(X, Sum) ->
    X + Sum.

myfunc2() ->
lists:foldl(fun(X, Sum) -> myfunc(X, Sum) end, 0, [1,2,3,4,5]).

% lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).
% 15
% > lists:foldl(fun(X, Prod) -> X * Prod end, 1, [1,2,3,4,5]).
% 120
#!/usr/bin/env escript

main([Input]) ->
    {ok, Index} = search(Input, 1),
    io:format("~B~n", [Index]),
    ok.

search([A, B, C, D | _Rest], Index) when
	  A =/= B, A =/= C, A =/= D, B =/= C, B =/= D, C =/= D ->
    % io:format("# ~B: ~p~n", [Index, [A, B, C, D]]),
    {ok, Index + 3};
search([A | Rest], Index) ->
    _ = A,  % unused
    % io:format("# ~B: ~p | ~p~n", [Index, [A], Rest]),
    search(Rest, Index + 1);
search([], _Index) ->
    error.

% vim: sts=4:

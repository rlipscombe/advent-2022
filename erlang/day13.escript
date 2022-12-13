#!/usr/bin/env escript

-mode(compile).

main([Path]) ->
    {ok, Bytes} = file:read_file(Path),
    Pairs = parse_pairs(Bytes),
    io:format("~p~n", [Pairs]),
    ok.

parse_pairs(Bytes) ->
    parse_pairs_2(binary:split(Bytes, <<"\n\n">>, [global]), []).

parse_pairs_2([<<>>], Acc) ->
    lists:reverse(Acc);
parse_pairs_2([P | Rest], Acc) ->
    [A, B] = binary:split(P, <<"\n">>, [global]),
    parse_pairs_2(Rest, [{parse_packet(A), parse_packet(B)} | Acc]).

parse_packet(Packet) when is_binary(Packet) ->
    parse_packet(binary_to_list(<<Packet/binary, ".">>));
parse_packet(Packet) ->
    {ok, Tokens, _} = erl_scan:string(Packet),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Result.

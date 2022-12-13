#!/usr/bin/env escript

-mode(compile).

main([Path]) ->
    {ok, Bytes} = file:read_file(Path),
    Pairs = parse_pairs(Bytes),
    % io:format("~p~n", [Pairs]),
    part1(Pairs),
    part2(Pairs),
    ok.

part1(Pairs) ->
    Sum = lists:foldl(
        fun({I, {A, B}}, Acc) ->
            io:format("~B: Compare ~p vs ~p~n", [I, A, B]),
            Result = compare(A, B),
            io:format("~B: ~p~n", [I, Result]),
            case Result of
                correct -> Acc + I;
                incorrect -> Acc
            end
        end,
        0,
        lists:enumerate(Pairs)
    ),
    io:format("sum: ~B~n", [Sum]),
    ok.

part2(Pairs) ->
    % Can't be bothered to rewrite the parser, so just jam them back together again.
    Packets0 = combine_pairs(Pairs),
    Div2 = [[2]],
    Div6 = [[6]],
    Packets = [Div2, Div6 | Packets0],
    Sorted = lists:sort(
        fun(A, B) ->
            case compare(A, B) of
                correct -> true;
                _ -> false
            end
        end,
        Packets
    ),
    io:format("~p~n", [Sorted]),
    Pos2 = search(Div2, Sorted),
    Pos6 = search(Div6, Sorted),
    io:format("~B * ~B = ~B~n", [Pos2, Pos6, Pos2 * Pos6]),
    ok.

search(Item, List) ->
    search(Item, List, 1).

search(Item, [Item | _List], Index) -> Index;
search(Item, [_ | List], Index) -> search(Item, List, Index + 1).

combine_pairs(Pairs) ->
    combine_pairs(Pairs, []).

combine_pairs([{A, B} | Rest], Acc) ->
    combine_pairs(Rest, [A, B | Acc]);
combine_pairs([], Acc) ->
    % doesn't need reversing; we're going to sort it anyway.
    Acc.

parse_pairs(Bytes) ->
    parse_pairs_2(binary:split(Bytes, <<"\n\n">>, [global]), []).

parse_pairs_2([], Acc) ->
    lists:reverse(Acc);
parse_pairs_2([<<>>], Acc) ->
    lists:reverse(Acc);
parse_pairs_2([P | Rest], Acc) ->
    parse_pairs_3(binary:split(P, <<"\n">>, [global]), Rest, Acc).

parse_pairs_3([A, B], Rest, Acc) ->
    parse_pairs_2(Rest, [{parse_packet(A), parse_packet(B)} | Acc]);
parse_pairs_3([A, B, <<>>], Rest, Acc) ->
    parse_pairs_3([A, B], Rest, Acc).

parse_packet(Packet) when is_binary(Packet) ->
    parse_packet(binary_to_list(<<Packet/binary, ".">>));
parse_packet(Packet) ->
    {ok, Tokens, _} = erl_scan:string(Packet),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Result.

compare(Left, Right) when is_integer(Left), is_integer(Right), Left < Right ->
    % If both values are integers, the lower integer should come first. If the left integer is lower than the right
    % integer, the inputs are in the right order.
    correct;
compare(Left, Right) when is_integer(Left), is_integer(Right), Left > Right ->
    % If the left integer is higher than the right integer, the inputs are not in the right order.
    incorrect;
compare(Left, Right) when is_integer(Left), is_integer(Right), Left =:= Right ->
    % Otherwise, the inputs are the same integer; continue checking the next part of the input.
    continue;
compare(_Left = [L | Ls], _Right = [R | Rs]) ->
    % If both values are lists, compare the first value of each list, then the second value, and so on.
    case compare(L, R) of
        correct -> correct;
        incorrect -> incorrect;
        continue -> compare(Ls, Rs)
    end;
compare(_Left = [], _Right = [_R | _Rs]) ->
    % If the left list runs out of items first, the inputs are in the right order.
    correct;
compare(_Left = [_L | _Ls], _Right = []) ->
    % If the right list runs out of items first, the inputs are not in the right order.
    incorrect;
compare(_Left = [], _Right = []) ->
    % If the lists are the same length and no comparison makes a decision about the order, continue checking the next
    % part of the input.
    continue;
compare(Left, Right) when is_list(Left), is_integer(Right) ->
    % If exactly one value is an integer, convert the integer to a list which contains that integer as its only value,
    % then retry the comparison.
    compare(Left, [Right]);
compare(Left, Right) when is_integer(Left), is_list(Right) ->
    compare([Left], Right).

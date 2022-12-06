#!/usr/bin/env escript

main([Count, Input]) ->
    {ok, Index} = search(list_to_integer(Count), Input),
    io:format("~B~n", [Index]),
    ok.

search(Count, List) ->
    search_1(Count, List, 1).

search_1(Count, List = [_ | Rest], Index) when is_list(List), length(List) >= Count ->
    search_2(Count, lists:split(Count, List), Rest, Index).

search_2(Count, {Packet, _}, Continue, Index) ->
    % io:format("# search ~B ~p ~p ~B~n", [Count, Packet, Continue, Index]),
    case length(lists:uniq(Packet)) of
        C when C =:= Count ->
            {ok, Index + Count - 1};
        _ ->
            search_1(Count, Continue, Index + 1)
    end.

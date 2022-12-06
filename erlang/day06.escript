#!/usr/bin/env escript

main([Count, Input]) ->
    {ok, Index} = search(list_to_integer(Count), Input),
    io:format("~B~n", [Index]),
    ok.

search(Count, List) ->
    search(Count, List, 1).

search(Count, List, Index) when is_list(List), length(List) >= Count ->
    search(Count, lists:split(Count, List), Index);
search(Count, {Packet, Rest}, Index) ->
    % io:format("# search ~B ~p ~p ~B~n", [Count, Packet, Rest, Index]),
    case length(lists:uniq(Packet)) of
	C when C =:= Count ->
	    {ok, Index + Count - 1};
	_ ->
	    search(Count, tl(Packet) ++ Rest, Index + 1)
    end.

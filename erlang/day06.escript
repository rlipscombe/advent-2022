#!/usr/bin/env escript

main([Count, Input]) ->
    {ok, Index} = search(list_to_integer(Count), Input),
    io:format("~B~n", [Index]),
    ok.

% lists:split/2 errors out if the list is too short.
split(Count, List) when length(List) =< Count ->
    {List, []};
split(Count, List) when is_list(List) ->
    lists:split(Count, List).

search(Count, List) ->
    search(Count, List, 1).

search(Count, List, Index) when is_list(List) ->
    search(Count, split(Count, List), Index);
search(_Count, {[], _}, _Index) ->
    error;
search(Count, {Packet, Rest}, Index) ->
    % io:format("# search ~B ~p ~p ~B~n", [Count, Packet, Rest, Index]),
    case length(lists:uniq(Packet)) of
	C when C =:= Count ->
	    {ok, Index + Count - 1};
	_ ->
	    search(Count, tl(Packet) ++ Rest, Index + 1)
    end.

-module(day07).
-export([main/1]).

main([InputFile]) ->
    {ok, _} = application:ensure_all_started(day07),

    {ok, Input0} = file:read_file(InputFile),
    Input = string:split(string:trim(Input0), "\n", all),
    io:format("~p\n", [Input]),

    Root = whereis(day07_sup),
    lists:foldl(
        fun
            (<<"$ cd /">>, _Cwd) ->
                Root;
            (<<"$ cd ..">>, Cwd) ->
                {dictionary, Dict} = erlang:process_info(Cwd, dictionary),
                {_, [Parent | _]} = lists:keyfind('$ancestors', 1, Dict),
                Parent;
            (<<"$ cd ", Dir/binary>>, Cwd) ->
                io:format("~p~n", [supervisor:which_children(Cwd)]),
                [Child] = [Pid || {Id, Pid, _, _} <- supervisor:which_children(Cwd), Id =:= Dir],
                Child;
            (<<"$ ls">>, Cwd) ->
                Cwd;
            (<<"dir ", Dir/binary>>, Cwd) ->
                {ok, _Pid} = supervisor:start_child(Cwd, #{id => Dir, start => {day07_dir_sup, start_link, []}, type => supervisor}),
                Cwd;
            (Bin, Cwd) ->
                [Size, Name] = binary:split(Bin, <<" ">>),
                io:format("~p ~p~n", [Name, Size]),
                {ok, _Pid} = supervisor:start_child(Cwd, #{id => Name, start => {day07_file_worker, start_link, [Size]}, type => worker}),
                Cwd
        end,
        Root,
        Input
    ),

    observer:start(),
    timer:sleep(infinity),
    ok.

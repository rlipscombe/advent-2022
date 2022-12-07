-module(day07_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{id => root, start => {day07_dir_sup, start_link, []}, type => supervisor}],
    {ok, {SupFlags, ChildSpecs}}.

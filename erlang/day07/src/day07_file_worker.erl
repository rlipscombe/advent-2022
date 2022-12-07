-module(day07_file_worker).
-export([start_link/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

start_link(Size) ->
    gen_server:start_link(?MODULE, [Size], []).

init([Size]) ->
    {ok, Size}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

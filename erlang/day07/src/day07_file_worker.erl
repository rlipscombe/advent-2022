-module(day07_file_worker).
-export([start_link/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

start_link(Size) ->
    gen_server:start_link(?MODULE, [Size], []).

init([Size]) ->
    {ok, Size}.

handle_call(du, _From, State) ->
    {reply, State, State};
    handle_call(Req, From, State) ->
    ?LOG_ERROR(#{req => Req, from => From, state => State}),
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

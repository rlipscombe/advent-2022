-module(day07_dir_sup).
-behaviour(gen_server).
-export([start_link/0]).
-export([du/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

du(Pid) ->
    gen_server:call(Pid, du).

init([]) ->
    State = [],
    {ok, State}.

handle_call({start_child, #{id := Id, start := {M, F, A}, type := Type}}, _From, State) ->
    {ok, Pid} = apply(M, F, A),
    {reply, {ok, Pid}, [{Id, Pid, Type, [M]} | State]};
handle_call(which_children, _From, State) ->
    {reply, State, State};
handle_call(du, _From, State) ->
    {reply, [day07_dir_sup:du(Pid) || {_, Pid, _, _} <- State], State};
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

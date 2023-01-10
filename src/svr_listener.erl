%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2023, <nemo>
%%% @doc
%%% @end
%%% Created : 09. 1月 2023 15:50
%%%-------------------------------------------------------------------
-module(svr_listener).

-behaviour(gen_server).

-export([get_sock/0]).
%% callback
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(ACCEPT_NUM, 20).
-define(HOST, "localhost").
-define(PORT, 33334).
-define(LISTEN_OPTS, [
    binary,
    {backlog, 200}
]).
-define(CONNECT_OPTS, [
    binary,
    {packet, raw},
    {active, true},
    {reuseaddr, true},
    {nodelay, false},
    {delay_send, true},
    {keepalive, true},
    {exit_on_close, true}
]).

-record(state, {lsock}).

get_sock() ->
    gen_tcp:connect(localhost, ?PORT, ?CONNECT_OPTS).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    erlang:send_after(3000, self(), start_listen),
    {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(start_listen, State = #state{}) ->
    {ok, LS} = gen_tcp:listen(?PORT, ?LISTEN_OPTS),
    Rsu = link(LS),
    {ok, Port} = inet:port(LS),
    io:format("prot : ~w ~w ~n", [Port, Rsu]),
    [sup_accept:start_child(LS) || _ID <- lists:seq(1, ?ACCEPT_NUM)],
    {noreply, State#state{lsock = LS}};
handle_info(_Info, State = #state{}) ->
    io:format("listener ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

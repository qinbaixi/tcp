%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2023, <nemo>
%%% @doc
%%% @end
%%% Created : 09. 1月 2023 15:47
%%%-------------------------------------------------------------------
-module(svr_tcp).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({accept, Sock}, State = #state{}) ->
    link(Sock),
    {noreply, State#state{sock = Sock}};
handle_info({tcp, Socket, Data}, #state{sock = Socket} = State) ->
    io:format("recv ~w ~w ~n", [Socket, Data]),
    {noreply, State};
handle_info({'DOWN', Ref, port, Socket, Reason}, State) ->
    io:format("recv ~w ~w ~w~n", [Ref, Socket, Reason]),
    {noreply, State};
handle_info({tcp_closed, Socket}, State = #state{}) ->
    io:format("close ~w ~n", [Socket]),
    unlink(Socket),
    {noreply, State#state{sock = undefined}};
handle_info({'EXIT', Socket, Reason}, State = #state{sock = Sock}) ->
    io:format("close ~w Reason ~w, ~w~n", [Socket, Reason, Socket =:= Sock]),
    unlink(Sock),
    {noreply, State#state{sock = undefined}};
handle_info(_Info, State = #state{}) ->
    io:format("recv unspect ~p ~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
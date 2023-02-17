%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2023, <nemo>
%%% @doc
%%% @end
%%% Created : 09. 1月 2023 15:47
%%%-------------------------------------------------------------------
-module(svr_websock).

-behaviour(gen_server).

-include("buff.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock, state = wait, buff = #buff{}}).

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
    {noreply, State#state{sock = Sock, state = done}};
handle_info({tcp, Socket, Data}, #state{sock = Socket, state = done, buff = Buff} = State) ->
    NewBuff = websocket_decode:unpack_data(Data, Buff),
    End = NewBuff#buff.is_end,
    if
        ((NewBuff#buff.opcode =:= 1) orelse (NewBuff#buff.opcode =:= 2)) andalso End ->
            RespData = websocket_decode:pack_data(NewBuff#buff.payload_data),
            io:format("~p ~n", [NewBuff#buff.payload_data]),
            gen_tcp:send(Socket, RespData),
            {noreply, State#state{buff = #buff{}}};
        true ->
            {noreply, State#state{buff = NewBuff}}
    end;
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
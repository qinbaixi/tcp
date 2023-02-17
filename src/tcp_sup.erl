%%%-------------------------------------------------------------------
%% @doc tcp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    Children = [
        #{id => 'sup_tcp',
            start => {'sup_websock', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => ['sup_tcp']},

        #{id => 'svr_listener',
            start => {'svr_listener', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => ['svr_listener']},

        #{id => 'sup_accept',
            start => {'sup_accept', start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => ['sup_accept']}
    ],


    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

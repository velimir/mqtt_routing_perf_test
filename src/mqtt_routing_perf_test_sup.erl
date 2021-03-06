-module(mqtt_routing_perf_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    WorkersSup = #{id => mqtt_routing_perf_test_workers_sup_sup,
                   start => {mqtt_routing_perf_test_workers_sup_sup, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => supervisor,
                   modules => [mqtt_routing_perf_test_workers_sup_sup]},

    Stats = #{id => mqtt_routing_perf_test_stats,
              start => {mqtt_routing_perf_test_stats, start_link, []},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [mqtt_routing_perf_test_stats]},

    Controller = #{id => mqtt_routing_perf_test_ctrl,
                   start => {mqtt_routing_perf_test_ctrl, start_link, []},
                   restart => temporary,
                   shutdown => 5000,
                   type => worker,
                   modules => [mqtt_routing_perf_test_ctrl]},

    Children = [WorkersSup, Stats, Controller],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-module(mqtt_routing_perf_test_pub_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, terminate_child/1]).

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

start_child(Topics) ->
    supervisor:start_child(?SERVER, [Topics]).

terminate_child(Id) ->
    supervisor:terminate_child(?SERVER, Id).


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

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    AChild = #{id => mqtt_routing_perf_test_pub,
               start => {mqtt_routing_perf_test_pub, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [mqtt_routing_perf_test_pub]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-module(mqtt_playground_ctrl).

-behaviour(gen_statem).

-include("mqtt_playground.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([message_received/2]).
-export([start_execution/3, publish/3, receive_msg/3]).

-define(SERVER, ?MODULE).

-record(execution, {msg_count   = 0 :: pos_integer(),
                    topic_count = 0 :: pos_integer(),
                    topic_len   = 0 :: pos_integer()}).

-record(data, {execution :: #execution{},
               execution_plan :: [#execution{}],
               publisher :: pid(),
               receiver :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).


message_received(Server, Message) ->
    gen_statem:cast(Server, Message).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
init([]) ->
    process_flag(trap_exit, true),
    MQTTMaxLen = 2048,% EMQX max len: https://github.com/emqx/emqx/blob/3d45da8e032a523f7e511f7d38a8f62d187411ba/src/emqx_topic.erl#L44
    TopicLengths = takewhile(exp_stream(), lt(MQTTMaxLen)) ++ [MQTTMaxLen],
    ExecutionPlan =
        [#execution{msg_count = 20, topic_count = 100,
                    topic_len = N}
         || N <- TopicLengths, N /= 1],
    Data = #data{execution_plan = ExecutionPlan},
    {ok, start_execution, Data,
     [{next_event, internal, undefined}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (State Name)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec start_execution('enter',
                 OldState :: atom(),
                 Data :: term()) ->
                        gen_statem:state_enter_result('start_execution');
                (gen_statem:event_type(),
                 Msg :: term(),
                 Data :: term()) ->
                        gen_statem:event_handler_result(atom()).
start_execution(internal, _Msg, #data{execution_plan = []}) ->
    init:stop(0),
    {stop, normal};
start_execution(internal, _Msg,
                #data{execution_plan = [Execution | Plan]} = Data0) ->
    #execution{topic_count = TopicCount, topic_len = TopicLen} = Execution,
    Topics = gen_topics(TopicCount, TopicLen),
    {ok, SubPid} = mqtt_playground_sub_sup:start_child(self(), Topics),
    {ok, PubPid} = mqtt_playground_pub_sup:start_child(Topics),
    Data =
        Data0#data{
          execution = Execution, execution_plan = Plan,
          publisher = PubPid, receiver = SubPid},
    {next_state, publish, Data,
     [{next_event, internal, undefined}]}.

publish(internal, _Msg,
        #data{execution = #execution{msg_count = 0},
              publisher = PubPid, receiver = SubPid} = Data) ->
    ok = mqtt_playground_pub_sup:terminate_child(PubPid),
    ok = mqtt_playground_sub_sup:terminate_child(SubPid),
    Data1 = Data#data{execution = undefined,
                      publisher = undefined,
                      receiver = undefined},
    {next_state, start_execution, Data1,
     [{next_event, internal, undefined}]};
publish(internal, _Msg,
        #data{execution = #execution{msg_count = MsgCount} = Exec,
              publisher = PubPid} = Data)
  when MsgCount > 0 ->
    mqtt_playground_pub:publish(PubPid),
    Data1 = Data#data{execution = Exec#execution{msg_count = MsgCount - 1}},
    {next_state, receive_msg, Data1}.

receive_msg(cast,
            #msg{sent_at = SentTime, received_at = ReceiveTime},
            #data{execution =
                      #execution{topic_count = TopicCount,
                                 topic_len = TopicLen}} = Data) ->
    Stat = #stat{time = ReceiveTime - SentTime,
                 topic_count = TopicCount,
                 topic_len = TopicLen},
    mqtt_playground_stats:report(Stat),
    {next_state, publish, Data,
     [{next_event, internal, undefined}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()} |
                         (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gen_topics(TopicCount, TopicLen) ->
    gen_topics(TopicCount, TopicLen, sets:new()).

gen_topics(0 = _TopicCount, _TopicLen, Topics) ->
    array:from_list(sets:to_list(Topics));
gen_topics(TopicCount, TopicLen, Topics) ->
    Topic = topic(TopicLen),
    case sets:is_element(Topic, Topics) of
        true ->
            gen_topics(TopicCount, TopicLen, Topics);
        false ->
            Topics1 = sets:add_element(Topic, Topics),
            gen_topics(TopicCount - 1, TopicLen, Topics1)
    end.

topic(Len) ->
    <<$/, Topic/binary>> =
        << <<$/, ($0 + (B rem ($z - $0)))>>
           || <<B>> <= crypto:strong_rand_bytes(Len)>>,
    Topic.

stream(Fun, Acc) ->
    fun() ->
            {Value, Acc1} = Fun(Acc),
            {Value, stream(Fun, Acc1)}
    end.

exp_stream() ->
    exp_stream(1).

exp_stream(X) ->
    exp_stream(X, 2).

exp_stream(X, Base) ->
    stream(fun exp_stream_impl/1, {X, Base}).

exp_stream_impl({X, Base}) ->
    {X, {X * Base, Base}}.

stream_acc_(Pred) ->
    fun(Item, Acc) ->
            case Pred(Item) of
                true ->
                    {next, [Item | Acc]};
                false ->
                    {stop, Acc}
            end
    end.

lte(Value) ->
    fun(Lh) -> Lh =< Value end.

lt(Value) ->
    fun(Lh) -> Lh < Value end.

takewhile(Stream, Pred) ->
    StreamAcc = stream_acc_(Pred),
    stream_foldl(Stream, StreamAcc, []).

stream_foldl(Stream, Fun, Acc) ->
    {Item, Stream1} = Stream(),
    case Fun(Item, Acc) of
        {next, Acc1} ->
            stream_foldl(Stream1, Fun, Acc1);
        {stop, Acc1} ->
            lists:reverse(Acc1)
    end.

%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(EUNIT).

exp_stream_test_() ->
    [
     ?_assertMatch({1, _}, (exp_stream(1))()),
     ?_assertMatch({2, _},
                   begin
                       Stream = exp_stream(1),
                       {_, Stream1} = Stream(),
                       Stream1()
                   end)
    ].

stream_foldl_test_() ->
    [
     ?_assertEqual(
        [],
        stream_foldl(exp_stream(1),
                     fun(_X, Acc) -> {stop, Acc} end,
                     [])),
     ?_assertEqual(
        [1, 2],
        stream_foldl(exp_stream(1),
                     fun
                         (X, Acc) when X rem 2 == 0 -> {stop, [X | Acc]};
                         (X, Acc) -> {next, [X | Acc]}
                     end,
                     []))
    ].

takewhile_test_() ->
    [
     ?_assertEqual([], takewhile(exp_stream(1), lte(0))),
     ?_assertEqual([1, 2, 4, 8], takewhile(exp_stream(1), lte(10))),
     ?_assertEqual([1, 2, 4, 8, 16, 32, 64, 128, 256, 512],
                   takewhile(exp_stream(1), lt(1024))),
     ?_assertEqual([1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024],
                   takewhile(exp_stream(1), lte(1024)))
    ].


-endif.

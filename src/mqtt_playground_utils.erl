-module(mqtt_playground_utils).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([topic/1, topic_sequence/1, topic_sequence/2, rand_seq/1]).
-export([array_choose/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% TODO: optimise
topic(Len) ->
    <<$/, Topic/binary>> =
        << <<$/, ($0 + (B rem ($z - $0)))>>
           || <<B>> <= crypto:strong_rand_bytes(Len)>>,
    Topic.

topic_sequence(Len) ->
    topic_sequence(Len, 0).

topic_sequence(Len, SeqNum) ->
    topic_sequence(Len, SeqNum, []).

topic_sequence(Len, SeqNum, Acc) ->
    topic_sequence(Len, SeqNum, Acc, {$0, $z}).

topic_sequence(0, _SeqNum, Acc, _CharRange) ->
    Acc;
topic_sequence(Len, SeqNum, Acc, CharRange) ->
    NextChar = next_char(Len - 1, SeqNum, CharRange),
    topic_sequence(Len - 1, SeqNum, [NextChar | Acc], CharRange).

next_char(CharNum, SeqNum, {Start, End}) ->
    RangeLen = End - Start,
    Start + ((SeqNum + CharNum) rem RangeLen).

rand_seq(Len) ->
    rand_seq(Len, $0, $z).

rand_seq(Len, Start, End) ->
    rand_seq(Len, Start, End, []).

rand_seq(0, _Start, _End, Acc) ->
    Acc;
rand_seq(Len, Start, End, Acc) ->
    Item = rand_item(Start, End),
    rand_seq(Len - 1, Start, End, [Item | Acc]).

rand_item(Start, End) ->
    Start + rand:uniform(End - Start).

array_choose(Array) ->
    Size = array:size(Array),
    RandIndex = rand:uniform(Size) - 1,
    array:get(RandIndex, Array).

%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(EUNIT).

topic_sequence_test_() ->
    [
     ?_assertEqual("", topic_sequence(0)),
     ?_assertEqual("0", topic_sequence(1)),
     ?_assertEqual("01", topic_sequence(2)),
     ?_assertEqual(
        "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxy",
        topic_sequence($z - $0)),
     ?_assertEqual(
        "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxy0",
        topic_sequence(($z - $0) + 1)),
     ?_assertEqual("1", topic_sequence(1, 1)),
     ?_assertEqual("12", topic_sequence(2, 1)),
     ?_assertEqual("01", topic_sequence(2, $z - $0))
    ].

next_char_test_() ->
    [
     ?_assertEqual($0, next_char(0, 0, {$0, $z})),
     ?_assertEqual($1, next_char(1, 0, {$0, $z})),
     ?_assertEqual($1, next_char(0, 1, {$0, $z})),
     ?_assertEqual($0, next_char($z - $0, 0, {$0, $z}))
    ].

-define(_test_rs(Seed, Expr), ?_test(begin rand:seed(Seed), Expr end)).

rand_seq_test_() ->
    {setup,
     fun() ->
             {exrop, [59459703597342671|31074721781414535]}
     end,
     fun(Seed) ->
             [?_test_rs(Seed, ?assertEqual("", rand_seq(0))),
              ?_test_rs(Seed, ?assertEqual("dkVAvixAyG", rand_seq(10))),
              ?_test_rs(Seed, ?assertEqual("5B9=n<ADNOdkVAvixAyG", rand_seq(20)))]
     end
    }.

-endif.

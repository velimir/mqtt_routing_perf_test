
-record(msg, {sent_at     :: integer(),
              received_at :: integer()}).

-record(stat, {time        :: integer(),
               topic_count :: integer(),
               topic_len   :: integer()}).

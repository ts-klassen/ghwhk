-module(ghwhk_subscribe).

-export_type([
        payload/0
    ]).

-export([
        start_link/0
      , subscribe/0
      , subscribe/1
      , list/0
      , await/0
      , broadcast/1
    ]).

-type payload() :: map().

-spec start_link() -> {ok, pid()}.
start_link() ->
    pg:start_link(?MODULE).

-spec subscribe() -> ok.
subscribe() ->
    subscribe(self()).

-spec subscribe(pid()) -> ok.
subscribe(Pid) when is_pid(Pid) ->
    case exists(Pid) of
        true  ->
            ok;
        false ->
            pg:join(?MODULE, webhook_subs, Pid)
    end.

-spec await() -> payload().
await() ->
    subscribe(),
    receive
        {?MODULE, Payload} ->
            Payload
    end.

-spec broadcast(payload()) -> ok.
broadcast(Payload) when is_map(Payload) ->
    broadcast_(Payload, list());
broadcast(Payload) ->
    erlang:errror(badarg, [Payload]).

broadcast_(_Payload, []) ->
    ok;
broadcast_(Payload, [Pid|Pids]) ->
    Pid ! {?MODULE, Payload},
    broadcast_(Payload, Pids).

-spec list() -> [pid()].
list() ->
    pg:get_members(?MODULE, webhook_subs).

-spec exists(pid()) -> boolean().
exists(Pid) ->
    Set = sets:from_list(list(), [{version, 2}]),
    sets:is_element(Pid, Set).

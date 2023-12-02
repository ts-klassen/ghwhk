-module(ghwhk_value).

-export([
        map_lookup/2
      , map_upsert/3
    ]).

-export_type([
        maybe/1
      , map_key/0
    ]).

-type maybe(Value) :: {value, Value} | none.

-type map_key() :: [term()].

-spec map_lookup(map_key(), map()) -> maybe(term()).
map_lookup([], Value) ->
    {value, Value};
map_lookup(_, Value) when not is_map(Value) ->
    none;
map_lookup([H|T], Map) ->
    case maps:find(H, Map) of
        {ok, Value} ->
            map_lookup(T, Value);
        error ->
            none
    end.

-spec map_upsert(map_key(), term(), map()) -> map().
map_upsert(Key, Value, Map) ->
    map_upsert_(Key, Value, {value, Map}, [], []).
map_upsert_([], Value, _, [], []) ->
    Value;
map_upsert_([], Value, _, [{value, Map}|Maps], [Key|Keys]) ->
    map_upsert_([], Map#{Key=>Value}, none, Maps, Keys);
map_upsert_([], Value, _, [none|Maps], [Key|Keys]) ->
    map_upsert_([], #{Key=>Value}, none, Maps, Keys);
map_upsert_([H|T], Value, none, Maps, Keys) ->
    map_upsert_(T, Value, none, [none|Maps], [H|Keys]);
map_upsert_([H|T], Value, {value, Map}, Maps, Keys) ->
    Elem = map_lookup([H], Map),
    map_upsert_(T, Value, Elem, [{value, Map}|Maps], [H|Keys]).


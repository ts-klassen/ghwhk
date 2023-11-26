-module(ghwhk).

-export([
        await/0
    ]).

-export_type([
        webhook/0
    ]).

-type webhook() :: map().

-spec await() -> webhook().
await() ->
    ghwhk_subscribe:await().

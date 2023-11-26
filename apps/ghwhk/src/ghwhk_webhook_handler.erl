-module(ghwhk_webhook_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Payload = jsone:decode(Body),
    ghwhk_subscribe:broadcast(Payload),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"ok\n">>, Req1),
    {ok, Req, State}.

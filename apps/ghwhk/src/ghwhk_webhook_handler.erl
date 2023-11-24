-module(ghwhk_webhook_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    HostName = <<"test">>,
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<HostName/binary, "\n">>, Req0),
    {ok, Req, State}.

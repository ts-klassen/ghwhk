%%%-------------------------------------------------------------------
%% @doc ghwhk public API
%% @end
%%%-------------------------------------------------------------------

-module(ghwhk_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = case os:getenv("GITHUB_APP_WEBHOOK_PORT") of
        false ->
            8080;
        P when is_list(P) ->
            list_to_integer(P)
    end,
    WebhookURI = case os:getenv("GITHUB_APP_WEBHOOK_URI") of
        false ->
            "/github_webhook";
        URI when is_list(URI) ->
            URI
    end,
    Dispatch = cowboy_router:compile([
            {'_', [
                {WebhookURI, ghwhk_webhook_handler, []}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(ghwhk_http_listener,
        [{port, Port}, inet, inet6],
        #{
            env => #{dispatch => Dispatch}
          , middlewares => [
                cowboy_router
              , cowboy_handler
            ]
        }
    ),
    ghwhk_subscribe:start_link(),
    ghwhk_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

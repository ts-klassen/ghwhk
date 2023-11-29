-module(ghwhk_auth).

-export([
        jwt/0
      , access_token/1
    ]).

-export_type([
        installation_id/0
      , access_token/0
    ]).

-type installation_id() :: non_neg_integer().
-type access_token() :: unicode:unicode_binary().

-spec jwt() -> string().
jwt() ->
    os:cmd("/opt/ttsquest_test_token").

-spec access_token(installation_id()) -> access_token().
access_token(InstallationId) when is_integer(InstallationId) ->
    access_token(integer_to_list(InstallationId));
access_token(InstallationId) when is_list(InstallationId) ->
    JWT = jwt(),
    Url = "https://api.github.com/app/installations/"
       ++ InstallationId
       ++ "/access_tokens",
    Headers = [
        {"Accept", "application/vnd.github.machine-man-preview+json"}
      , {"User-Agent", "erlang/OTP"}
      , {"Authorization", "Bearer "++JWT}
    ],
    Options = [{body_format, binary}],
    {ok, Res} = httpc:request(post, {Url, Headers, "", ""}, [], Options),
    {{_,201,_}, _, Json} = Res,
    #{<<"token">>:=Token} = jsone:decode(Json),
    Token.

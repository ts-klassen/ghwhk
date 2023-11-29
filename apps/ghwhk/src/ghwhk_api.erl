-module(ghwhk_api).

-export([
        request/2
      , request/3
      , request/4
      , create_issue/2
      , get_issue/2
    ]).

-export_type([
        repos/0
      , method/0
      , payload/0
      , issue_contents/0
      , partial_issue_contents/0
      , issue_number/0
    ]).

-type repos() :: #{
        installation_id := ghwhk_auth:installation_id()
      , owner := unicode:unicode_binary()
      , repository := unicode:unicode_binary()
    }.

-type method() :: head | get | put | patch | post | options | delete.

-type payload() :: map().

-type issue_contents() :: #{
        title := unicode:unicode_binary()
      , body => unicode:unicode_binary()
      , assignee => unicode:unicode_binary() | null
      , milestone => null | unicode:unicode_binary() | integer()
      , labels => [unicode:unicode_binary()]
      , assignees => [unicode:unicode_binary()]
    }.
-type partial_issue_contents() :: #{
        title => unicode:unicode_binary()
      , body => unicode:unicode_binary()
      , assignee => unicode:unicode_binary() | null
      , milestone => null | unicode:unicode_binary() | integer()
      , labels => [unicode:unicode_binary()]
      , assignees => [unicode:unicode_binary()]
    }.

-type issue_number() :: pos_integer().


-spec create_issue(repos(), issue_contents()) -> payload().
create_issue(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, Payload) ->
    Uri = <<"/repos/", Owner/binary, "/", Repo/binary, "/issues">>,
    request(post, Uri, InstallationId, Payload).

-spec get_issue(repos(), issue_number()) -> payload().
get_issue(Repos, IssueNumber) when is_integer(IssueNumber) ->
    get_issue(Repos, integer_to_binary(IssueNumber));
get_issue(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, IssueNumber) when is_binary(IssueNumber) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues/"
      , IssueNumber/binary
    >>,
    request(get, Uri, InstallationId, #{}).

-spec request(unicode:unicode_binary(), ghwhk_auth:installation_id()
    ) -> payload().
request(Url, InstallationId) ->
    request(get, Url, InstallationId, #{}).
-spec request(
        unicode:unicode_binary(), ghwhk_auth:installation_id(), payload()
    ) -> payload().
request(Url, InstallationId, Payload) ->
    request(post, Url, InstallationId, Payload).

-spec request(
        method()
      , unicode:unicode_binary()
      , ghwhk_auth:installation_id()
      , payload()
    ) -> payload().
request(Method, Url, InstallationId, Payload) when is_binary(Url) ->
    request(Method, binary_to_list(Url), InstallationId, Payload);
request(Method, [$/|Uri], InstallationId, Payload) ->
    Body = jsone:encode(Payload),
    Token = ghwhk_auth:access_token(InstallationId),
    Headers = [
        {"Accept", "application/vnd.github.machine-man-preview+json"}
      , {"User-Agent", "erlang/OTP"}
      , {"Authorization", "token "++Token}
    ],
    Url = "https://api.github.com/" ++ Uri,
    Request = case Method of
        get ->
            {Url, Headers};
        _ ->
            {Url, Headers, "application/json", Body}
    end,
    Options = [{body_format, binary}],
    {ok, Res} = httpc:request(Method, Request, [], Options),
    case Res of
        {{_,Status,_}, _, Json} when 200 =< Status, Status =< 299 ->
            jsone:decode(Json);
        Error ->
            error({httpc_error, Error})
    end.

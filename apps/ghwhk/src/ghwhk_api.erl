-module(ghwhk_api).

-export([
        request/2
      , request/3
      , request/4
      , list_issue/1
      , create_issue/2
      , get_issue/2
      , update_issue/3
      , list_comment/1
      , list_comment/2
      , create_comment/3
      , get_comment/2
      , update_comment/3
    ]).

-export_type([
        repos/0
      , method/0
      , payload/0
      , issue_contents/0
      , issue_number/0
    ]).

-type repos() :: #{
        installation_id := ghwhk_auth:installation_id()
      , owner := unicode:unicode_binary()
      , repository := unicode:unicode_binary()
    }.

-type method() :: head | get | put | patch | post | options | delete.

-type payload() :: map() | list().

-type issue_contents() :: #{
        title := unicode:unicode_binary()
      , body => unicode:unicode_binary()
      , assignee => unicode:unicode_binary() | null
      , milestone => null | unicode:unicode_binary() | integer()
      , labels => [unicode:unicode_binary()]
      , assignees => [unicode:unicode_binary()]
    }.

-type issue_number() :: pos_integer().
-type comment_id() :: pos_integer().


-spec list_issue(repos()) -> payload().
list_issue(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues"
    >>,
    request(get, Uri, InstallationId, #{}).

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

-spec update_issue(repos(), issue_number(), issue_contents()) -> payload().
update_issue(Repos, IssueNumber, Payload) when is_integer(IssueNumber) ->
    update_issue(Repos, integer_to_binary(IssueNumber), Payload);
update_issue(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, IssueNumber, Payload) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues/"
      , IssueNumber/binary
    >>,
    request(patch, Uri, InstallationId, Payload).


-spec list_comment(repos()) -> payload().
list_comment(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues/comments"
    >>,
    request(get, Uri, InstallationId, #{}).

-spec list_comment(repos(), issue_number()) -> payload().
list_comment(Repos, IssueNumber) when is_integer(IssueNumber) ->
    list_comment(Repos, integer_to_binary(IssueNumber));
list_comment(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, IssueNumber) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues/comments"
      , IssueNumber/binary
    >>,
    request(get, Uri, InstallationId, #{}).

-spec create_comment(
        repos(), issue_number(), unicode:unicode_binary()
    ) -> payload().
create_comment(Repos, Num, Body) when is_integer(Num) ->
    create_comment(Repos, integer_to_binary(Num), Body);
create_comment(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, IssueNumber, Body) ->
    Uri = <<"/repos/", Owner/binary, "/", Repo/binary, "/issues/",
        IssueNumber/binary, "/comments"
    >>,
    request(post, Uri, InstallationId, #{body=>Body}).

-spec get_comment(repos(), comment_id()) -> payload().
get_comment(Repos, Id) when is_integer(Id) ->
    get_comment(Repos, integer_to_binary(Id));
get_comment(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, CommentId) when is_binary(CommentId) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues/comments/"
      , CommentId/binary
    >>,
    request(get, Uri, InstallationId, #{}).

-spec update_comment(
        repos(), comment_id(), unicode:unicode_binary()
    ) -> payload().
update_comment(Repos, Num, Body) when is_integer(Num) ->
    update_comment(Repos, integer_to_binary(Num), Body);
update_comment(#{
        installation_id := InstallationId
      , owner := Owner
      , repository := Repo
    }, CommentId, Body) ->
    Uri = <<"/repos/"
      , Owner/binary
      , "/"
      , Repo/binary
      , "/issues/comments"
      , CommentId/binary
    >>,
    request(patch, Uri, InstallationId, #{body=>Body}).















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

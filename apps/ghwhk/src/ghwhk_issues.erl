-module(ghwhk_issues).

-export([
        new/1
      , new/3
      , get/1
      , list/1
      , create/1
      , update/1
      , await/1
      , await/2
    ]).

%getters
-export([
        repos/1
      , payload/1
      , contents/1
      , number/1
      , login/1
      , installation_id/1
      , owner/1
      , repository/1
      , title/1
      , body/1
      , assignee/1
      , milestone/1
      , labels/1
      , assignees/1
    ]).

%setters
-export([
        repos/2
      , payload/2
      , contents/2
      , number/2
      , login/2
      , installation_id/2
      , owner/2
      , repository/2
      , title/2
      , body/2
      , assignee/2
      , milestone/2
      , labels/2
      , assignees/2
    ]).

-export_type([
        issue/0
      , action/0
    ]).

-opaque issue() :: #{
        repos => ghwhk_api:repos()
      , payload => ghwhk_api:payload()
      , contents => ghwhk_api:issue_contents()
      , number => ghwhk_api:issue_number()
      , login => unicode:unicode_binary()
    }.

%% https://docs.github.com/ja/webhooks/webhook-events-and-payloads#issues
-type action() ::
        assigned
      | closed
      | deleted
      | demilestoned
      | edited
      | labeled
      | locked
      | milestoned
      | opened
      | pinned
      | reopened
      | transferred
      | unassigned
      | unlabeled
      | unlocked
      | unpinned
      .


%% also works for ghwhk_comments:comment() instead of issue()
-spec new(ghwhk_api:repos() | issue()) -> issue().
new(#{repos:=Repos}) ->
    new(Repos);
new(Repos) ->
    repos(Repos, #{}).
-spec new(
        ghwhk_auth:installation_id()
      , unicode:unicode_binary()
      , unicode:unicode_binary()
    ) -> issue().
new(InstallationId, Owner, Repository) ->
    Issue0 = installation_id(InstallationId, #{}),
    Issue1 = owner(Owner, Issue0),
    repository(Repository, Issue1).

-spec await(issue()) -> issue().
await(Issue0) ->
    {value, Repos} = repos(Issue0),
    Key = <<"issue">>,
    #{Key:=Payload} = ghwhk_subscribe:await(Repos, Key),
    payload(Payload, Issue0).

-spec await(action(), issue()) -> issue().
await(Action, Issue) when is_atom(Action) ->
    await(atom_to_binary(Action), Issue);
await(Action, Issue0) ->
    {value, Repos} = repos(Issue0),
    Key = <<"issue">>,
    #{Key:=Payload} = ghwhk_subscribe:await(Repos, Key, Action),
    payload(Payload, Issue0).

-spec get(issue()) -> issue().
get(#{repos:=Repos, number:=IssueNumber}=Issue) ->
    Payload = ghwhk_api:get_issue(Repos, IssueNumber),
    payload(Payload, Issue).

-spec list(issue()) -> [issue()].
list(#{repos:=Repos}=Issue) ->
    PayloadList = ghwhk_api:list_issue(Repos),
    lists:map(fun(Payload) ->
        payload(Payload, Issue)
    end, PayloadList).

-spec create(issue()) -> issue().
create(#{repos:=Repos, contents:=Contents}=Issue) ->
    Payload = ghwhk_api:create_issue(Repos, Contents),
    payload(Payload, Issue).

-spec update(issue()) -> issue().
update(#{repos:=Repos, contents:=Contents, number:=Number}=Issue) ->
    Payload = ghwhk_api:update_issue(Repos, Number, Contents),
    payload(Payload, Issue).


%% Getters and Setters
-spec repos(issue()) -> klsn:maybe(ghwhk_api:repos()).
repos(Issue) ->
    klsn_map:lookup([repos], Issue).

-spec repos(ghwhk_api:repos(), issue()) -> issue().
repos(Repos, Issue) ->
    klsn_map:upsert([repos], Repos, Issue).

-spec payload(issue()) -> klsn:maybe(ghwhk_api:payload()).
payload(Issue) ->
    klsn_map:lookup([payload], Issue).

-spec payload(ghwhk_api:payload(), issue()) -> issue().
payload(Payload, Issue0) ->
    KeyMap = #{
        [<<"number">>] => [number]
      , [<<"title">>] => [contents, title]
      , [<<"body">>] => [contents, body]
      , [<<"assignee">>] => [contents, assignee]
      , [<<"milestone">>] => [contents, milestone]
      , [<<"labels">>] => [contents, labels]
      , [<<"assignees">>] => [contents, assignees]
      , [<<"user">>, <<"login">>] => [login]
    },
    Issue = payload_(KeyMap, Payload, Issue0),
    klsn_map:upsert([payload], Payload, Issue).

payload_(KeyMap, Payload, Issue0) ->
    maps:fold(fun(LookupKey, UpsertKey, Issue)->
        payload_(LookupKey, UpsertKey, Payload, Issue)
    end, Issue0, KeyMap).
payload_(LookupKey, UpsertKey, Payload, Issue) ->
    case klsn_map:lookup(LookupKey, Payload) of
        none ->
            Issue;
        {value, Value} ->
            klsn_map:upsert(UpsertKey, Value, Issue)
    end.

-spec contents(issue()) -> klsn:maybe(ghwhk_api:issue_contents()).
contents(Issue) ->
    klsn_map:lookup([contents], Issue).

-spec contents(ghwhk_api:issue_contents(), issue()) -> issue().
contents(Contents, Issue) ->
    klsn_map:upsert([contents], Contents, Issue).

-spec number(issue()) -> klsn:maybe(ghwhk_api:issue_number()).
number(Issue) ->
    klsn_map:lookup([number], Issue).

-spec number(ghwhk_api:issue_number(), issue()) -> issue().
number(Number, Issue) ->
    klsn_map:upsert([number], Number, Issue).

-spec login(issue()) -> klsn:maybe(unicode:unicode_binary()).
login(Issue) ->
    klsn_map:lookup([login], Issue).
    
-spec login(unicode:unicode_binary(), issue()) -> issue().
login(Body, Issue) ->
    klsn_map:upsert([login], Body, Issue).

%% Getters and Setters for repos
-spec installation_id(issue()) -> ghwhk_auth:installation_id().
installation_id(Issue) ->
    klsn_map:lookup([repos, installation_id], Issue).

-spec installation_id(ghwhk_auth:installation_id(), issue()
    ) -> issue().
installation_id(InstallationId, Issue) ->
    klsn_map:upsert([repos, installation_id], InstallationId, Issue).

-spec owner(issue()) -> unicode:unicode_binary().
owner(Issue) ->
    klsn_map:lookup([repos, owner], Issue).

-spec owner(unicode:unicode_binary(), issue()
    ) -> issue().
owner(Owner, Issue) ->
    klsn_map:upsert([repos, owner], Owner, Issue).

-spec repository(issue()) -> unicode:unicode_binary().
repository(Issue) ->
    klsn_map:lookup([repos, repository], Issue).

-spec repository(unicode:unicode_binary(), issue()
    ) -> issue().
repository(Repository, Issue) ->
    klsn_map:upsert([repos, repository], Repository, Issue).

%% Getters and Setters for contents
-spec title(issue()) -> klsn:maybe(unicode:unicode_binary()).
title(Issue) ->
    klsn_map:lookup([contents, title], Issue).

-spec title(unicode:unicode_binary(), issue()) -> issue().
title(Title, Issue) ->
    klsn_map:upsert([contents, title], Title, Issue).

-spec body(issue()) -> klsn:maybe(unicode:unicode_binary()).
body(Issue) ->
    klsn_map:lookup([contents, body], Issue).

-spec body(unicode:unicode_binary(), issue()) -> issue().
body(Body, Issue) ->
    klsn_map:upsert([contents, body], Body, Issue).

-spec assignee(issue()) -> klsn:maybe(unicode:unicode_binary() | null).
assignee(Issue) ->
    klsn_map:lookup([contents, assignee], Issue).

-spec assignee(unicode:unicode_binary() | null, issue()) -> issue().
assignee(Assignee, Issue) ->
    klsn_map:upsert([contents, assignee], Assignee, Issue).

-spec milestone(issue()) -> klsn:maybe(null | unicode:unicode_binary() | integer()).
milestone(Issue) ->
    klsn_map:lookup([contents, milestone], Issue).

-spec milestone(null | unicode:unicode_binary() | integer(), issue()) -> issue().
milestone(Milestone, Issue) ->
    klsn_map:upsert([contents, milestone], Milestone, Issue).

-spec labels(issue()) -> klsn:maybe([unicode:unicode_binary()]).
labels(Issue) ->
    klsn_map:lookup([contents, labels], Issue).

-spec labels([unicode:unicode_binary()], issue()) -> issue().
labels(Labels, Issue) ->
    klsn_map:upsert([contents, labels], Labels, Issue).

-spec assignees(issue()) -> klsn:maybe([unicode:unicode_binary()]).
assignees(Issue) ->
    klsn_map:lookup([contents, assignees], Issue).

-spec assignees([unicode:unicode_binary()], issue()) -> issue().
assignees(Assignees, Issue) ->
    klsn_map:upsert([contents, assignees], Assignees, Issue).


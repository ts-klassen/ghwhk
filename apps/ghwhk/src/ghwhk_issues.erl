-module(ghwhk_issues).

-export([
        new/1
      , new/3
      , get/2
    ]).

%getters
-export([
        repos/1
      , payload/1
      , contents/1
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
    ]).

-opaque issue() :: #{
        repos => ghwhk_api:repos()
      , payload => ghwhk_api:payload()
      , contents => ghwhk_api:issue_contents()
    }.

-spec new(ghwhk_api:repos()) -> issue().
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

-spec get(ghwhk_api:issue_number(), issue()) -> issue().
get(IssueNumber, #{repos:=Repos}=Issue) ->
    Contents = ghwhk_api:get_issue(Repos, IssueNumber),
    contents(Contents, Issue).


%% Getters and Setters
-spec repos(issue()) -> ghwhk_value:maybe(ghwhk_api:repos()).
repos(Issue) ->
    ghwhk_value:map_lookup([repos], Issue).

-spec repos(ghwhk_api:repos(), issue()) -> issue().
repos(Repos, Issue) ->
    ghwhk_value:map_upsert([repos], Repos, Issue).

-spec payload(issue()) -> ghwhk_value:maybe(ghwhk_api:payload()).
payload(Issue) ->
    ghwhk_value:map_lookup([payload], Issue).

-spec payload(ghwhk_api:payload(), issue()) -> issue().
payload(Payload, Issue) ->
    ghwhk_value:map_upsert([payload], Payload, Issue).

-spec contents(issue()) -> ghwhk_value:maybe(ghwhk_api:issue_contents()).
contents(Issue) ->
    ghwhk_value:map_lookup([contents], Issue).

-spec contents(ghwhk_api:issue_contents(), issue()) -> issue().
contents(Contents, Issue) ->
    ghwhk_value:map_upsert([contents], Contents, Issue).

%% Getters and Setters for repos
-spec installation_id(issue()) -> ghwhk_auth:installation_id().
installation_id(Issue) ->
    ghwhk_value:map_lookup([repos, installation_id], Issue).

-spec installation_id(ghwhk_auth:installation_id(), issue()
    ) -> issue().
installation_id(InstallationId, Issue) ->
    ghwhk_value:map_upsert([repos, installation_id], InstallationId, Issue).

-spec owner(issue()) -> unicode:unicode_binary().
owner(Issue) ->
    ghwhk_value:map_lookup([repos, owner], Issue).

-spec owner(unicode:unicode_binary(), issue()
    ) -> issue().
owner(Owner, Issue) ->
    ghwhk_value:map_upsert([repos, owner], Owner, Issue).

-spec repository(issue()) -> unicode:unicode_binary().
repository(Issue) ->
    ghwhk_value:map_lookup([repos, repository], Issue).

-spec repository(unicode:unicode_binary(), issue()
    ) -> issue().
repository(Repository, Issue) ->
    ghwhk_value:map_upsert([repos, repository], Repository, Issue).

%% Getters and Setters for contents
-spec title(issue()) -> ghwhk_value:maybe(unicode:unicode_binary()).
title(Issue) ->
    ghwhk_value:map_lookup([contents, title], Issue).

-spec title(unicode:unicode_binary(), issue()) -> issue().
title(Title, Issue) ->
    ghwhk_value:map_upsert([contents, title], Title, Issue).

-spec body(issue()) -> ghwhk_value:maybe(unicode:unicode_binary()).
body(Issue) ->
    ghwhk_value:map_lookup([contents, body], Issue).

-spec body(unicode:unicode_binary(), issue()) -> issue().
body(Body, Issue) ->
    ghwhk_value:map_upsert([contents, body], Body, Issue).

-spec assignee(issue()) -> ghwhk_value:maybe(unicode:unicode_binary() | null).
assignee(Issue) ->
    ghwhk_value:map_lookup([contents, assignee], Issue).

-spec assignee(unicode:unicode_binary() | null, issue()) -> issue().
assignee(Assignee, Issue) ->
    ghwhk_value:map_upsert([contents, assignee], Assignee, Issue).

-spec milestone(issue()) -> ghwhk_value:maybe(null | unicode:unicode_binary() | integer()).
milestone(Issue) ->
    ghwhk_value:map_lookup([contents, milestone], Issue).

-spec milestone(null | unicode:unicode_binary() | integer(), issue()) -> issue().
milestone(Milestone, Issue) ->
    ghwhk_value:map_upsert([contents, milestone], Milestone, Issue).

-spec labels(issue()) -> ghwhk_value:maybe([unicode:unicode_binary()]).
labels(Issue) ->
    ghwhk_value:map_lookup([contents, labels], Issue).

-spec labels([unicode:unicode_binary()], issue()) -> issue().
labels(Labels, Issue) ->
    ghwhk_value:map_upsert([contents, labels], Labels, Issue).

-spec assignees(issue()) -> ghwhk_value:maybe([unicode:unicode_binary()]).
assignees(Issue) ->
    ghwhk_value:map_lookup([contents, assignees], Issue).

-spec assignees([unicode:unicode_binary()], issue()) -> issue().
assignees(Assignees, Issue) ->
    ghwhk_value:map_upsert([contents, assignees], Assignees, Issue).


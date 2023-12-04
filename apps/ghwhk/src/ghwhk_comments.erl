-module(ghwhk_comments).

-export([
        new/1
      , new/3
      , get/1
      , list/1
      , create/1
      , update/1
    ]).

%getters
-export([
        repos/1
      , payload/1
      , number/1
      , installation_id/1
      , owner/1
      , repository/1
      , id/1
      , body/1
    ]).

%setters
-export([
        repos/2
      , payload/2
      , number/2
      , installation_id/2
      , owner/2
      , repository/2
      , id/2
      , body/2
    ]).

-export_type([
        comment/0
    ]).

-opaque comment() :: #{
        repos => ghwhk_api:repos()
      , payload => ghwhk_api:payload()
      , body => unicode:unicode_binary()
      , id => ghwhk_api:comment_number()
      , number => ghwhk_api:issue_number()
    }.

-spec new(ghwhk_api:repos()) -> comment().
new(Repos) ->
    repos(Repos, #{}).
-spec new(
        ghwhk_auth:installation_id()
      , unicode:unicode_binary()
      , unicode:unicode_binary()
    ) -> comment().
new(InstallationId, Owner, Repository) ->
    Comment0 = installation_id(InstallationId, #{}),
    Comment1 = owner(Owner, Comment0),
    repository(Repository, Comment1).

-spec get(comment()) -> comment().
get(#{repos:=Repos, id:=Id}=Comment) ->
    Payload = ghwhk_api:get_comment(Repos, Id),
    payload(Payload, Comment).

-spec list(comment() | ghwhk_issue:issue()) -> [comment()].
list(#{repos:=Repos, number:=Number}=Comment) ->
    PayloadList = ghwhk_api:list_comment(Repos, Number),
    lists:map(fun(Payload) ->
        payload(Payload, Comment)
    end, PayloadList);
list(#{repos:=Repos}=Comment) ->
    PayloadList = ghwhk_api:list_comment(Repos),
    lists:map(fun(Payload) ->
        payload(Payload, Comment)
    end, PayloadList).

-spec create(comment()) -> comment().
create(#{repos:=Repos, number:=Number, body:=Body}=Comment) ->
    Payload = ghwhk_api:create_comment(Repos, Number, Body),
    payload(Payload, Comment).

-spec update(comment()) -> comment().
update(#{repos:=Repos, id:=Id, body:=Body}=Comment) ->
    Payload = ghwhk_api:update_comment(Repos, Id, Body),
    payload(Payload, Comment).


%% Getters and Setters
-spec repos(comment()) -> ghwhk_value:maybe(ghwhk_api:repos()).
repos(Comment) ->
    ghwhk_value:map_lookup([repos], Comment).

-spec repos(ghwhk_api:repos(), comment()) -> comment().
repos(Repos, Comment) ->
    ghwhk_value:map_upsert([repos], Repos, Comment).

-spec payload(comment()) -> ghwhk_value:maybe(ghwhk_api:payload()).
payload(Comment) ->
    ghwhk_value:map_lookup([payload], Comment).

-spec payload(ghwhk_api:payload(), comment()) -> comment().
payload(Payload, Comment) ->
    ghwhk_value:map_upsert([payload], Payload, Comment).

-spec number(comment()) -> ghwhk_value:maybe(ghwhk_api:issue_number()).
number(Comment) ->
    ghwhk_value:map_lookup([number], Comment).

-spec number(ghwhk_api:issue_number(), comment()) -> comment().
number(Number, Comment) ->
    ghwhk_value:map_upsert([number], Number, Comment).

-spec id(comment()) -> ghwhk_value:maybe(ghwhk_api:comment_id()).
id(Comment) ->
    ghwhk_value:map_lookup([id], Comment).

-spec id(ghwhk_api:comment_id(), comment()) -> comment().
id(Id, Comment) ->
    ghwhk_value:map_upsert([id], Id, Comment).

%% Getters and Setters for repos
-spec installation_id(comment()) -> ghwhk_auth:installation_id().
installation_id(Comment) ->
    ghwhk_value:map_lookup([repos, installation_id], Comment).

-spec installation_id(ghwhk_auth:installation_id(), comment()
    ) -> comment().
installation_id(InstallationId, Comment) ->
    ghwhk_value:map_upsert([repos, installation_id], InstallationId, Comment).

-spec owner(comment()) -> unicode:unicode_binary().
owner(Comment) ->
    ghwhk_value:map_lookup([repos, owner], Comment).

-spec owner(unicode:unicode_binary(), comment()
    ) -> comment().
owner(Owner, Comment) ->
    ghwhk_value:map_upsert([repos, owner], Owner, Comment).

-spec repository(comment()) -> unicode:unicode_binary().
repository(Comment) ->
    ghwhk_value:map_lookup([repos, repository], Comment).

-spec repository(unicode:unicode_binary(), comment()
    ) -> comment().
repository(Repository, Comment) ->
    ghwhk_value:map_upsert([repos, repository], Repository, Comment).

%% Getters and Setters for contents
-spec body(comment()) -> ghwhk_value:maybe(unicode:unicode_binary()).
body(Comment) ->
    ghwhk_value:map_lookup([body], Comment).

-spec body(unicode:unicode_binary(), comment()) -> comment().
body(Body, Comment) ->
    ghwhk_value:map_upsert([body], Body, Comment).


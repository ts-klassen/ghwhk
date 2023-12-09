-module(ghwhk_comments).

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
      , number/1
      , installation_id/1
      , owner/1
      , repository/1
      , id/1
      , body/1
      , login/1
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
      , login/2
    ]).

-export_type([
        comment/0
      , action/0
    ]).

-opaque comment() :: #{
        repos => ghwhk_api:repos()
      , payload => ghwhk_api:payload()
      , body => unicode:unicode_binary()
      , id => ghwhk_api:comment_number()
      , number => ghwhk_api:issue_number()
      , login => unicode:unicode_binary()
    }.

%% https://docs.github.com/ja/webhooks/webhook-events-and-payloads#issue_comment
-type action() :: created | deleted | edited.


-spec new(
        ghwhk_api:repos() | comment() | ghwhk_issues:issue()
    ) -> comment().
new(#{repos:=Repos}=Comment0) ->
    Comment = new(Repos),
    case number(Comment0) of
        none ->
            Comment;
        {value, Number} ->
            number(Number, Comment)
    end;
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

-spec await(comment()) -> comment().
await(Comment0) ->
    {value, Repos} = repos(Comment0),
    Key = <<"comment">>,
    #{Key:=Payload} = ghwhk_subscribe:await(Repos, Key),
    payload(Payload, Comment0).

-spec await(action(), comment()) -> comment().
await(Action, Comment) when is_atom(Action) ->
    await(atom_to_binary(Action), Comment);
await(Action, Comment0) ->
    {value, Repos} = repos(Comment0),
    Key = <<"comment">>,
    #{Key:=Payload} = ghwhk_subscribe:await(Repos, Key, Action),
    payload(Payload, Comment0).


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
-spec repos(comment()) -> klsn:maybe(ghwhk_api:repos()).
repos(Comment) ->
    klsn_map:lookup([repos], Comment).

-spec repos(ghwhk_api:repos(), comment()) -> comment().
repos(Repos, Comment) ->
    klsn_map:upsert([repos], Repos, Comment).

-spec payload(comment()) -> klsn:maybe(ghwhk_api:payload()).
payload(Comment) ->
    klsn_map:lookup([payload], Comment).

-spec payload(ghwhk_api:payload(), comment()) -> comment().
payload(Payload, Comment0) ->
    KeyMap = #{
        [<<"issue">>, <<"number">>] => [number]
      , [<<"id">>] => [id]
      , [<<"user">>, <<"login">>] => [login]
      , [<<"body">>] => [body]
    },
    Comment = payload_(KeyMap, Payload, Comment0),
    klsn_map:upsert([payload], Payload, Comment).

payload_(KeyMap, Payload, Comment0) ->
    maps:fold(fun(LookupKey, UpsertKey, Comment)->
        payload_(LookupKey, UpsertKey, Payload, Comment)
    end, Comment0, KeyMap).
payload_(LookupKey, UpsertKey, Payload, Comment) ->
    case klsn_map:lookup(LookupKey, Payload) of
        none ->
            Comment;
        {value, Value} ->
            klsn_map:upsert(UpsertKey, Value, Comment)
    end.


-spec number(comment()) -> klsn:maybe(ghwhk_api:issue_number()).
number(Comment) ->
    klsn_map:lookup([number], Comment).

-spec number(ghwhk_api:issue_number(), comment()) -> comment().
number(Number, Comment) ->
    klsn_map:upsert([number], Number, Comment).

-spec id(comment()) -> klsn:maybe(ghwhk_api:comment_id()).
id(Comment) ->
    klsn_map:lookup([id], Comment).

-spec id(ghwhk_api:comment_id(), comment()) -> comment().
id(Id, Comment) ->
    klsn_map:upsert([id], Id, Comment).

%% Getters and Setters for repos
-spec installation_id(comment()) -> ghwhk_auth:installation_id().
installation_id(Comment) ->
    klsn_map:lookup([repos, installation_id], Comment).

-spec installation_id(ghwhk_auth:installation_id(), comment()
    ) -> comment().
installation_id(InstallationId, Comment) ->
    klsn_map:upsert([repos, installation_id], InstallationId, Comment).

-spec owner(comment()) -> unicode:unicode_binary().
owner(Comment) ->
    klsn_map:lookup([repos, owner], Comment).

-spec owner(unicode:unicode_binary(), comment()
    ) -> comment().
owner(Owner, Comment) ->
    klsn_map:upsert([repos, owner], Owner, Comment).

-spec repository(comment()) -> unicode:unicode_binary().
repository(Comment) ->
    klsn_map:lookup([repos, repository], Comment).

-spec repository(unicode:unicode_binary(), comment()
    ) -> comment().
repository(Repository, Comment) ->
    klsn_map:upsert([repos, repository], Repository, Comment).

-spec login(comment()) -> klsn:maybe(unicode:unicode_binary()).
login(Comment) ->
    klsn_map:lookup([login], Comment).

-spec login(unicode:unicode_binary(), comment()) -> comment().
login(Body, Comment) ->
    klsn_map:upsert([login], Body, Comment).

%% Getters and Setters for contents
-spec body(comment()) -> klsn:maybe(unicode:unicode_binary()).
body(Comment) ->
    klsn_map:lookup([body], Comment).

-spec body(unicode:unicode_binary(), comment()) -> comment().
body(Body, Comment) ->
    klsn_map:upsert([body], Body, Comment).


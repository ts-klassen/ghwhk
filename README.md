ghwhk
=====

An OTP application

Github app webhook for erlang

Build
-----

    $ rebar3 compile

Example
-------
https://github.com/ts-klassen/supdls/issues/18
```
Issue0 = ghwhk_issues:new(44333065, <<"ts-klassen">>, <<"supdls">>),
Issue1 = ghwhk_issues:await(opened, Issue0), % wait for webhook
{value, IssueTitle} = ghwhk_issues:title(Issue1),
{value, IssueUserLogin} = ghwhk_issues:login(Issue1),
CommentBody = <<"Hi ", IssueUserLogin/binary, ". Thank you for creating an issue `", IssueTitle/binary, "`.">>,
Comment0 = ghwhk_comments:new(Issue1),
Comment1 = ghwhk_comments:body(CommentBody, Comment0),
Comment2 = ghwhk_comments:create(Comment1).
```


## Module: ghwhk_issues

This module defines functions for working with GitHub issues.

### Exported Functions

#### Constructors
- **new/1**
  - *Spec:* `new(ghwhk_api:repos()) -> issue()`
  - *Description:* Creates a new issue with the given repositories.

- **new/3**
  - *Spec:* `new(ghwhk_auth:installation_id(), unicode:unicode_binary(), unicode:unicode_binary()) -> issue()`
  - *Description:* Creates a new issue with the given installation ID, owner, and repository.

#### Operations
- **get/1**
  - *Spec:* `get(issue()) -> issue()`
  - *Description:* Retrieves details for a specific issue.

- **list/1**
  - *Spec:* `list(issue()) -> [issue()]`
  - *Description:* Lists all issues for the specified repositories.

- **create/1**
  - *Spec:* `create(issue()) -> issue()`
  - *Description:* Creates a new issue with the specified contents.

- **update/1**
  - *Spec:* `update(issue()) -> issue()`
  - *Description:* Updates the contents of an existing issue.

- **await/1**
  - *Spec:* `await(issue()) -> issue()`
  - *Description:* Awaits changes to the specified issue.

#### Getters
- **repos/1**
  - *Spec:* `repos(issue()) -> klsn:maybe(ghwhk_api:repos())`
  - *Description:* Retrieves the repositories associated with the issue.

- **payload/1**
  - *Spec:* `payload(issue()) -> klsn:maybe(ghwhk_api:payload())`
  - *Description:* Retrieves the payload associated with the issue.

- **contents/1**
  - *Spec:* `contents(issue()) -> klsn:maybe(ghwhk_api:issue_contents())`
  - *Description:* Retrieves the contents associated with the issue.

- **number/1**
  - *Spec:* `number(issue()) -> klsn:maybe(ghwhk_api:issue_number())`
  - *Description:* Retrieves the issue number.

- **login/1**
  - *Spec:* `login(issue()) -> klsn:maybe(unicode:unicode_binary())`
  - *Description:* Retrieves the login associated with the issue.

#### Setters
- **repos/2**
  - *Spec:* `repos(ghwhk_api:repos(), issue()) -> issue()`
  - *Description:* Sets the repositories for the issue.

- **payload/2**
  - *Spec:* `payload(ghwhk_api:payload(), issue()) -> issue()`
  - *Description:* Sets the payload for the issue.

- **contents/2**
  - *Spec:* `contents(ghwhk_api:issue_contents(), issue()) -> issue()`
  - *Description:* Sets the contents for the issue.

- **number/2**
  - *Spec:* `number(ghwhk_api:issue_number(), issue()) -> issue()`
  - *Description:* Sets the issue number.

- **login/2**
  - *Spec:* `login(unicode:unicode_binary(), issue()) -> issue()`
  - *Description:* Sets the login for the issue.

#### Type
- **issue/0**
  - *Description:* Represents a GitHub issue.

#### Getters and Setters for repos
- **installation_id/1**
  - *Spec:* `installation_id(issue()) -> ghwhk_auth:installation_id()`
  - *Description:* Retrieves the installation ID associated with the issue.

- **owner/1**
  - *Spec:* `owner(issue()) -> unicode:unicode_binary()`
  - *Description:* Retrieves the owner associated with the issue.

- **repository/1**
  - *Spec:* `repository(issue()) -> unicode:unicode_binary()`
  - *Description:* Retrieves the repository associated with the issue.

#### Getters and Setters for contents
- **title/1**
  - *Spec:* `title(issue()) -> klsn:maybe(unicode:unicode_binary())`
  - *Description:* Retrieves the title associated with the issue.

- **body/1**
  - *Spec:* `body(issue()) -> klsn:maybe(unicode:unicode_binary())`
  - *Description:* Retrieves the body associated with the issue.

- **assignee/1**
  - *Spec:* `assignee(issue()) -> klsn:maybe(unicode:unicode_binary() | null)`
  - *Description:* Retrieves the assignee associated with the issue.

- **milestone/1**
  - *Spec:* `milestone(issue()) -> klsn:maybe(null | unicode:unicode_binary() | integer())`
  - *Description:* Retrieves the milestone associated with the issue.

- **labels/1**
  - *Spec:* `labels(issue()) -> klsn:maybe([unicode:unicode_binary()])`
  - *Description:* Retrieves the labels associated with the issue.

- **assignees/1**
  - *Spec:* `assignees(issue()) -> klsn:maybe([unicode:unicode_binary()])`
  - *Description:* Retrieves the assignees associated with the issue.


## Module: ghwhk_comments

This module defines functions for working with GitHub comments.

### Exported Functions

#### Constructors
- **new/1**
  - *Spec:* `new(ghwhk_api:repos()) -> comment()`
  - *Description:* Creates a new comment with the given repositories.

- **new/3**
  - *Spec:* `new(ghwhk_auth:installation_id(), unicode:unicode_binary(), unicode:unicode_binary()) -> comment()`
  - *Description:* Creates a new comment with the given installation ID, owner, and repository.

#### Operations
- **get/1**
  - *Spec:* `get(comment()) -> comment()`
  - *Description:* Retrieves details for a specific comment.

- **list/1**
  - *Spec:* `list(comment() | ghwhk_issue:issue()) -> [comment()]`
  - *Description:* Lists comments for the specified comment or issue.

- **create/1**
  - *Spec:* `create(comment()) -> comment()`
  - *Description:* Creates a new comment with the specified contents.

- **update/1**
  - *Spec:* `update(comment()) -> comment()`
  - *Description:* Updates the contents of an existing comment.

- **await/1**
  - *Spec:* `await(comment()) -> comment()`
  - *Description:* Awaits changes to the specified comment.

#### Getters
- **repos/1**
  - *Spec:* `repos(comment()) -> klsn:maybe(ghwhk_api:repos())`
  - *Description:* Retrieves the repositories associated with the comment.

- **payload/1**
  - *Spec:* `payload(comment()) -> klsn:maybe(ghwhk_api:payload())`
  - *Description:* Retrieves the payload associated with the comment.

- **number/1**
  - *Spec:* `number(comment()) -> klsn:maybe(ghwhk_api:issue_number())`
  - *Description:* Retrieves the issue number associated with the comment.

- **installation_id/1**
  - *Spec:* `installation_id(comment()) -> ghwhk_auth:installation_id()`
  - *Description:* Retrieves the installation ID associated with the comment.

- **owner/1**
  - *Spec:* `owner(comment()) -> unicode:unicode_binary()`
  - *Description:* Retrieves the owner associated with the comment.

- **repository/1**
  - *Spec:* `repository(comment()) -> unicode:unicode_binary()`
  - *Description:* Retrieves the repository associated with the comment.

- **id/1**
  - *Spec:* `id(comment()) -> klsn:maybe(ghwhk_api:comment_id())`
  - *Description:* Retrieves the comment ID.

- **body/1**
  - *Spec:* `body(comment()) -> klsn:maybe(unicode:unicode_binary())`
  - *Description:* Retrieves the body associated with the comment.

- **login/1**
  - *Spec:* `login(comment()) -> klsn:maybe(unicode:unicode_binary())`
  - *Description:* Retrieves the login associated with the comment.

#### Setters
- **repos/2**
  - *Spec:* `repos(ghwhk_api:repos(), comment()) -> comment()`
  - *Description:* Sets the repositories for the comment.

- **payload/2**
  - *Spec:* `payload(ghwhk_api:payload(), comment()) -> comment()`
  - *Description:* Sets the payload for the comment.

- **number/2**
  - *Spec:* `number(ghwhk_api:issue_number(), comment()) -> comment()`
  - *Description:* Sets the issue number for the comment.

- **installation_id/2**
  - *Spec:* `installation_id(ghwhk_auth:installation_id(), comment()) -> comment()`
  - *Description:* Sets the installation ID for the comment.

- **owner/2**
  - *Spec:* `owner(unicode:unicode_binary(), comment()) -> comment()`
  - *Description:* Sets the owner for the comment.

- **repository/2**
  - *Spec:* `repository(unicode:unicode_binary(), comment()) -> comment()`
  - *Description:* Sets the repository for the comment.

- **id/2**
  - *Spec:* `id(ghwhk_api:comment_id(), comment()) -> comment()`
  - *Description:* Sets the comment ID.

- **body/2**
  - *Spec:* `body(unicode:unicode_binary(), comment()) -> comment()`
  - *Description:* Sets the body for the comment.

- **login/2**
  - *Spec:* `login(unicode:unicode_binary(), comment()) -> comment()`
  - *Description:* Sets the login for the comment.

#### Type
- **comment/0**
  - *Description:* Represents a GitHub comment.

klsn
----
`klsn:maybe/1`
```
-type maybe(Value) :: {value, Value} | none.
```

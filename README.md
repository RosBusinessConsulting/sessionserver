Sessionserver
=============

**Server for handling user sessions.**

Author: Sergey Sobko <ssobko@rbc.ru>

The code is mainly written as an exercise in Erlang during development of to be deployed Python version for 
[RosBusinessConsulting](http://rbcholding.com/).

## Introduction

Sessionserver protocol is specific TCP protocol for handling user sessions. It was originally developed and implemented
long time ago using Perl.

## Protocol

Client input is marked ">", server output as "<". Every command except VERSION closes connection after output.
Every param may consist of alphanumeric and special characters.

```
> VERSION <client_version>

> CREATE_SESSION <good_login> <good_password> 
< OK All ok
< password=<good_password_hash>
< session_id=<new_session_id>
< groups=<group1> <group2> ... <groupN>
< login=<good_login>

> CREATE_SESSION <bad_login> <any_password> 
< ERROR No user with login '<bad_login>'

> CREATE_SESSION <good_login> <bad_password> 
< ERROR Invalid password

> CHECK_SESSION <new_session_id>
< OK All ok
< password=<good_password_hash>
< session_id=<new_session_id>
< groups=<group1> <group2> ... <groupN>
< login=<good_login>

> CHECK_SESSION <bad_session>
< ERROR No such session

> DELETE_SESSION <new_session_id>
< OK All ok
< session_id=<new_session_id>
< login=<good_login>

> DELETE_SESSION <bad_session>
< ERROR No such session
```

## Getting the code

The code is hosted at [GitHub](https://github.com/RosBusinessConsulting/sessionserver/).

Check out the latest development version anonymously with:

```
 $ git clone git://github.com/RosBusinessConsulting/sessionserver.git
 $ cd sessionserver
```

## Building

From source:

Install the dependencies:

- [Rebar](https://github.com/basho/rebar/) (may be installed using your package manager)
- [Ranch](https://github.com/ninenines/ranch/)

Getting dependencies (after Rebar is installed):

    $ rebar get-deps
    
Compilation:

    $ rebar compile
    
## Usage

To run development environment
```
$ cp default.config.example default.config
$ erl -pa ebin deps/*/ebin -eval "application:start(sessionserver)" -config default
```

Test user may be added to Mnesia backend this way:

    1> sessionserver_db:update_user(test_user, test_password, [group1, group2], null).

Connect to TCP server may be done this way:

    $ nc localhost 8080
    
Check application is running properly:

    CHECK_SESSION non_existent_session
    
 
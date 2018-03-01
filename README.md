# elli_basicauth

*Basic authentication middleware for [elli][]*

[![Hex.pm][hex badge]][hex package]
[![Erlang][erlang badge]][erlang downloads]
[![Travis CI][travis badge]][travis builds]
[![Coverage Status][coveralls badge]][coveralls link]

[elli]: https://github.com/elli-lib/elli
[hex badge]: https://img.shields.io/hexpm/v/elli_basicauth.svg
[hex package]: https://hex.pm/packages/elli_basicauth
[erlang badge]: https://img.shields.io/badge/erlang-%E2%89%A518.0-red.svg
[erlang downloads]: http://www.erlang.org/downloads
[travis builds]: https://travis-ci.org/elli-lib/elli_basicauth
[travis badge]: https://travis-ci.org/elli-lib/elli_basicauth.svg
[coveralls badge]: https://coveralls.io/repos/github/elli-lib/elli_basicauth/badge.svg?branch=develop
[coveralls link]: https://coveralls.io/github/elli-lib/elli_basicauth?branch=develop
[license badge]: https://img.shields.io/hexpm/l/elli_basicauth.svg


## Installation

```erlang
{deps, [
  {elli, "2.0.2"},
  {elli_basicauth, "0.1.0"}
]}.
```


Use it together with the [Elli webserver](https://github.com/knutin/elli)
like this:

## Example

```erlang
-module(my_elli_stuff).

-export([start_link/0, auth_fun/3]).


start_link() ->
    BasicauthConfig = [
                       {auth_fun, fun my_elli_stuff:auth_fun/3},
                       {auth_realm, <<"Admin Area">>} % optional
                      ],

    Config = [
              {mods, [
                      {elli_basicauth, BasicauthConfig},
                      {elli_example_callback, []}
                     ]}
             ],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}]).


auth_fun(Req, User, Password) ->
    case elli_request:path(Req) of
        [<<"protected">>] -> password_check(User, Password);
        _                 -> ok
    end.


password_check(User, Password) ->
    case {User, Password} of
        {undefined, undefined}      -> unauthorized;
        {<<"admin">>, <<"secret">>} -> ok;
        {User, Password}            -> forbidden
    end.
```

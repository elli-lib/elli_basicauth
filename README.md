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


## Example

- Start an Erlang shell with elli and elli_basicauth loaded.

    ```fish
    rebar3 as test shell
    ```

- Start [elli_basicauth_example](./test/elli_basicauth_example.erl).

    ```erlang
    1> {ok, Pid} = elli_basicauth_example:start_link().
    ```

- Make requests, e.g. using [HTTPie](https://httpie.org/).
    ```fish
    http :8080/protected
    ```
    ```http
    HTTP/1.1 401 Unauthorized
    Connection: Keep-Alive
    Content-Length: 12
    WWW-Authenticate: Basic realm="Admin Area"

    Unauthorized
    ```

    ```fish
    http -a user:pass :8080/protected
    ```
    ```http
    HTTP/1.1 403 Forbidden
    Connection: Keep-Alive
    Content-Length: 9

    Forbidden
    ```

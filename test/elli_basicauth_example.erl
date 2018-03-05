-module(elli_basicauth_example).


-export([start_link/0, auth_fun/2]).


start_link() ->
    Config = [
              {auth_fun, fun ?MODULE:auth_fun/2},
              {auth_realm, <<"Admin Area">>}
             ],
    CallbackArgs = [
                    {mods, [
                            {elli_basicauth, Config},
                            {elli_example_callback, []}
                           ]}
                   ],
    elli:start_link([{callback, elli_middleware},
                     {callback_args, CallbackArgs}]).


-spec auth_fun(Req, Credentials) -> AuthStatus when
      Req          :: elli:req(),
      Credentials  :: elli_basicauth:credentials(),
      AuthStatus   :: elli_basicauth:auth_status().
auth_fun(Req, Credentials) ->
    do_auth_fun(elli_request:path(Req), Credentials).


do_auth_fun([<<"protected">>], Credentials) -> password_check(Credentials);
do_auth_fun(_Path, _Credentials)            -> ok.


-spec password_check(Credentials) -> AuthStatus when
      Credentials :: elli_basicauth:credentials(),
      AuthStatus  :: elli_basicauth:auth_status().
password_check({undefined, undefined})      -> unauthorized;
password_check({<<"admin">>, <<"secret">>}) -> ok;
password_check(_Credentials)                -> forbidden.

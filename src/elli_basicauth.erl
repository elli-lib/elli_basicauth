%% @doc Elli basicauth middleware
%% @author Martin Rehfeld
%% @author Eric Bailey
%% @copyright 2013, Martin Rehfeld; 2018, elli-lib team
%%
%% This middleware provides basic authentication to protect
%% requests, based on a user-configured authentication function.
-module(elli_basicauth).

-behaviour(elli_handler).

-export([handle/2, handle_event/3, default_auth_fun/2]).

-export_type([auth_fun/0, auth_status/0, config/0, credentials/0]).


%% @type auth_fun(). A user-configurable authentication function.
-type auth_fun() :: fun((Req         :: elli:req(),
                         Credentials :: credentials()) ->
                               AuthStatus :: auth_status()).


-type credentials() :: {undefined, undefined} |
                       {Username :: binary(),
                        Password :: binary()}.


%% @type auth_status(). The result of an {@type auth_fun()}.
-type auth_status() :: ok |
                       unauthorized |
                       forbidden |
                       hidden.


%% @type config(). A property list of options.
%% The configurable options are:
%% <dl>
%%   <dt>`auth_fun'</dt>
%%   <dd>An {@type auth_fun()}</dd>
%%   <dt>`auth_realm'</dt>
%%   <dd>A binary <a href="https://tools.ietf.org/html/rfc1945#section-11">realm</a>.</dd>
%% </dl>
-type config() :: [{auth_fun, auth_fun()} |
                   {auth_realm, binary()} |
                   term()].


-define(DEFAULT_CREDENTIALS, {undefined, undefined}).


-define(DEFAULT_REALM, <<"Secure Area">>).


%% @doc Protect `Req' based on the configured `auth_fun'.
%% If none is given, the default authentication is `forbidden'.
-spec handle(elli:req(), config()) -> elli_handler:result().
handle(Req, Config) ->
    Credentials = credentials(Req),
    Authentication = apply(auth_fun(Config), [Req, Credentials]),
    do_handle(Authentication, Config).


-spec do_handle(auth_status(), config()) -> elli_handler:result().
do_handle(ok, _Config) ->
    ignore;
do_handle(unauthorized, Config) ->
    Headers = [{<<"WWW-Authenticate">>, auth_realm(Config)}],
    {401, Headers, <<"Unauthorized">>};
do_handle(forbidden, _Config) ->
    {403, [], <<"Forbidden">>};
do_handle(hidden, _Config) ->
    {404, [], <<>>}.


%% @doc No-op to satisfy the `elli_handler' behaviour. Return `ok'.
-spec handle_event(elli_handler:event(), list(), config()) -> ok.
handle_event(_Event, _Args, _Config) ->
    ok.


%% @doc Default to `forbidden', in case of missing `auth_fun' config.
-spec default_auth_fun(Req, Credentials) -> AuthStatus when
      Req         :: elli:req(),
      Credentials :: credentials(),
      AuthStatus  :: auth_status().
default_auth_fun(_Req, {_User, _Password}) ->
    forbidden.


%%
%% INTERNAL HELPERS
%%

-spec auth_fun(config()) -> auth_fun().
auth_fun(Config) ->
    proplists:get_value(auth_fun, Config, fun default_auth_fun/2).


-spec auth_realm(config()) -> binary().
auth_realm(Config) ->
    Realm = proplists:get_value(auth_realm, Config, ?DEFAULT_REALM),
    iolist_to_binary([<<"Basic realm=\"">>, Realm, <<"\"">>]).


-spec credentials(elli:req()) -> credentials().
credentials(Req) ->
    credentials_from_header(authorization_header(Req)).


-spec authorization_header(elli:req()) -> undefined | binary().
authorization_header(Req) ->
    elli_request:get_header(<<"Authorization">>, Req).


-spec credentials_from_header(undefined | binary()) -> credentials().
credentials_from_header(<<"Basic ", EncodedCredentials/binary>>) ->
    decoded_credentials(EncodedCredentials);
credentials_from_header(_Authorization) ->
    ?DEFAULT_CREDENTIALS.


-spec decoded_credentials(binary()) -> credentials().
decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    do_decoded_credentials(binary:split(DecodedCredentials, <<$:>>)).


-spec do_decoded_credentials([binary()]) -> credentials().
do_decoded_credentials([User, Password]) ->
    {User, Password};
do_decoded_credentials(_Bins) ->
    ?DEFAULT_CREDENTIALS.

%% @doc Elli basicauth middleware
%% @author Martin Rehfeld
%% @author Eric Bailey
%% @copyright 2013, Martin Rehfeld; 2018, elli-lib team
%%
%% This middleware provides basic authentication to protect
%% requests, based on a user-configured authentication function.
-module(elli_basicauth).

-behaviour(elli_handler).

-export([handle/2, handle_event/3]).

-export_type([auth_fun/0, auth_status/0, config/0]).


%% @type auth_fun(). A user-configurable authentication function.
-type auth_fun() :: fun((Req      :: elli:req(),
                         Username :: binary(),
                         Password :: binary()) ->
                               AuthStatus :: auth_status()).


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


%% @doc Protect `Req' based on the configured `auth_fun'.
%% If none is given, the default authentication is `forbidden'.
-spec handle(elli:req(), config()) -> elli_handler:result().
handle(Req, Config) ->
    {User, Password} = credentials(Req),

    case apply(auth_fun(Config), [Req, User, Password]) of
        unauthorized ->
            throw({401,
                   [{<<"WWW-Authenticate">>, auth_realm(Config)}],
                   <<"Unauthorized">>});

        forbidden ->
            throw({403, [], <<"Forbidden">>});

        hidden ->
            throw({404, [], <<>>});

        _ ->
            ignore
    end.
%% @doc No-op to satisfy the `elli_handler' behaviour. Return `ok'.
-spec handle_event(elli_handler:event(), list(), config()) -> ok.
handle_event(_Event, _Args, _Config) ->
    ok.




%%
%% INTERNAL HELPERS
%%

-type credentials() :: {undefined, undefined} |
                       {Username :: binary(),
                        Password :: binary()}.


auth_fun(Config) ->
    proplists:get_value(auth_fun, Config,
        %% default to forbidden in case of missing auth_fun config
        fun (_Req, _User, _Password) ->
            forbidden
        end).


auth_realm(Config) ->
    Realm = proplists:get_value(auth_realm, Config, <<"Secure Area">>),
    iolist_to_binary([<<"Basic realm=\"">>, Realm, <<"\"">>]).


credentials(Req) ->
    case authorization_header(Req) of
        undefined ->
            {undefined, undefined};

        AuthorizationHeader ->
            credentials_from_header(AuthorizationHeader)
    end.


authorization_header(Req) ->
    elli_request:get_header(<<"Authorization">>, Req).


credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$ >>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);

        _ ->
            {undefined, undefined}
    end.


decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [User, Password] ->
            {User, Password};

        _ ->
            {undefined, undefined}
    end.

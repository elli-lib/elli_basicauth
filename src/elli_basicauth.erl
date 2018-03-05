%% @doc Elli basicauth middleware
%%
%% This middleware provides basic authentication to protect
%% Reqs based on a user-configured authentication function
%% @author Martin Rehfeld

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


handle_event(_, _, _) ->
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

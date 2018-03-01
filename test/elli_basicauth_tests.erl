-module(elli_basicauth_tests).
-include_lib("eunit/include/eunit.hrl").


-define(USER, <<"Aladdin">>).
-define(PASSWORD, <<"open sesame">>).
-define(VALID_CREDENTIALS, <<"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==">>).
-define(INVALID_CREDENTIALS, <<"Basic cGxhaW46d3Jvbmc=">>).
-define(COMPLEX_CREDENTIALS, <<"Complex TWFsZm9ybWVkQ3JlZGVudGlhbHM=">>).
-define(MALFORMED_CREDENTIALS, <<"Basic TWFsZm9ybWVkQ3JlZGVudGlhbHM=">>).


-define(FORBIDDEN, {403, [], <<"Forbidden">>}).
-define(NOT_FOUND, {404, [], <<>>}).


%%
%% TESTS
%%

elli_basicauth_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [?_test(no_auth([], ?FORBIDDEN)),
      ?_test(no_auth(unauthorized(<<"Secure Area">>))),
      ?_test(no_auth(basicauth_config_with_custom_realm(), unauthorized(<<"Members only">>))),
      ?_test(with_auth(?VALID_CREDENTIALS, ignore)),
      ?_test(with_auth(?INVALID_CREDENTIALS, hidden_basicauth_config(), ?NOT_FOUND)),
      ?_test(with_auth(?INVALID_CREDENTIALS, ?FORBIDDEN)),
      ?_test(with_auth(?COMPLEX_CREDENTIALS, unauthorized(<<"Secure Area">>))),
      ?_test(with_auth(?MALFORMED_CREDENTIALS, unauthorized(<<"Secure Area">>)))]}.


setup() ->
    meck:new(elli_request),
    elli_request.


teardown(Mod) ->
    ?assert(meck:validate(Mod)),
    meck:unload(Mod).


elli_handler_behaviour_test() ->
    ?assertMatch(ok, elli_basicauth:handle_event(request_complete,
                                                 [mock_request,
                                                  mock_response_code,
                                                  mock_response_headers,
                                                  mock_response_body,
                                                  mock_timings], mock_config)),

    ?assertMatch(ok, elli_basicauth:handle_event(request_throw,
                                                 mock_dummy, mock_config)),

    ?assertMatch(ok, elli_basicauth:handle_event(request_exit,
                                                 mock_dummy, mock_config)),

    ?assertMatch(ok, elli_basicauth:handle_event(request_error,
                                                 mock_dummy, mock_config)),

    ?assertMatch(ok, elli_basicauth:handle_event(request_parse_error,
                                                 [mock_data], mock_args)),

    ?assertMatch(ok, elli_basicauth:handle_event(client_closed,
                                                 [mock_when], mock_config)),

    ?assertMatch(ok, elli_basicauth:handle_event(client_timeout,
                                                 [mock_when], mock_config)),

    ?assertMatch(ok, elli_basicauth:handle_event(elli_startup,
                                                 [], mock_config)).


%%
%% HELPERS
%%

basicauth_config() ->
    [{auth_fun, fun auth_fun/3}].


hidden_basicauth_config() ->
    [{auth_fun, fun hidden_auth_fun/3}].


basicauth_config_with_custom_realm() ->
    [{auth_fun, fun auth_fun/3},
     {auth_realm, <<"Members only">>}].


auth_fun(_Req, undefined, undefined) -> unauthorized;
auth_fun(_Req, ?USER, ?PASSWORD) -> ok;
auth_fun(_Req, _User, _Password) -> forbidden.


hidden_auth_fun(_Req, undefined, undefined) -> hidden;
hidden_auth_fun(_Req, ?USER, ?PASSWORD) -> ok;
hidden_auth_fun(_Req, _User, _Password) -> hidden.


no_auth(Expected) ->
    no_auth(basicauth_config(), Expected).


no_auth(Config, Expected) ->
    with_auth(undefined, Config, Expected).


with_auth(Authorization, Expected) ->
    with_auth(Authorization, basicauth_config(), Expected).


with_auth(Authorization, Config, Expected) ->
    meck:expect(elli_request, get_header,
                fun (<<"Authorization">>, mock_request) ->
                        Authorization
                end),
    ?assertEqual(Expected, (catch elli_basicauth:handle(mock_request, Config))).


unauthorized() ->
    unauthorized(<<"Secure Area">>).

unauthorized(Realm) ->
    {401,
     [{<<"WWW-Authenticate">>,
       <<"Basic realm=\"", Realm/binary, "\"">>}],
     <<"Unauthorized">>}.

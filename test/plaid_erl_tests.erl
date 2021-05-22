
-module(plaid_erl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("plaid_erl.hrl").

%% Test functions

request_test() ->
  {
    setup,
    fun connect/0,
    fun close/1,
    fun request_test_run_/1
  }.

request_async_test() ->
  {
    setup,
    fun connect/0,
    fun close/1,
    fun request_async_test_run_/1
  }.

request_test_run_(Pid) ->
  meck:expect(httpc, request, fun (Method, {Url, _RHeaders, CType, RBody}, _HTTPOpts, _Opts) ->
    ?assertEqual(Method, post),
    ?assertEqual(Url, "https://sandbox.plaid.com/item/get"),
    ?assertEqual(CType, "application/json"),
    ?assertEqual(RBody, jiffy:encode(#{ <<"access_token">> => <<"token">> })),
    ReqId = make_ref(),
    Status = {"HTTP/1.1", 200, "OK"},
    Headers = [{"content-type", "application/json"}],
    Body = jiffy:encode(#{ <<"foo">> => <<"bar">> }),
    Pid ! {httpc, {ReqId, {Status, Headers, Body}}},
    {ok, ReqId}
  end),
  {ok, Res} = plaid_erl:request(Pid, <<"/item/get">>, #{ <<"access_token">> => <<"token">>}),
  ?assert(is_map(Res)),
  ?_assertEqual(maps:get(<<"foo">>, Res), <<"bar">>).

request_async_test_run_(Pid) ->
  meck:expect(httpc, request, fun (Method, {Url, _RHeaders, CType, RBody}, _HTTPOpts, _Opts) ->
    ?assertEqual(Method, post),
    ?assertEqual(Url, "https://sandbox.plaid.com/item/get"),
    ?assertEqual(CType, "application/json"),
    ?assertEqual(RBody, jiffy:encode(#{ <<"access_token">> => <<"token">> })),
    ReqId = make_ref(),
    Status = {"HTTP/1.1", 200, "OK"},
    Headers = [{"content-type", "application/json"}],
    Body = jiffy:encode(#{ <<"foo">> => <<"bar">> }),
    Pid ! {httpc, {ReqId, {Status, Headers, Body}}},
    {ok, ReqId}
  end),
  Ref = plaid_erl:request_async(Pid, <<"/item/get">>, #{ <<"access_token">> => <<"token">>}),
  {ok, Res} = receive
                {Ref, Ret} -> Ret
              end,
  ?assert(is_map(Res)),
  ?_assertEqual(maps:get(<<"foo">>, Res), <<"bar">>).

%% Internal functions

connect() ->
  Init = #plaid_init{
    client_id = <<"foo">>,
    secret = <<"bar">>,
    env = <<"sandbox">>
  },
  {ok, Pid} = plaid_erl:start_link(Init),
  meck:new(httpc, [passthrough]),
  Pid.

close(Pid) ->
  meck:unload(httpc),
  plaid_erl:stop(Pid).
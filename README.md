plaid_erl
=====

An OTP application to integrate with the plaid API. Exports a gen_server compatible module, `plaid_erl`, which sits on top
of an `httpc` client. The internal gen_server uses asynchronous calls against its internal `httpc` client to improve
throughput.

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 test

Usage
-----

Initialization using plaid environment:
```erlang
Init = #plaid_init{
  client_id=ClientID,
  secret=Secret,
  env=Env
},
{ok, Pid} = plaid_erl:start_link(Init)
```

Initialization using base url:
```erlang
Init = #plaid_init{
  client_id=ClientID,
  secret=Secret,
  url=URL
},
{ok, Pid} = plaid_erl:start_link(Init)
```

Generic request:
```erlang
UrlSuffix = <<"/item/get">>,
Values = #{ <<"access_token">> => AccessToken },
{ok, Res} = plaid_erl:request(Pid, UrlSuffix, Values)
```

Exported function request:
```erlang
{ok, Res} = plaid_erl:item_get(Pid, AccessToken)
```

Stop:
```erlang
plaid_erl:stop(Pid)
```
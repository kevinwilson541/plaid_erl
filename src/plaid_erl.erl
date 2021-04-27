-module(plaid_erl).

-behavior(gen_server).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
  % server control %
  start_link/1,
  stop/1,
  % item methods %
  item_get/2,
  item_get/3,
  item_remove/2,
  item_remove/3,
  item_webhook_update/3,
  item_webhook_update/4,
  % institution methods %
  institutions_get/4,
  institutions_get/5,
  institutions_get_by_id/3,
  institutions_get_by_id/4,
  institutions_search/4,
  institutions_search/5,
  % account methods %
  accounts_get/2,
  accounts_get/3,
  % token methods %
  link_token_create/6,
  link_token_create/7,
  link_token_get/2,
  link_token_get/3,
  item_public_token_exchange/2,
  item_public_token_exchange/3,
  item_access_token_invalidate/2,
  item_access_token_invalidate/3,
  % processor methods %
  processor_token_create/4,
  processor_token_create/5,
  processor_stripe_token_create/3,
  processor_stripe_token_create/4,
  processor_auth_get/2,
  processor_auth_get/3,
  processor_balance_get/2,
  processor_balance_get/3,
  processor_identity_get/2,
  processor_identity_get/3
  % sandbox methods %
  % employer methods %
  % transaction methods %
  % auth methods %
  % balance methods %
  % identity methods %
  % asset methods %
  % investment methods %
  % liabilities methods %
  % payment initiation methods %
  % bank transfer methods %
  % deposit switch methods %
]).

-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

-include("plaid_erl.hrl").


%% public methods %%

-spec start_link(Args :: plaid_init()) -> {ok, pid()}.
start_link(Args=#plaid_init{}) ->
  gen_server:start_link(?MODULE, Args, []).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
  gen_server:stop(Pid).

-spec item_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_get(Pid, Token) ->
  item_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_get(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
item_get(Pid, Token, Timeout) when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(Pid, #plaid_req{ type=?ITEM_GET, values=Values, url_suffix = <<"/item/get">> }, Timeout).

-spec item_remove(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_remove(Pid, Token) ->
  item_remove(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_remove(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
item_remove(Pid, Token, Timeout) when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(Pid, #plaid_req{ type=?ITEM_REMOVE, values=Values, url_suffix = <<"/item/remove">> }, Timeout).

-spec item_webhook_update(Pid :: pid(), Token :: binary(), Webhook :: binary()) -> plaid_erl_res().
item_webhook_update(Pid, Token, Webhook) ->
  item_webhook_update(Pid, Token, Webhook, ?DEFAULT_CALL_TIMEOUT).

-spec item_webhook_update(
    Pid :: pid(),
    Token :: binary(),
    Webhook :: binary(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
item_webhook_update(Pid, Token, Webhook, Timeout) when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{
    <<"access_token">> => Token,
    <<"webhook">> => Webhook
  },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ITEM_WEBHOOK_UPDATE, values=Values, url_suffix = <<"/item/webhook/update">> },
    Timeout
  ).

-spec institutions_get(
    Pid :: pid(),
    Count :: non_neg_integer(),
    Offset :: non_neg_integer(),
    Options :: maps:map()
) -> plaid_erl_res().
institutions_get(Pid, Count, Offset, Options) ->
  institutions_get(Pid, Count, Offset, Options, ?DEFAULT_CALL_TIMEOUT).

-spec institutions_get(
    Pid :: pid(),
    Count :: non_neg_integer(),
    Offset :: non_neg_integer(),
    Options :: maps:map(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
institutions_get(Pid, Count, Offset, Options, Timeout)
  when  is_integer(count),
        count >= 0,
        is_integer(offset),
        offset >= 0,
        is_map(options),
        is_integer(Timeout), Timeout > 0 ->

  Options2 = maps:with([
    <<"products">>,
    <<"country_codes">>,
    <<"routing_numbers">>,
    <<"oauth">>,
    <<"include_optional_metadata">>
  ], Options),
  Values = #{
    <<"count">> => Count,
    <<"offset">> => Offset,
    <<"options">> => Options2
  },
  gen_server:call(
    Pid,
    #plaid_req{ type=?INSTITUTIONS_GET, values=Values, url_suffix = <<"/institutions/get">> },
    Timeout
  ).

-spec institutions_get_by_id(Pid :: pid(), Id :: binary(), Options :: maps:map()) -> plaid_erl_res().
institutions_get_by_id(Pid, Id, Options) ->
  institutions_get_by_id(Pid, Id, Options, ?DEFAULT_CALL_TIMEOUT).

-spec institutions_get_by_id(
    Pid :: pid(),
    Id :: binary(),
    Options :: maps:map(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
institutions_get_by_id(Pid, Id, Options, Timeout)
  when is_binary(Id), is_map(Options), is_integer(Timeout), Timeout > 0 ->

  Options2 = maps:with([
    <<"include_optional_metadata">>,
    <<"include_status">>
  ], Options),
  Values = #{
    <<"institution_id">> => Id,
    <<"options">> => Options2
  },
  gen_server:call(
    Pid,
    #plaid_req{ type=?INSTITUTIONS_GET_BY_ID, values=Values, url_suffix = <<"/institutions/get_by_id">> },
    Timeout
  ).

-spec institutions_search(Pid :: pid(), Query :: binary(), Products :: list(), Options :: maps:map()) -> plaid_erl_res().
institutions_search(Pid, Query, Products, Options) ->
  institutions_search(Pid, Query, Products, Options, ?DEFAULT_CALL_TIMEOUT).

-spec institutions_search(
    Pid :: pid(),
    Query :: binary(),
    Products :: list(),
    Options :: maps:map(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
institutions_search(Pid, Query, Products, Options, Timeout)
  when is_binary(Query), is_list(Products), is_map(Options), is_integer(Timeout), Timeout > 0 ->

  Options2 = maps:with([
    <<"country_codes">>,
    <<"oauth">>,
    <<"include_optional_metadata">>
  ], Options),
  Values = #{
    <<"query">> => Query,
    <<"products">> => Products,
    <<"options">> => Options2
  },
  gen_server:call(
    Pid,
    #plaid_req{ type=?INSTITUTIONS_SEARCH, values=Values, url_suffix = <<"/institutions/search">> },
    Timeout
  ).

-spec accounts_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
accounts_get(Pid, Token) ->
  accounts_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec accounts_get(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
accounts_get(Pid, Token, Timeout) ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ACCOUNTS_GET, values=Values, url_suffix = <<"/accounts/get">> },
    Timeout
  ).

-spec link_token_create(
    Pid :: pid(),
    Name :: binary(),
    Lang :: binary(),
    Codes :: list(),
    User :: maps:map(),
    Options :: maps:map()
) -> plaid_erl_res().
link_token_create(Pid, Name, Lang, Codes, User, Options) ->
  link_token_create(Pid, Name, Lang, Codes, User, Options, ?DEFAULT_CALL_TIMEOUT).

-spec link_token_create(
    Pid :: pid(),
    Name :: binary(),
    Lang :: binary(),
    Codes :: list(),
    User :: maps:map(),
    Options :: maps:map(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
link_token_create(Pid, Name, Lang, Codes, User, Options, Timeout)
  when is_binary(Name), is_binary(Lang), is_list(Codes), is_map(User), is_map(Options) ->
  Options2 = maps:with([
    <<"products">>,
    <<"webhook">>,
    <<"access_token">>,
    <<"link_customization_name">>,
    <<"redirect_uri">>,
    <<"android_package_name">>,
    <<"account_filters">>,
    <<"payment_initiation">>,
    <<"deposit_switch">>
  ], Options),
  Values = maps:merge(#{
    <<"client_name">> => Name,
    <<"language">> => Lang,
    <<"country_codes">> => Codes,
    <<"user">> => User
  }, Options2),
  gen_server:call(
    Pid,
    #plaid_req{ type=?LINK_TOKEN_CREATE, values=Values, url_suffix = <<"/link/token/create">> },
    Timeout
  ).

-spec link_token_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
link_token_get(Pid, Token) ->
  link_token_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec link_token_get(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
link_token_get(Pid, Token, Timeout) when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"link_token">> => Token },
  gen_server:call(Pid, #plaid_req{ type=?LINK_TOKEN_GET, values=Values, url_suffix = <<"/link/token/get">> }, Timeout).

-spec item_public_token_exchange(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_public_token_exchange(Pid, Token) ->
  item_public_token_exchange(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_public_token_exchange(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
item_public_token_exchange(Pid, Token, Timeout) when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"public_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ITEM_PUBLIC_TOKEN_EXCHANGE, values=Values, url_suffix = <<"/item/public_token/exchange">> },
    Timeout
  ).

-spec item_access_token_invalidate(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_access_token_invalidate(Pid, Token) ->
  item_access_token_invalidate(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_access_token_invalidate(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
item_access_token_invalidate(Pid, Token, Timeout) when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ITEM_ACCESS_TOKEN_INVALIDATE, values=Values, url_suffix = <<"/item/access_token/invalidate">> },
    Timeout
  ).

-spec processor_token_create(
    Pid :: pid(),
    Token :: binary(),
    AccountID :: binary(),
    Processor :: binary()
) -> plaid_erl_res().
processor_token_create(Pid, Token, AccountID, Processor) ->
  processor_token_create(Pid, Token, AccountID, Processor, ?DEFAULT_CALL_TIMEOUT).

-spec processor_token_create(
    Pid :: pid(),
    Token :: binary(),
    AccountID :: binary(),
    Processor :: binary(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
processor_token_create(Pid, Token, AccountID, Processor, Timeout)
  when is_binary(Token), is_binary(AccountID), is_binary(Processor), is_integer(Timeout), Timeout > 0 ->
  Values = #{
    <<"access_token">> => Token,
    <<"account_id">> => AccountID,
    <<"processor">> => Processor
  },
  gen_server:call(
    Pid,
    #plaid_req{ type=?PROCESSOR_TOKEN_CREATE, values=Values, url_suffix = <<"/processor/token/create">> },
    Timeout
  ).

-spec processor_stripe_token_create(Pid :: pid(), Token :: binary(), AccountID :: binary()) -> plaid_erl_res().
processor_stripe_token_create(Pid, Token, AccountID) ->
  processor_stripe_token_create(Pid, Token, AccountID, ?DEFAULT_CALL_TIMEOUT).

-spec processor_stripe_token_create(
    Pid :: pid(),
    Token :: binary(),
    AccountID :: binary(),
    Timeout :: non_neg_integer()
) -> plaid_erl_res().
processor_stripe_token_create(Pid, Token, AccountID, Timeout)
  when is_binary(Token), is_binary(AccountID), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token, <<"account_id">> => AccountID },
  Req = #plaid_req{
    type=?PROCESSOR_STRIPE_BANK_ACCOUNT_TOKEN_CREATE,
    values=Values,
    url_suffix = <<"/processor/stripe/bank_account_token/create">>
  },
  gen_server:call(Pid, Req, Timeout).

-spec processor_auth_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
processor_auth_get(Pid, Token) ->
  processor_auth_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec processor_auth_get(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
processor_auth_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"processor_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?PROCESSOR_AUTH_GET, values=Values, url_suffix = <<"/processor/auth/get">> },
    Timeout
  ).

-spec processor_balance_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
processor_balance_get(Pid, Token) ->
  processor_balance_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec processor_balance_get(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
processor_balance_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"processor_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?PROCESSOR_BALANCE_GET, values=Values, url_suffix = <<"/processor/balance/get">> },
    Timeout
  ).

-spec processor_identity_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
processor_identity_get(Pid, Token) ->
  processor_identity_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec processor_identity_get(Pid :: pid(), Token :: binary(), Timeout :: non_neg_integer()) -> plaid_erl_res().
processor_identity_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"processor_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?PROCESSOR_IDENTITY_GET, values=Values, url_suffix = <<"/processor/identity/get">> },
    Timeout
  ).


%% gen_server callbacks %%

-spec init(Args :: plaid_init()) -> {ok, plaid_state()}.
init(#plaid_init{ client_id=ClientID, secret=Secret, env=Env, timeout=Timeout, connect_timeout=ConnectTimeout })
  when is_binary(ClientID), is_binary(Secret), is_binary(Env) ->
  NTimeout = if
               is_integer(Timeout) and Timeout > 0 -> Timeout;
               true -> ?DEFAULT_TIMEOUT
             end,
  NConnTimeout = if
                   is_integer(ConnectTimeout) and ConnectTimeout > 0 -> ConnectTimeout;
                   true -> ?DEFAULT_CONNECT_TIMEOUT
                 end,
  {ok, #plaid_state{
    client_id=ClientID,
    secret=Secret,
    env=Env,
    waiting=maps:new(),
    timeout=NTimeout,
    connect_timeout=NConnTimeout}
  }.

-spec terminate(Reason :: term(), State :: plaid_state()) -> ok.
terminate(_Reason, #plaid_state{waiting=Waiting}) ->
  terminate_pending_reqs(maps:to_list(Waiting)),
  ok.

-spec handle_call(Req :: plaid_req(), From :: term(), State :: plaid_state()) -> {noreply, plaid_state()}.
handle_call(Req=#plaid_req{}, From, State) ->
  State2 = handle_req(Req, From, State),
  {noreply, State2};
handle_call(_Req, _From, State) ->
  {noreply, State}.

-spec handle_cast(Cast :: term(), State :: plaid_state()) -> {noreply, plaid_state()}.
handle_cast(_Cast, State) ->
  {noreply, State}.

-spec handle_info(Info :: term(), State :: plaid_state()) -> {noreply, plaid_state()}.
handle_info({http, {ReqID, {error, _Reason}}}, State) ->
  #plaid_state{ waiting=W } = State,

  case maps:get(ReqID, W, undefined) of
    undefined ->
      ok;
    #plaid_req_ctx{ from=From } ->
      handle_reply(From, {error, #plaid_erl_error{ code=?HTTP_ERROR, http_code=?UNKNOWN_HTTP_CODE, message = <<"HTTP connection error occurred.">> }})
  end,

  W2 = maps:remove(ReqID, W),
  {noreply, State#plaid_state{ waiting=W2 }};
handle_info({http, {ReqID, {Status, Headers, Body}}}, State) ->
  #plaid_state{ waiting=W } = State,

  case maps:get(ReqID, W, undefined) of
    undefined ->
      ok;
    #plaid_req_ctx{ from=From, type=Type } ->
      Result = parse_result(Type, Status, Headers, Body),
      handle_reply(From, Result)
  end,

  W2 = maps:remove(ReqID, W),
  {noreply, State#plaid_state{ waiting=W2 }};
handle_info(_Info, State) ->
  {noreply, State}.


%% private functions %%

terminate_pending_reqs([]) ->
  ok;
terminate_pending_reqs([{ReqID, #plaid_req_ctx{from=From}} | Rest]) ->
  httpc:cancel_request(ReqID),
  gen_server:reply(From, {error, #plaid_erl_error{ code=?INTERNAL_ERROR, message = <<"Request prematurely terminated due to process shutoff.">> }}),
  terminate_pending_reqs(Rest).

handle_req(#plaid_req{ type=Type, values=Values, url_suffix=UrlSuffix }, From, State) ->
  #plaid_state{ client_id=ClientID, secret=Secret, waiting=W } = State,
  Url = binary:bin_to_list(<< (construct_base_url(State))/binary, UrlSuffix/binary>>),
  Headers = construct_headers(),
  Body = jiffy:encode(maps:merge(#{
    <<"client_id">> => ClientID,
    <<"secret">> => Secret
  }, Values)),
  ReqID = make_req(?HTTP_POST, Url, Headers, Body, State),

  State#plaid_state{ waiting=maps:put(ReqID, #plaid_req_ctx{ from=From, type=Type }, W) }.

construct_headers() ->
  [{ "Content-Type", "application/json" }].

construct_base_url(#plaid_state{ env=Env }) ->
  << "https://", Env/binary, ".plaid.com" >>.

make_req(Method, Url, Headers, Body, #plaid_state{ timeout=Timeout, connect_timeout=ConnTimeout }) ->
  {ok, ReqID} = httpc:request(
    Method,
    {Url, Headers, "application/json", Body},
    [{ timeout, Timeout }, { connect_timeout, ConnTimeout }],
    [{ sync, false }, { receiver, self() }]
  ),

  ReqID.

handle_reply(From, {error, Err=#plaid_error{}}) ->
  gen_server:reply(From, {error, Err}),
  ok;
handle_reply(From, {error, Err=#plaid_erl_error{}}) ->
  gen_server:reply(From, {error, Err}),
  ok;
handle_reply(From, {ok, Body}) ->
  gen_server:reply(From, {ok, Body}),
  ok.

parse_result(Type, {_HTTPVersion, Code, _Reason}, Headers, Body) when Code =:= 200 ->
  Parsable = check_json_res(Headers),
  if
    Parsable -> parse_ok_res(Type, jiffy:decode(Body, [return_maps]));
    true -> {error, #plaid_erl_error{ code=?INTERNAL_ERROR, message = <<"Plaid response is not JSON.">> }}
  end;
parse_result(_Type, {_HTTPVersion, Code, _Reason}, Headers, Body) when Code >= 400 ->
  Parsable = check_json_res(Headers),
  if
    Parsable -> parse_error_json(jiffy:decode(Body, [return_maps]));
    true -> parse_error_res(Code, Body)
  end;
parse_result(_Type, {_HTTPVersion, _Code, _Reason}, _Headers, _Body) ->
  {error, #plaid_erl_error{ code=?INTERNAL_ERROR, message = <<"Unknown plaid response returned.">> }}.

% TODO: parse ok res using request type
parse_ok_res(_Type, Body) when is_map(Body) ->
  {ok, Body}.

parse_error_res(Code, Body) ->
  {error, #plaid_erl_error{ code=?HTTP_ERROR, http_code=Code, message=Body }}.

parse_error_json(Error) when is_map(Error) ->
  #{
    <<"error_code">> := ErrorCode,
    <<"error_type">> := ErrorType,
    <<"error_message">> := ErrorMessage
  } = Error,
  DisplayMessage = map_get_optional(<<"display_message">>, Error),
  ReqId = map_get_optional(<<"request_id">>, Error),
  Causes = map_get_optional(<<"causes">>, Error),
  Status = map_get_optional(<<"status">>, Error),
  DocURL = map_get_optional(<<"documentation_url">>, Error),
  SuggestedAction = map_get_optional(<<"suggested_action">>, Error),
  {error, #plaid_error{
    code=ErrorCode,
    type=ErrorType,
    message=ErrorMessage,
    display=DisplayMessage,
    request_id=ReqId,
    causes=Causes,
    status=Status,
    doc_url=DocURL,
    suggested_action=SuggestedAction}
  }.

map_get_optional(Key, Map) ->
  case maps:get(Key, Map, undefined) of
    undefined -> undefined;
    Val -> Val
  end.

check_json_res(Headers) ->
  Cap = proplists:get_value("Content-Type", Headers),
  Lower = proplists:get_value("content-type", Headers),

  if
    Cap =/= undefined -> string:prefix(Cap, "application/json") =/= nomatch;
    Lower =/= undefined -> string:prefix(Lower, "application/json") =/= nomatch;
    true -> false
  end.
-module(plaid_erl).

-behavior(gen_server).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
  % server control %
  start_link/1,
  stop/1,
  % generic method %
  request/3,
  request/4,
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
  processor_identity_get/3,
  % sandbox methods %
  sandbox_public_token_create/4,
  sandbox_public_token_create/5,
  sandbox_item_reset_login/2,
  sandbox_item_reset_login/3,
  sandbox_item_set_verification_status/4,
  sandbox_item_set_verification_status/5,
  sandbox_item_fire_webhook/3,
  sandbox_item_fire_webhook/4,
  sandbox_bank_transfer_simulate/4,
  sandbox_bank_transfer_simulate/5,
  % employer methods %
  employers_search/3,
  employers_search/4,
  % transaction methods %
  transactions_get/5,
  transactions_get/6,
  transactions_refresh/2,
  transactions_refresh/3,
  categories_get/1,
  categories_get/2,
  % auth methods %
  auth_get/3,
  auth_get/4,
  % balance methods %
  accounts_balance_get/3,
  accounts_balance_get/4,
  % identity methods %
  identity_get/2,
  identity_get/3,
  % asset methods %
  asset_report_create/4,
  asset_report_create/5,
  asset_report_get/3,
  asset_report_get/4,
  asset_report_pdf_get/2,
  asset_report_pdf_get/3,
  asset_report_refresh/3,
  asset_report_refresh/4,
  asset_report_filter/3,
  asset_report_filter/4,
  asset_report_remove/2,
  asset_report_remove/3,
  asset_report_audit_create/3,
  asset_report_audit_create/4,
  asset_report_audit_remove/2,
  asset_report_audit_remove/3
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

-spec request(Pid :: pid(), UrlSuffix :: binary(), Values :: maps:map()) -> plaid_erl_res().
request(Pid, UrlSuffix, Values) ->
  request(Pid, UrlSuffix, Values, ?DEFAULT_CALL_TIMEOUT).

-spec request(
    Pid :: pid(),
    UrlSuffix :: binary(),
    Values :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
request(Pid, UrlSuffix, Values, Timeout)
  when is_binary(UrlSuffix), is_map(Values), is_integer(Timeout), Timeout > 0 ->
  gen_server:call(Pid, #plaid_req{ type=?GENERIC_COMMAND, values=Values, url_suffix=UrlSuffix }, Timeout).

-spec item_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_get(Pid, Token) ->
  item_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
item_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(Pid, #plaid_req{ type=?ITEM_GET, values=Values, url_suffix = <<"/item/get">> }, Timeout).

-spec item_remove(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_remove(Pid, Token) ->
  item_remove(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_remove(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
item_remove(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(Pid, #plaid_req{ type=?ITEM_REMOVE, values=Values, url_suffix = <<"/item/remove">> }, Timeout).

-spec item_webhook_update(Pid :: pid(), Token :: binary(), Webhook :: binary()) -> plaid_erl_res().
item_webhook_update(Pid, Token, Webhook) ->
  item_webhook_update(Pid, Token, Webhook, ?DEFAULT_CALL_TIMEOUT).

-spec item_webhook_update(
    Pid :: pid(),
    Token :: binary(),
    Webhook :: binary(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
item_webhook_update(Pid, Token, Webhook, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
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
    Timeout :: pos_integer()
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
    Timeout :: pos_integer()
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
    Timeout :: pos_integer()
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

-spec accounts_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
accounts_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
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
    Timeout :: pos_integer()
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

-spec link_token_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
link_token_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"link_token">> => Token },
  gen_server:call(Pid, #plaid_req{ type=?LINK_TOKEN_GET, values=Values, url_suffix = <<"/link/token/get">> }, Timeout).

-spec item_public_token_exchange(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_public_token_exchange(Pid, Token) ->
  item_public_token_exchange(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_public_token_exchange(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
item_public_token_exchange(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"public_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ITEM_PUBLIC_TOKEN_EXCHANGE, values=Values, url_suffix = <<"/item/public_token/exchange">> },
    Timeout
  ).

-spec item_access_token_invalidate(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
item_access_token_invalidate(Pid, Token) ->
  item_access_token_invalidate(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec item_access_token_invalidate(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
item_access_token_invalidate(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
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
    Timeout :: pos_integer()
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
    Timeout :: pos_integer()
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

-spec processor_auth_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
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

-spec processor_balance_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
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

-spec processor_identity_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
processor_identity_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"processor_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?PROCESSOR_IDENTITY_GET, values=Values, url_suffix = <<"/processor/identity/get">> },
    Timeout
  ).

-spec sandbox_public_token_create(
    Pid :: pid(),
    InstID :: binary(),
    Products :: list(),
    Options :: maps:map()
) -> plaid_erl_res().
sandbox_public_token_create(Pid, InstID, Products, Options) ->
  sandbox_public_token_create(Pid, InstID, Products, Options, ?DEFAULT_CALL_TIMEOUT).

-spec sandbox_public_token_create(
    Pid :: pid(),
    InstID :: binary(),
    Products :: list(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
sandbox_public_token_create(Pid, InstID, Products, Options, Timeout)
  when is_binary(InstID), is_list(Products), is_map(Options), is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:with([
    <<"webhook">>,
    <<"override_username">>,
    <<"override_password">>
  ], Options),
  Values = #{ <<"institution_id">> => InstID, <<"initial_products">> => Products, <<"options">> => Options2 },
  gen_server:call(
    Pid,
    #plaid_req{ type=?SANDBOX_PUBLIC_TOKEN_CREATE, values=Values, url_suffix = <<"/sandbox/public_token/create">> },
    Timeout
  ).

-spec sandbox_item_reset_login(
    Pid :: pid(),
    Token :: binary()
) -> plaid_erl_res().
sandbox_item_reset_login(Pid, Token) ->
  sandbox_item_reset_login(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec sandbox_item_reset_login(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
sandbox_item_reset_login(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?SANDBOX_ITEM_RESET_LOGIN, values=Values, url_suffix = <<"/sandbox/item/reset_login">> },
    Timeout
  ).

-spec sandbox_item_set_verification_status(
    Pid :: pid(),
    Token :: binary(),
    AccountID :: binary(),
    VerifyStatus :: binary()
) -> plaid_erl_res().
sandbox_item_set_verification_status(Pid, Token, AccountID, VerifyStatus) ->
  sandbox_item_set_verification_status(Pid, Token, AccountID, VerifyStatus, ?DEFAULT_CALL_TIMEOUT).

-spec sandbox_item_set_verification_status(
    Pid :: pid(),
    Token :: binary(),
    AccountID :: binary(),
    VerifyStatus :: binary(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
sandbox_item_set_verification_status(Pid, Token, AccountID, VerifyStatus, Timeout)
  when is_binary(Token), is_binary(AccountID), is_binary(VerifyStatus), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token, <<"account_id">> => AccountID, <<"verification_status">> => VerifyStatus },
  gen_server:call(
    Pid,
    #plaid_req{
      type=?SANDBOX_ITEM_SET_VERIFY_STATUS,
      values=Values,
      url_suffix = <<"/sandbox/item/set_verification_status">>
    },
    Timeout
  ).

-spec sandbox_item_fire_webhook(Pid :: pid(), Token :: binary(), Code :: binary()) -> plaid_erl_res().
sandbox_item_fire_webhook(Pid, Token, Code) ->
  sandbox_item_fire_webhook(Pid, Token, Code, ?DEFAULT_CALL_TIMEOUT).

-spec sandbox_item_fire_webhook(
    Pid :: pid(),
    Token :: binary(),
    Code :: binary(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
sandbox_item_fire_webhook(Pid, Token, Code, Timeout)
  when is_binary(Token), is_binary(Code), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token, <<"webhook_code">> => Code },
  gen_server:call(
    Pid,
    #plaid_req{ type=?SANDBOX_ITEM_FIRE_WEBHOOK, values=Values, url_suffix = <<"/sandbox/item/fire_webhook">> },
    Timeout
  ).

-spec sandbox_bank_transfer_simulate(
    Pid :: pid(),
    TransferID :: binary(),
    Event :: binary(),
    Options :: maps:map()
) -> plaid_erl_res().
sandbox_bank_transfer_simulate(Pid, TransferID, Event, Options) ->
  sandbox_bank_transfer_simulate(Pid, TransferID, Event, Options, ?DEFAULT_CALL_TIMEOUT).

-spec sandbox_bank_transfer_simulate(
    Pid :: pid(),
    TransferID :: binary(),
    Event :: binary(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
sandbox_bank_transfer_simulate(Pid, TransferID, Event, Options, Timeout)
  when is_binary(TransferID), is_binary(Event), is_map(Options), is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:with([
    <<"failure_reason">>
  ], Options),
  Values = maps:merge(#{
    <<"bank_transfer_id">> => TransferID,
    <<"event_type">> => Event
  }, Options2),
  gen_server:call(
    Pid,
    #plaid_req{
      type=?SANDBOX_BANK_TRANSFER_SIMULATE,
      values=Values,
      url_suffix = <<"/sandbox/bank_transfer/simulate">>
    },
    Timeout
  ).

-spec employers_search(Pid :: pid(), Query :: binary(), Products :: list()) -> plaid_erl_res().
employers_search(Pid, Query, Products) ->
  employers_search(Pid, Query, Products, ?DEFAULT_CALL_TIMEOUT).

-spec employers_search(
    Pid :: pid(),
    Query :: binary(),
    Products :: list(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
employers_search(Pid, Query, Products, Timeout)
  when is_binary(Query), is_list(Products), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"query">> => Query, <<"products">> => Products },
  gen_server:call(
    Pid,
    #plaid_req{ type=?EMPLOYERS_SEARCH, values=Values, url_suffix = <<"/employers/search">> },
    Timeout
  ).

-spec transactions_get(
    Pid :: pid(),
    Token :: binary(),
    Start :: binary(),
    End :: binary(),
    Options :: maps:map()
) -> plaid_erl_res().
transactions_get(Pid, Token, Start, End, Options) ->
  transactions_get(Pid, Token, Start, End, Options, ?DEFAULT_CALL_TIMEOUT).

-spec transactions_get(
    Pid :: pid(),
    Token :: binary(),
    Start :: binary(),
    End :: binary(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
transactions_get(Pid, Token, Start, End, Options=#{ <<"count">> := Count, <<"offset">> := Offset }, Timeout)
  when  is_binary(Token),
        is_binary(Start),
        is_binary(End),
        is_integer(Count),
        Count >= 0,
        is_integer(Offset),
        Offset >= 0,
        is_integer(Timeout),
        Timeout > 0 ->
  Options2 = maps:with([
    <<"count">>,
    <<"offset">>,
    <<"account_ids">>
  ], Options),
  Values = maps:merge(
    #{ <<"options">> => Options2 },
    #{ <<"access_token">> => Token, <<"start_date">> => Start, <<"end_date">> => End }
  ),
  gen_server:call(
    Pid,
    #plaid_req{ type=?TRANSACTIONS_GET, values=Values, url_suffix = <<"/transactions/get">> },
    Timeout
  ).

-spec transactions_refresh(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
transactions_refresh(Pid, Token) ->
  transactions_refresh(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec transactions_refresh(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
transactions_refresh(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?TRANSACTIONS_REFRESH, values=Values, url_suffix = <<"/transactions/refresh">> },
    Timeout
  ).

-spec categories_get(Pid :: pid()) -> plaid_erl_res().
categories_get(Pid) ->
  categories_get(Pid, ?DEFAULT_CALL_TIMEOUT).

-spec categories_get(Pid :: pid(), Timeout :: pos_integer()) -> plaid_erl_res().
categories_get(Pid, Timeout)
  when is_integer(Timeout), Timeout > 0 ->
  Values = #{},
  gen_server:call(
    Pid,
    #plaid_req{ type=?CATEGORIES_GET, values=Values, url_suffix = <<"/categories/get">> },
    Timeout
  ).

-spec auth_get(Pid :: pid(), Token :: binary(), Options :: maps:map()) -> plaid_erl_res().
auth_get(Pid, Token, Options) ->
  auth_get(Pid, Token, Options, ?DEFAULT_CALL_TIMEOUT).

-spec auth_get(Pid :: pid(), Token :: binary(), Options :: maps:map(), Timeout :: pos_integer()) -> plaid_erl_res().
auth_get(Pid, Token, Options, Timeout)
  when is_binary(Token), is_map(Options), is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:with([
    <<"account_ids">>
  ], Options),
  Values = #{ <<"access_token">> => Token, <<"options">> => Options2 },
  gen_server:call(
    Pid,
    #plaid_req{ type=?AUTH_GET, values=Values, url_suffix = <<"/auth/get">> },
    Timeout
  ).

-spec accounts_balance_get(Pid :: pid(), Token :: binary(), Options :: maps:map()) -> plaid_erl_res().
accounts_balance_get(Pid, Token, Options) ->
  accounts_balance_get(Pid, Token, Options, ?DEFAULT_CALL_TIMEOUT).

-spec accounts_balance_get(
    Pid :: pid(),
    Token :: binary(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
accounts_balance_get(Pid, Token, Options, Timeout)
  when is_binary(Token), is_map(Options), is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:with([
    <<"account_ids">>,
    <<"min_last_updated_time">>
  ], Options),
  Values = #{ <<"access_token">> => Token, <<"options">> => Options2 },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ACCOUNTS_BALANCE_GET, values=Values, url_suffix = <<"/accounts/balance/get">> },
    Timeout
  ).

-spec identity_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
identity_get(Pid, Token) ->
  identity_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec identity_get(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
identity_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?IDENTITY_GET, values=Values, url_suffix = <<"/identity/get">> },
    Timeout
  ).

-spec asset_report_create(
    Pid :: pid(),
    Tokens :: list(),
    Days :: non_neg_integer(),
    Options :: maps:map()
) -> plaid_erl_res().
asset_report_create(Pid, Tokens, Days, Options) ->
  asset_report_create(Pid, Tokens, Days, Options, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_create(
    Pid :: pid(),
    Tokens :: list(),
    Days :: non_neg_integer(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
asset_report_create(Pid, Tokens, Days, Options, Timeout)
  when is_list(Tokens), is_integer(Days), is_map(Options), Days >= 0, is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:with([
    <<"client_report_id">>,
    <<"webhook">>,
    <<"user">>
  ], Options),
  Values = #{ <<"access_tokens">> => Tokens, <<"days_requested">> => Days, <<"options">> => Options2 },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ASSET_REPORT_CREATE, values=Values, url_suffix = <<"/asset_report/create">> },
    Timeout
  ).

-spec asset_report_get(Pid :: pid(), Token :: binary(), Options :: maps:map()) -> plaid_erl_res().
asset_report_get(Pid, Token, Options) ->
  asset_report_get(Pid, Token, Options, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_get(
    Pid :: pid(),
    Token :: binary(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
asset_report_get(Pid, Token, Options, Timeout)
  when is_binary(Token), is_map(Options), is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:get([
    <<"include_insights">>
  ], Options),
  Values = maps:merge(Options2, #{ <<"access_report_token">> => Token}),
  gen_server:call(
    Pid,
    #plaid_req{ type=?ASSET_REPORT_GET, values=Values, url_suffix = <<"/asset_report/get">> },
    Timeout
  ).

-spec asset_report_pdf_get(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
asset_report_pdf_get(Pid, Token) ->
  asset_report_pdf_get(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_pdf_get(Pid :: pid(), Tkoen :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
asset_report_pdf_get(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_report_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ASSET_REPORT_PDF_GET, values=Values, url_suffix = <<"/asset_report/pdf/get">> },
    Timeout
  ).

-spec asset_report_refresh(Pid :: pid(), Token :: binary(), Options :: maps:map()) -> plaid_erl_res().
asset_report_refresh(Pid, Token, Options) ->
  asset_report_refresh(Pid, Token, Options, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_refresh(
    Pid :: pid(),
    Token :: binary(),
    Options :: maps:map(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
asset_report_refresh(Pid, Token, Options, Timeout)
  when is_binary(Token), is_map(Options), is_integer(Timeout), Timeout > 0 ->
  Options2 = maps:with([
    <<"days_requested">>,
    <<"options">>
  ], Options),
  Values = maps:merge(Options2, #{ <<"access_report_token">> => Token }),
  gen_server:call(
    Pid,
    #plaid_req{ type=?ASSET_REPORT_REFRESH, values=Values, url_suffix = <<"/asset_report/refresh">> },
    Timeout
  ).

-spec asset_report_filter(Pid :: pid(), Token :: binary(), Exclude :: list()) -> plaid_erl_res().
asset_report_filter(Pid, Token, Exclude) ->
  asset_report_filter(Pid, Token, Exclude, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_filter(
    Pid :: pid(),
    Token :: binary(),
    Exclude :: list(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
asset_report_filter(Pid, Token, Exclude, Timeout)
  when is_binary(Token), is_list(Exclude), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_report_token">> => Token, <<"account_ids_to_exclude">> => Exclude },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ASSET_REPORT_FILTER, values=Values, url_suffix = <<"/asset_report/filter">> },
    Timeout
  ).

-spec asset_report_remove(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
asset_report_remove(Pid, Token) ->
  asset_report_remove(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_remove(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
asset_report_remove(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_report_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{ type=?ASSET_REPORT_REMOVE, values=Values, url_suffix = <<"/asset_report/remove">> },
    Timeout
  ).

-spec asset_report_audit_create(Pid :: pid(), Token :: binary(), Auditor :: binary()) -> plaid_erl_res().
asset_report_audit_create(Pid, Token, Auditor) ->
  asset_report_audit_create(Pid, Token, Auditor, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_audit_create(
    Pid :: pid(),
    Token :: binary(),
    Auditor :: binary(),
    Timeout :: pos_integer()
) -> plaid_erl_res().
asset_report_audit_create(Pid, Token, Auditor, Timeout)
  when is_binary(Token), is_binary(Auditor), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_report_token">> => Token, <<"auditor_id">> => Auditor },
  gen_server:call(
    Pid,
    #plaid_req{
      type=?ASSET_REPORT_AUDIT_COPY_CREATE,
      values=Values,
      url_suffix = <<"/asset_report/audit_copy/create">>
    },
    Timeout
  ).

-spec asset_report_audit_remove(Pid :: pid(), Token :: binary()) -> plaid_erl_res().
asset_report_audit_remove(Pid, Token) ->
  asset_report_audit_remove(Pid, Token, ?DEFAULT_CALL_TIMEOUT).

-spec asset_report_audit_remove(Pid :: pid(), Token :: binary(), Timeout :: pos_integer()) -> plaid_erl_res().
asset_report_audit_remove(Pid, Token, Timeout)
  when is_binary(Token), is_integer(Timeout), Timeout > 0 ->
  Values = #{ <<"access_report_token">> => Token },
  gen_server:call(
    Pid,
    #plaid_req{
      type=?ASSET_REPORT_AUDIT_COPY_REMOVE,
      values=Values,
      url_suffix = <<"/asset_report/audit_copy/remove">>
    },
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
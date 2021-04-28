
-record(plaid_init, { client_id, secret, env, timeout, connect_timeout }).
-record(plaid_state, { client_id, secret, env, timeout, connect_timeout, waiting }).
-record(plaid_req, { type, values, url_suffix }).
-record(plaid_req_ctx, { from, type }).
-record(plaid_error, { code, type, message, display, request_id, causes, status, suggested_action, doc_url }).
-record(plaid_erl_error, { code, http_code, message }).

-type plaid_init() :: #plaid_init{}.
-type plaid_state() :: #plaid_state{}.
-type plaid_req() :: #plaid_req{}.
-type plaid_req_ctx() :: #plaid_req_ctx{}.
-type plaid_error() :: #plaid_error{}.
-type plaid_erl_error() :: #plaid_erl_error{}.
-type plaid_erl_res() :: {ok, maps:map()} | {error, plaid_error()} | {error, plaid_erl_error()}.

-define(DEFAULT_TIMEOUT, 90000).
-define(DEFAULT_CONNECT_TIMEOUT, 30000).
-define(INTERNAL_ERROR, internal).
-define(HTTP_ERROR, http).
-define(UNKNOWN_HTTP_CODE, unknown).
-define(DEFAULT_CALL_TIMEOUT, 5000).
-define(HTTP_POST, post).
-define(DEFAULT_COUNT, 100).
-define(DEFAULT_OFFSET, 0).

% generic command
-define(GENERIC_COMMAND, generic_command).
% item commands
-define(ITEM_GET, item_get).
-define(ITEM_REMOVE, item_remove).
-define(ITEM_WEBHOOK_UPDATE, item_webhook_update).
% institution commands
-define(INSTITUTIONS_GET, institutions_get).
-define(INSTITUTIONS_GET_BY_ID, institutions_get_by_id).
-define(INSTITUTIONS_SEARCH, institutions_search).
% account commands
-define(ACCOUNTS_GET, accounts_get).
% token commands
-define(LINK_TOKEN_CREATE, link_token_create).
-define(LINK_TOKEN_GET, link_token_get).
-define(ITEM_PUBLIC_TOKEN_EXCHANGE, item_public_token_exchange).
-define(ITEM_ACCESS_TOKEN_INVALIDATE, item_access_token_invalidate).
% processor commands
-define(PROCESSOR_TOKEN_CREATE, processor_token_create).
-define(PROCESSOR_STRIPE_BANK_ACCOUNT_TOKEN_CREATE, processor_stripe_bank_account_token_create).
-define(PROCESSOR_AUTH_GET, processor_auth_get).
-define(PROCESSOR_BALANCE_GET, processor_balance_get).
-define(PROCESSOR_IDENTITY_GET, processor_identity_get).
% sandbox commands
-define(SANDBOX_PUBLIC_TOKEN_CREATE, sandbox_public_token_create).
-define(SANDBOX_ITEM_RESET_LOGIN, sandbox_item_reset_login).
-define(SANDBOX_ITEM_SET_VERIFY_STATUS, sandbox_item_set_verify_status).
-define(SANDBOX_ITEM_FIRE_WEBHOOK, sandbox_item_fire_webhook).
-define(SANDBOX_BANK_TRANSFER_SIMULATE, sandbox_bank_transfer_simulate).
-define(SANDBOX_BANK_TRANSFER_FIRE_WEBHOOK, sandbox_bank_transfer_fire_webhook).
-define(SANDBOX_PROCESSOR_TOKEN_CREATE, sandbox_processor_token_create).
% employer commands
-define(EMPLOYERS_SEARCH, employers_search).
% transaction commands
-define(TRANSACTIONS_GET, transactions_get).
-define(TRANSACTIONS_REFRESH, transactions_refresh).
-define(CATEGORIES_GET, categories_get).
% auth commands
-define(AUTH_GET, auth_get).
% balance commands
-define(ACCOUNTS_BALANCE_GET, accounts_balance_get).
% identity commands
-define(IDENTITY_GET, identity_get).
% asset commands
-define(ASSET_REPORT_CREATE, asset_report_create).
-define(ASSET_REPORT_GET, asset_report_get).
-define(ASSET_REPORT_PDF_GET, asset_report_pdf_get).
-define(ASSET_REPORT_REFRESH, asset_report_refresh).
-define(ASSET_REPORT_FILTER, asset_report_filter).
-define(ASSET_REPORT_REMOVE, asset_report_remove).
-define(ASSET_REPORT_AUDIT_COPY_CREATE, asset_report_audit_copy_create).
-define(ASSET_REPORT_AUDIT_COPY_REMOVE, asset_report_audit_copy_remove).
% investment commands
-define(INVESTMENTS_HOLDINGS_GET, investments_holdings_get).
-define(INVESTMENTS_TRANSACTIONS_GET, investments_transactions_get).
% liabilities commands
-define(LIABILITIES_GET, liabilities_get).
% payment initiation commands
-define(PAYMENT_INITIATION_RECIPIENT_CREATE, payment_initiation_recipient_create).
-define(PAYMENT_INITIATION_RECIPIENT_GET, payment_initiation_recipient_get).
-define(PAYMENT_INITIATION_RECIPIENT_LIST, payment_initiation_recipient_list).
-define(PAYMENT_INITIATION_PAYMENT_CREATE, payment_initiation_payment_create).
-define(PAYMENT_INITIATION_PAYMENT_GET, payment_initiation_payment_get).
-define(PAYMENT_INITIATION_PAYMENT_LIST, payment_initiation_payment_list).
% bank transfer commands
-define(BANK_TRANSFER_CREATE, bank_transfer_create).
-define(BANK_TRANSFER_CANCEL, bank_transfer_cancel).
-define(BANK_TRANSFER_GET, bank_transfer_get).
-define(BANK_TRANSFER_LIST, bank_transfer_list).
-define(BANK_TRANSFER_EVENT_LIST, bank_transfer_event_list).
-define(BANK_TRANSFER_EVENT_SYNC, bank_transfer_event_sync).
-define(BANK_TRANSFER_MIGRATE_ACCOUNT, bank_transfer_migrate_account).
-define(BANK_TRANSFER_BALANCE_GET, bank_transfer_balance_get).
% deposit switch commands
-define(DEPOSIT_SWITCH_CREATE, deposit_switch_create).
-define(DEPOSIT_SWITCH_ALT_CREATE, deposit_switch_alt_create).
-define(DEPOSIT_SWITCH_GET, deposit_switch_get).
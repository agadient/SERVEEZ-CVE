/* guile-server konstants -- see gsk-make */
static symstr_t guile_functions[] = {
  { .str = "global-init" },
  { .str = "init" },
  { .str = "detect-proto" },
  { .str = "connect-socket" },
  { .str = "finalize" },
  { .str = "global-finalize" },
  { .str = "info-client" },
  { .str = "info-server" },
  { .str = "notify" },
  { .str = "reset" },
  { .str = "handle-request" }
};
#define guile_functions_count 11
enum guile_functions_ix {
  fn_global_init,
  fn_init,
  fn_detect_proto,
  fn_connect_socket,
  fn_finalize,
  fn_global_finalize,
  fn_info_client,
  fn_info_server,
  fn_notify,
  fn_reset,
  fn_handle_request
};
static symstr_t guile_sock_fns[] = {
  { .str = "disconnected" },
  { .str = "kicked" },
  { .str = "check-request" },
  { .str = "handle-request" },
  { .str = "idle" },
  { .str = "trigger-condition" },
  { .str = "trigger" },
  { .str = "check-oob-request" }
};
#define guile_sock_fns_count 8
enum guile_sock_fns_ix {
  sfn_disconnected,
  sfn_kicked,
  sfn_check_request,
  sfn_handle_request,
  sfn_idle,
  sfn_trigger_condition,
  sfn_trigger,
  sfn_check_oob_request
};
/* guile-server konstants ends here */

-ifndef(ERLANG_DOCKER_HTTP_H).
-define(ERLANG_DOCKER_HTTP_H, true).

-define(HTTP_PORT, application:get_env(erlang_docker, http_port, 8080)).

-define(HTTP_METHOD_GET, <<"GET">>).
-define(HTTP_METHOD_POST, <<"POST">>).

-define(HTTP_STATUS_OK, 200).

-define(HTTP_CODE_SUCCESS, 0).
-define(HTTP_CODE_FAIL, 1).

-define(NODE_A_SVC_DOMAIN, application:get_env(erlang_docker, a_svc_domain, "")).
-define(NODE_B_SVC_DOMAIN, application:get_env(erlang_docker, b_svc_domain, "")).

-endif.

%%%-------------------------------------------------------------------
%%% @doc
%%% http 服务模块
%%% @end
%%%-------------------------------------------------------------------
-module(erlang_docker_http_server).

-include("erlang_docker.hrl").

%% API
-export([
    start/0,
    init/2
]).

%% @doc 定义路由，启动 http 服务
start() ->
    Handler = ?MODULE,
    Port = ?HTTP_PORT,
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', Handler, []},
            {"/ping", Handler, []},
            {"/node", Handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    ok.

%% @doc 初始化
init(Req, Opts) ->
    MsgMap =
        case catch cowboy_req:read_body(Req) of
            {ok, Body, _} ->
                Method = cowboy_req:method(Req),
                Path = cowboy_req:path(Req),
                case Method of
                    ?HTTP_METHOD_GET ->
                        handle_get(Path, Req);
                    ?HTTP_METHOD_POST ->
                        handle_post(Path, Body);
                    Method ->
                        #{code => ?HTTP_CODE_FAIL, data => <<>>, msg => <<"HTTP server unused method type!">>}
                end;
            _Error ->
                #{code => ?HTTP_CODE_FAIL, data => <<>>, msg => <<"HTTP read body fail!">>}
        end,
    {ok, {ok, pack_req(MsgMap, Req)}, Opts}.

%% @doc 响应
pack_req(MsgMap, Req) ->
    Json = jsx:encode(MsgMap),
    cowboy_req:reply(?HTTP_STATUS_OK, #{
        <<"content-type">> => <<"application/json">>
    }, Json, Req).

%% @doc Get
handle_get(Path, Req) ->
    ParseQs = cowboy_req:parse_qs(Req),
    handle_get_do(Path, ParseQs).
%% @doc ping pong
handle_get_do(<<"/ping">>, _ParseQs) ->
    #{code => ?HTTP_CODE_SUCCESS, data => <<>>, msg => <<"pong">>};
handle_get_do(_Method, _ParseQs) ->
    #{code => ?HTTP_CODE_FAIL, msg => <<"error path">>}.

%% @doc Post
handle_post(Path, Body) ->
    case catch erlang_docker_misc:json_decode(Body) of
        Map when is_map(Map) ->
            handle_post_do(Path, Map);
        _ ->
            #{code => ?HTTP_CODE_FAIL, data => <<>>, msg => <<"HTTP json decode fail!">>}
    end.
%% @doc node
handle_post_do(<<"/node">>, _Map) ->
    #{code => ?HTTP_CODE_SUCCESS, data => #{node => node()}, msg => <<>>};
handle_post_do(_Path, _Map) ->
    #{code => ?HTTP_CODE_FAIL, msg => <<"error path">>}.

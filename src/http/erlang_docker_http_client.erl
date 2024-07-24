%%%-------------------------------------------------------------------
%%% @doc
%%% http 客户端模块
%%% @end
%%%-------------------------------------------------------------------
-module(erlang_docker_http_client).

-include("erlang_docker.hrl").

%% API
-export([
    ping/1,
    node/1,
    request/4
]).

%% @doc ping pong
ping(Url) ->
    request(?HTTP_METHOD_GET, Url, "/ping", #{}).

%% @doc 获取节点
node(Url) ->
    request(?HTTP_METHOD_POST, Url, "/node", #{}).

%% @doc http 请求
request(Method, Url, Router, Body) ->
    NewUrl = Url ++ Router,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    BodyBin = jsx:encode(Body),
    % {pool, false}: 禁用连接池确保每次请求都不复用连接, 否则复用会导致无法负载均衡
    Options = [{pool, false}, {recv_timeout, 5000}],
    case catch hackney:request(Method, NewUrl, Headers, BodyBin, Options) of
        {ok, 200, _, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RetBodyBin} ->
                    % 显式关闭连接
                    hackney:close(ClientRef),
                    case catch erlang_docker_misc:json_decode(RetBodyBin) of
                        #{code := 0, data := Data} ->
                            {ok, Data};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

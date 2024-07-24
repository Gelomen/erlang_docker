%%%-------------------------------------------------------------------
%%% @doc
%%% rpc
%%% @end
%%%-------------------------------------------------------------------
-module(erlang_docker_rpc).

-include("erlang_docker.hrl").

%% API
-export([
    call/3,
    rpc_node/0,
    rpc_node/1
]).

call(Mod, Fun, Args) ->
    case rpc_node() of
        {ok, Node} ->
            rpc:call(Node, Mod, Fun, Args);
        Error ->
            Error
    end.

rpc_node() ->
    case node_type() of
        {ok, "a" ++ _Rest} -> rpc_node(b);
        {ok, "b" ++ _Rest} -> rpc_node(a);
        {ok, Term} -> {node_type_error, Term};
        Error -> Error
    end.
rpc_node(a) -> request_node(?NODE_A_SVC_DOMAIN);
rpc_node(b) -> request_node(?NODE_B_SVC_DOMAIN).

node_type() ->
    case string:tokens(atom_to_list(node()), "@") of
        [Name, _] ->
            case string:tokens(Name, "-") of
                List when length(List) > 0 ->
                    {ok, lists:last(List)};
                _ ->
                    node_type_error
            end;
        _ ->
            node_type_error
    end.

%% @doc 获取请求的节点
request_node(Url) ->
    case erlang_docker_http_client:node(Url) of
        {ok, #{node := NodeBinary}} ->
            {ok, binary_to_atom(NodeBinary)};
        _ ->
            get_node_error
    end.

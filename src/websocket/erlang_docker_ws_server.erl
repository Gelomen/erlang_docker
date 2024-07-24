%%%-------------------------------------------------------------------
%%% @doc
%%% websocket 服务模块
%%% @end
%%%-------------------------------------------------------------------
-module(erlang_docker_ws_server).

-include("erlang_docker.hrl").

%% API
-export([
    start/0,
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

%% @doc 定义路由，启动 ws 服务
start() ->
    Handler = ?MODULE,
    Port = ?WS_PORT,
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erlang_docker, "index.html"}},
            {"/websocket", Handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, erlang_docker, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(ws, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    ok.

%% @doc 初始化
init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    {[], State}.

websocket_handle({text, <<"node">>}, State) ->
    {[{text, erlang:atom_to_binary(node())}], State};
websocket_handle({text, <<"mfa ", MFABinary/binary>>}, State) ->
    Return = mfa(false, MFABinary),
    {[{text, erlang:list_to_bitstring(io_lib:format("~p", [Return]))}], State};
websocket_handle({text, <<"rpc ", MFABinary/binary>>}, State) ->
    Return = mfa(true, MFABinary),
    {[{text, erlang:list_to_bitstring(io_lib:format("~p", [Return]))}], State};
websocket_handle({text, Msg}, State) ->
    {[{text, Msg}], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info(_Info, State) ->
    {[], State}.

mfa(IsRPC, MFABinary) ->
    try
        [ModStr, FunArgsStr] = catch string:tokens(binary_to_list(MFABinary), ":"),
        {Module, Function, Arguments} =
            case string:tokens(FunArgsStr, "()") of
                [FunStr] ->
                    Mod = erlang:list_to_atom(ModStr),
                    Fun = erlang:list_to_atom(FunStr),
                    {Mod, Fun, []};
                [FunStr, ArgsStr] ->
                    {ok, Tokens, _} = erl_scan:string("[" ++ ArgsStr ++ "]."),
                    {ok, Args} = erl_parse:parse_term(Tokens),
                    Mod = erlang:list_to_atom(ModStr),
                    Fun = erlang:list_to_atom(FunStr),
                    {Mod, Fun, Args}
            end,
        case IsRPC of
            true -> erlang_docker_rpc:call(Module, Function, Arguments);
            _ -> erlang:apply(Module, Function, Arguments)
        end
    catch
        Exception:Reason:Stacktrace -> {Exception, Reason, Stacktrace}
    end.

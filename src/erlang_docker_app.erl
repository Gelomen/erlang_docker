%%%-------------------------------------------------------------------
%% @doc erlang_docker public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_docker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case erlang_docker_sup:start_link() of
        {ok, Pid} ->
            erlang_docker_http_server:start(),
            erlang_docker_ws_server:start(),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%% internal functions

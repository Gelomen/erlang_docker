%%%-------------------------------------------------------------------
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(erlang_docker_misc).

-include("erlang_docker.hrl").

%% API
-export([
    json_decode/1
]).

json_decode(Json) ->
    jsx:decode(Json, [{labels, atom}, return_maps]).

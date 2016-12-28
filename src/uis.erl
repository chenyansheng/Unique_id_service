%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2016-12-24
%%% @doc application start | stop
%%%-----------------------------------------------
-module(uis).
-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

-define(APP_NAME, uis).

%% @doc start service
start() ->
    try
        ok = application:start(?APP_NAME)
    catch
        Type:Error ->
            io:format("start app fail： ~p, ~p", [Type, Error]),
            init:stop(1)
    end.

%% @doc stop service
stop() ->
    application:stop(?APP_NAME),
    erlang:halt(0, [{flush, false}]).


%%---------------------
%% callback
%%---------------------
%% @doc callback start
start(_Type, StartArgs) ->
    {ok, Sup} = sup:start_link(StartArgs),
    {ok, Sup}.

%% @doc callback stop
stop(_State) ->
    ok.

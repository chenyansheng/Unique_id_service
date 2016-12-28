%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2016-12-24
%%% @doc supervisor
%%%-----------------------------------------------
-module(sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

%% @doc start link
start_link(_Args) ->
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    Child = {service_id, {service_id, start_link, []},
                                    permanent, 5000, worker,[service_id]},
    case catch supervisor:start_child(Sup, Child) of
        {ok, _} ->
            ok;
        {error, {{already_started, _Pid}, _}} ->
            ok;
        Other ->
            io:format("supervisor start child faild: ~p", [Other]),
            throw(Other)
    end,
    {ok, Sup}.

%% @doc init
init([]) ->
    {ok,{{one_for_one, 10, 10}, []}}.

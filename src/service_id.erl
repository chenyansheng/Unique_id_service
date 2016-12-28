%%%-----------------------------------------------
%%% @author chenyansheng
%%% @date 2016-12-24
%%% @doc Unique id Service
%%%-----------------------------------------------
-module(service_id).

-export([start_link/0, get_state/0, info/0, get/0, inc/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([do_save_data/0]).

%% ets table
-define(TABLE, service_id_tab).
%% state
-record(state, {}).

%%-------------------
%% public fun
%%-------------------
%% @doc 启动
start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 获取状态
get_state() ->
    call(get_state).

info() ->
    ets:info(?TABLE).

%% @doc 获取当前值
get() ->
    ets:lookup_element(?TABLE, "id", 2).

%% @doc 加1
inc() ->
    Id = ets:update_counter(?TABLE, "id", 1),
    {ok, Id}.

%%--------------------
%% callback fun
%%--------------------
init(_Type) ->
    erlang:process_flag(trap_exit, true),
    ok = do_init_table(),
    ok = do_load_data(),
    {ok, #state{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = do_save_data(),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%----------------------
%% private fun
%%----------------------
%% @doc call调用
call(Req) ->
    gen_server:call(?MODULE, Req, 5000).

%% @doc 初始化table
do_init_table() ->
   ?TABLE = ets:new(?TABLE, [set, public, named_table, 
                              {keypos, 1}, {read_concurrency, true},
                              {write_concurrency, true}]),
    ok.

%% @doc 加载数据
do_load_data() ->
    File = do_get_file_name(),
    X =
    case filelib:is_file(File) of
        true ->
            {ok, Bin} = file:read_file(File),
            erlang:binary_to_integer(Bin);
        false ->
            0
    end,
    ets:insert(?TABLE, {"id", X}),
    ok.

%% @doc 保存数据
do_save_data() ->
    X = ets:lookup_element(?TABLE, "id", 2),
    File = do_get_file_name(),
    Bin = erlang:integer_to_binary(X),
    ok = file:write_file(File, Bin).

%% @doc 获取保存的文件名
do_get_file_name() ->
    {ok, Dir} = file:get_cwd(),
    lists:concat([Dir, "/log/info.txt"]).

%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <starinkin@gmail.com>
%%% @doc
%%% Element, that store values in cache application
%%% from book Erlang and OTP in Action
%%% @end
%%% Created :  4 Sep 2013 by Grigory Starinkin <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(cache_element).

-behaviour(gen_server).

%% API
-export([start_link/2,
	create/1,
	create/2,
	fetch/1,
	replace/2,
	delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%%--------------------------------------------------------------------
%% @doc Create new child with given Value and LeaseTime
%% @spec create(Value::term(), LeaseTime::integer() | 'infinity') ->
%%                                       supervisor:startchild_ret()
%% @end
%%--------------------------------------------------------------------
create(Value, LeaseTime) ->
    cache_sup:start_child(Value, LeaseTime).

%%--------------------------------------------------------------------
%% @doc Create new child with given Value and defalut lease time
%% @spec create(Value::term()) -> supervisor:startchild_ret()
%% @end
%%--------------------------------------------------------------------
create(Value) ->
    cache_sup:start_child(Value, ?DEFAULT_LEASE_TIME).

%%--------------------------------------------------------------------
%% @doc Get value from process with given Pid
%% @spec fetch(Pid::pid()) -> term()
%% @end
%%--------------------------------------------------------------------
fetch(Pid) ->
    gen_server:call(Pid, fetch).

%%--------------------------------------------------------------------
%% @doc Replace Value in given Pid process
%% @spec replace(Pid::pid(), Value::term()) -> ok
%% @end
%%--------------------------------------------------------------------
replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

%%--------------------------------------------------------------------
%% @doc Delete given process
%% @spec delete(Pid::pid()) -> ok
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    gen_server:cast(Pid, delete).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([Value::term(), LeaseTime::integer()]) -> {ok, State} |
%%                                             {ok, State, Timeout} |
%%                                                           ignore |
%%                                                   {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok,
     #state{value = Value,
            lease_time = LeaseTime,
            start_time = StartTime},
     time_left(StartTime, LeaseTime)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(fetch, _From, State) ->
    #state{value = Value,
	   lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime,
           start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    cache_store:delete(self()),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Get element remain time from given StartTime and LeaseTime
%% @spec time_left(_StartTime::any(), 'inifinity') -> 'inifinity';
%%       time_left(StartTime::integer(), LeaseTime::integer()) -> integer()
%% @end
%%--------------------------------------------------------------------
time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
         Time when Time =< 0 -> 0;
         Time                -> Time * 1000
    end.

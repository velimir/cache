%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <starinkin@gmail.com>
%%% @doc
%%% Supervisor module for cache elements
%%% from book Erlang and OTP in Action
%%% @end
%%% Created :  4 Sep 2013 by Grigory Starinkin <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(cache_element_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> supervisor:startlink_ret()
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start child
%% @spec start_child(Value::term(), LeaseTime::term()) -> supervisor:startchild_ret()
%% @end
%%--------------------------------------------------------------------
start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    CacheElement = {cache_elements, 
                    {cache_element, start_link, []},
                    temporary, brutal_kill, worker, [cache_element]},
    Children = [CacheElement],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

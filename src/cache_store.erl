%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <starinkin@gmail.com>
%%% @doc
%%% Store module, which stores Key -> Pid pairs
%%% from book Erlang and OTP in Action
%%% @end
%%% Created :  4 Sep 2013 by Grigory Starinkin <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(cache_store).

%% API
-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1
        ]).

-define(TABLE_ID, ?MODULE).

-record(key_to_pid, {key, pid}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create new cache_store instance
%% @spec init() -> ok
%% @end
%%--------------------------------------------------------------------
init() ->
    mnesia:start(),
    mnesia:create_table(key_to_pid,
                        [{index, [pid]},
                         {attributes, record_info(fields, key_to_pid)}]).


%%--------------------------------------------------------------------
%% @doc Insert Key, Pid pair to cache_store
%% @spec insert(Key::term(), Pid::pid()) -> ok
%% @end
%%--------------------------------------------------------------------
insert(Key, Pid) ->
    mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

%%--------------------------------------------------------------------
%% @doc Try to look up Pid for given Key
%% @spec lookup(Key::term()) -> {ok, Pid::pid()} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    case mnesia:dirty_read(key_to_pid, Key) of
        [{key_to_pid, Key, Pid}] ->
            case is_pid_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.


%%--------------------------------------------------------------------
%% @doc Delete pair with Pid from cache_store
%% @spec delete(Pid::pid()) -> true
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
         [#key_to_pid{} = Record] ->
              mnesia:dirty_delete_object(Record);
         _ ->
              ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checking whether the pid is a live
%%
%% @spec is_pid_alive(Pid::node()) -> true | false
%%
%% @end
%%--------------------------------------------------------------------
is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    lists:member(node(Pid), nodes()) andalso
    (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).

%%%---------------------------------------------------------------------------
%% UNIT TESTS
%%%---------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_read_write_test_() ->
    {spawn,
      [
       fun() ->
	       init(),
               SelfPid = self(),
	       insert(moscow, SelfPid),
	       {ok, SelfPid} = lookup(moscow)
       end
      ]
     }.

is_pid_alive_test_() ->
    [
     ?_assertEqual(is_pid_alive(self()), true),
     ?_assertEqual(is_pid_alive(list_to_pid("<0.12332.0>")), false),

     {setup,
      fun() ->
              spawn(?MODULE, init, [])
      end,
      fun(Pid) ->
              [
               ?_assert(is_pid_alive(Pid))
              ]
      end
     }
    ].

simple_delete_test_() ->
    {spawn,
     [
      fun() ->
	      init(),

              SelfPid = self(),
	      insert(moscow, SelfPid),
	      {ok, SelfPid} = lookup(moscow),

              FakePid = list_to_pid("<0.10201.0>"),
	      insert(washington, FakePid),
	      {error, not_found} = lookup(washington),

	      delete(SelfPid),
	      {error, not_found} = lookup(moscow)
      end
     ]
    }.

-endif.

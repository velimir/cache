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


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create new cache_store instance
%% @spec init() -> ok
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @doc Insert Key, Pid pair to cache_store
%% @spec insert(Key::term(), Pid::pid()) -> ok
%% @end
%%--------------------------------------------------------------------
insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

%%--------------------------------------------------------------------
%% @doc Try to look up Pid for given Key
%% @spec lookup(Key::term()) -> {ok, Pid::pid()} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
         [{Key, Pid}] -> {ok, Pid};
         []           -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Delete pair with Pid from cache_store
%% @spec delete(Pid::pid()) -> true
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).

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
	       insert(moscow, russia),
	       {ok, russia} = lookup(moscow)
       end
      ]
     }.

simple_delete_test() ->
    {spawn,
     [
      fun() ->
	      init(),

	      insert(moscow, russia),
	      {ok, russia} = lookup(moscow),

	      insert(washington, 'USA'),
	      {ok, 'USA'} = lookup(washington),

	      delete(russia),
	      {error, not_found} = lookup(moscow),

	      {ok, 'USA'} = lookup(washington)
      end
     ]
    }.

-endif.

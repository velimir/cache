%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <starinkin@gmail.com>
%%% @doc
%%% API module for control simple cache system
%%% from Erlang and OTP in Action
%%% @end
%%% Created :  4 Sep 2013 by Grigory Starinkin <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(simple_cache).

%% API
-export([insert/2,
	 insert/3,
	 lookup/1,
	 delete/1]).

-include("simple_cache.hrl").

%%--------------------------------------------------------------------
%% @doc Insert {Key, Value} pair to cache with given LeaseTime
%% @spec insert(Key::term(), Value::term(), LeaseTime::integer()) -> ok
%% @end
%%--------------------------------------------------------------------
insert(Key, Value, LeaseTime) ->
    case cache_store:lookup(Key) of
        {ok, Pid} ->
            cache_element:replace(Pid, Value),
            cache_element:replace(Key, Value);
        {error, _} ->
            {ok, Pid} = cache_element:create(Value, LeaseTime),
            cache_store:insert(Key, Pid),
            cache_event:create(Key, Value)
    end.

%%--------------------------------------------------------------------
%% @doc Insert {Key, Value} pair to cache with default lease time
%% @spec insert(Key::term(), Value::term()) -> ok
%% @end
%%--------------------------------------------------------------------
insert(Key, Value) ->
    insert(Key, Value, ?DEFAULT_LEASE_TIME).

%%--------------------------------------------------------------------
%% @doc Look up an Value in cache
%% @spec lookup(Key::term()) -> {ok, Value::term()} |
%%                               {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    try
        cache_event:lookup(Key),
        {ok, Pid} = cache_store:lookup(Key),
        {ok, Value} = cache_element:fetch(Pid),
        {ok, Value}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Delete Key from cache
%% @spec delete(Key::term()) -> ok
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
    cache_event:delete(Key),
    case cache_store:lookup(Key) of
         {ok, Pid} ->
             cache_element:delete(Pid);
         {error, _Reason} ->
             ok
    end.

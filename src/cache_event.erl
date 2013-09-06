%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <starinkin@gmail.com>
%%% @doc
%%% Event manager for simple cache module
%%% from Erlang and OTP in Action
%%% @end
%%% Created :  4 Sep 2013 by Grigory Starinkin <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(cache_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         lookup/1,
         create/2,
         replace/2,
         delete/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @doc Start cache event
%% @spec start_link() -> ok
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc Add handler
%% @spec add_handler(gen_event:handler(), term()) -> term()
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc Delete handler
%% @spec delete_handler(gen_event:handler(), term()) -> term()
%% @end
%%--------------------------------------------------------------------
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc Notify about looking up a Key in cache
%% @spec lookup(term()) -> ok
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    gen_event:notify(?SERVER, {lookup, Key}).

%%--------------------------------------------------------------------
%% @doc Notify about creating pair {Key, Value} in cache
%% @spec create(term(), term()) -> ok
%% @end
%%--------------------------------------------------------------------
create(Key, Value) ->
    gen_event:notify(?SERVER, {create, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc Notify about replacing Value part of pair {Key, '_'} in cache
%% @spec replace(term(), term()) -> ok
%% @end
%%--------------------------------------------------------------------
replace(Key, Value) ->
    gen_event:notify(?SERVER, {replace, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc Notify about deleting from cache
%% @spec delete(term()) -> ok
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
    gen_event:notify(?SERVER, {delete, Key}).

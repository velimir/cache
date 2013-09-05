%%%-------------------------------------------------------------------
%%% @author Grigory Starinkin <starinkin@gmail.com>
%%% @doc
%%% Test suite for simple cache system
%%% @end
%%% Created :  4 Sep 2013 by Grigory Starinkin <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(cache_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% @doc
%% Initialization before the suite.
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(simple_cache),
    Config.

end_per_suite(Config) ->
    application:stop(simple_cache),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the suite.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------

all() -> 
    [
     simple_test_case,
     cache_with_custom_lease_time,
     read_write_case
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

simple_test_case(_Config) -> 
    simple_cache:insert(india,  new_delhi),
    {ok, new_delhi} = simple_cache:lookup(india),
    ok.

cache_with_custom_lease_time(_Config) ->
    simple_cache:insert(russia, moscow, 1),
    simple_cache:insert('USA', washington),
    timer:sleep(2000),
    {error, not_found} = simple_cache:lookup(russia),
    {ok, washington} = simple_cache:lookup('USA'),
    ok.

read_write_case(_Config) ->
    simple_cache:insert(russia, moscow),
    simple_cache:insert('USA', washington),

    simple_cache:delete(russia),

    {ok, washington} = simple_cache:lookup('USA'),
    {error, not_found} = simple_cache:lookup(russia),

    ok.
    

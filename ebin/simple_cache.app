{application,simple_cache,
             [{description,"Simple cache"},
              {modules,[cache_SUITE,cache_app,cache_element,cache_store,
                        cache_sup,simple_cache]},
              {vsn,"1"},
              {registered,[cache_sup]},
              {applications,[kernel,stdlib]},
              {mod,{cache_app,[]}},
              {env,[]}]}.

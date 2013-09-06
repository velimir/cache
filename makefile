.PHONY: all clean distclean doc test

REBAR ?= rebar

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info
	rm -rf .eunit

doc:
	$(REBAR) doc

test:
	ct_run -spec ct.spec -pa ./ebin/

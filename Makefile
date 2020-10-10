
smoke-test: build
	@for i in samples/*.erl; do echo "# $$i"; ./_build/default/bin/efe ann $$i;./_build/default/bin/efe pp $$i; done

build:
	rebar3 escriptize



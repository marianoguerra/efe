
smoke-test: build
	@for i in samples/*.erl; do echo "# $$i"; ./_build/default/bin/efe ann $$i;./_build/default/bin/efe pp $$i; done

build:
	rebar3 escriptize

otp-test: build
	@for i in ~/.asdf/installs/erlang/23.1.1/lib/*/src/*.erl; do echo "# $$i";./_build/default/bin/efe pp $$i; done

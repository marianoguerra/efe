
smoke-test: build
	@for i in samples/*.erl; do echo "# $$i"; ./_build/default/bin/efe pp $$i; done

build:
	rebar3 escriptize
	cp _build/default/bin/efe .

otp-test: build
	@for i in otplib/*/src/*.erl; do echo "# $$i";./_build/default/bin/efe pp $$i > $$(echo out/`basename $$i .erl`.ex) ; done

otp-compile: build
	@cd out && for f in *.ex; do echo "# $$f" >> result.txt; elixirc $$f >> result.txt; done

format-output:
	@cd out && for f in *.ex; do mix format $$f; done

filter-result:
	@grep -v '^# ' out/result.txt | grep -v '== Compilation error in file' | grep -v '(elixir ' | grep -v '(stdlib 3' | sed 's/** (CompileError) //;s/** (SyntaxError) //'


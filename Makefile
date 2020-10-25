
smoke-test: build
	./efe pp samples.conf samples/*.erl

build:
	rebar3 escriptize
	cp _build/default/bin/efe .

otp-test: build
	./efe pp otp.conf otplib/*/src/*.erl

otp-compile: build
	@cd out && for f in **/*.ex; do echo "# $$f" >> result.txt; elixirc $$f >> result.txt; done

format-output:
	@cd out && for f in **/*.ex; do mix format $$f; done

filter-result:
	@grep -v '^# ' out/result.txt | grep -v '== Compilation error in file' | grep -v '(elixir ' | grep -v '(stdlib 3' | sed 's/** (CompileError) //;s/** (SyntaxError) //'

conf-test: build
	./efe conf otp.conf otplib/stdlib/src/lists.erl otplib/stdlib/src/array.erl

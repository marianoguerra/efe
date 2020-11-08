
smoke-test: build
	./efe pp samples.conf samples/*.erl

build:
	rebar3 escriptize
	cp _build/default/bin/efe .

translate-otp: build
	./efe pp otp.conf otp/lib/*/src/*.erl otp/erts/preloaded/src/*.erl

compile-output: build
	./run.bash compile-output

format-output:
	./run.bash format-output

filter-result:
	@grep -v '^# ' out/result.txt | grep -v '== Compilation error in file' | grep -v '(elixir ' | grep -v '(stdlib 3' | sed 's/** (CompileError) //;s/** (SyntaxError) //'

conf-test: build
	./efe conf otp.conf otplib/stdlib/src/lists.erl otplib/stdlib/src/array.erl

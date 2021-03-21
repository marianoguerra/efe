
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

otp:
	git clone https://github.com/erlang/otp.git
	cd otp && git checkout OTP-24.0-rc1

clean-inception:
	rm -rf efex out-erl out-ex out
inception: clean-inception build otp translate-otp
	mv out out-erl
	mix new efex
	rm efex/lib/efex.ex
	./efe pp efe.conf src/*.erl
	mv efex/lib/src/*.ex efex/lib/
	rmdir efex/lib/src
	sed -i 's/deps()/deps(),\n      escript: [main_module: :efe, name: "efe"]/g' efex/mix.exs
	cd efex && mix escript.build
	./efex/efe pp otp.conf otp/lib/*/src/*.erl otp/erts/preloaded/src/*.erl
	mv out out-ex
	diff -r out-erl out-ex

conf-test: build
	./efe conf otp.conf otplib/stdlib/src/lists.erl otplib/stdlib/src/array.erl

cons-test: build
	./efe pp samples.conf samples/cons.erl
	@cat samples/cons.erl
	@cat out/samples/cons.ex


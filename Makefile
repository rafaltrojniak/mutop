rebar=rebar

CFLAGS=
CXXFLAGS=

all:get-deps compile

compile:
	${rebar} compile

clean:
	${rebar} clean

get-deps: deps/cecho

deps/cecho :
	${rebar} get-deps

run: all
	@./run

all: 
	rebar compile generate

init:
	rebar get-deps update-deps

clean:
	rebar clean
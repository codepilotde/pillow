all: 
	rebar update-deps compile generate

init:
	rebar get-deps

clean:
	rebar clean
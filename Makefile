all: 
	erlc +debug_info -o ebin src/*.erl;

clean:
	rm -rf ebin/*.beam;
	rm -rf erl_crush.dump
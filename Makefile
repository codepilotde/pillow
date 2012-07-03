all: 
	erlc +native +debug_info -o ebin src/*.erl;

clean:
	rm -rf ebin/*.beam;
	rm -rf ebin/*.dump
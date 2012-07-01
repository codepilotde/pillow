for f in src/*.erl ; do erlc +debug_info -o ebin $f; done

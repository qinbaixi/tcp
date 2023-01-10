cd ..

erl +P 1024000 -sname tcp_node -pa ebin -pa ebin_deps -s tcp_app start -s reloader start
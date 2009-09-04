app: compile ebin/dhcp.app

check: compile
	dialyzer -c ebin

ebin/dhcp.app: src/dhcp.app
	mkdir -p ebin
	cp src/dhcp.app ebin/

compile:
	mkdir -p ebin
	erl -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.' -noshell

clean:
	rm -rf ebin/* erl_crash.dump

spy: app
	sudo erl -pa ebin -eval 'dhcp_spy:start_link().'

server: app
	sudo erl -pa ebin -eval 'dhcp_server:start_link().'

.PHONY: compile check app

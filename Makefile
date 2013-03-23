REBAR=./rebar

.PHONY: all

all:
	-rm c_src/.#  
	$(REBAR) compile

clean:
	$(REBAR) clean

console: all
	erl -smp disable -pa ebin

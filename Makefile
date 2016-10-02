all:
	@erlc *.erl

test:
	@make paper
	@make csp2pn
	@make web



csp2pn:
	@echo "*************************************" 
	@echo "*********** Test CSP2PN *************"
	@echo "*************************************" 
	@erl -run pn_suite main examples/pn_CSP2PN.xml  -noshell -s erlang halt

paper:
	@echo "*************************************" 
	@echo "************ Test paper *************"
	@echo "*************************************" 
	@erl -run pn_suite main examples/pn_paper.xml  -noshell -s erlang halt

web:
	@echo "*************************************" 
	@echo "*********** Test external ***********"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/pn_web.pnml  -noshell -s erlang halt 

compile:
	@erlc *.erl

all:
	@make paper
	@make csp2pn
	@make web
	@make error
	@make rakow1
	@make rakow2

test1:
	@erl -run pn_suite main PT/afcs_06_a.pnml  -noshell -s erlang halt

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

error:
	@echo "*************************************" 
	@echo "************ Test error *************"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/pn_error.xml  -noshell -s erlang halt

rakow5:
	@echo "*************************************" 
	@echo "************ Test rakow5 ************"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/pn_rakowp5.xml  -noshell -s erlang halt  


rakow6:
	@echo "*************************************" 
	@echo "************ Test rakow6 ************"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/pn_rakowp6.xml  -noshell -s erlang halt  


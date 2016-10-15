compile:
	@erlc *.erl

all:
	@make paper
	@make csp2pn
	@make web
	@make yu1
	@make yu2
	@make rakow1
	@make rakow2
	@make loop

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

yu1:
	@echo "*************************************" 
	@echo "********** Test Yu et al 1 **********"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/pn_yuetal1.xml  -noshell -s erlang halt

yu2:
	@echo "*************************************" 
	@echo "********** Test Yu et al 2 **********"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/pn_yuetal2.xml  -noshell -s erlang halt

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

loop:
	@echo "*************************************" 
	@echo "************* Test loop *************"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/loop.xml  -noshell -s erlang halt

bb:
	@echo "*************************************" 
	@echo "************** Test bb **************"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/bif_backwards.xml  -noshell -s erlang halt 

bf:
	@echo "*************************************" 
	@echo "************** Test bf **************"
	@echo "*************************************" 	
	@erl -run pn_suite main examples/bif_forwards.xml  -noshell -s erlang halt 

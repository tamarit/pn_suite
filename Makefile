ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))


compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -o ebin src/*.erl 

clean:
	@rm -Rf ebin
	@rm -f *.txt
	@find examples -name 'output' -prune -exec rm -fr {} \;
	@find . -name '*.dump' -prune -exec rm -fr {} \;

install:
	@erl -pa ebin -run make_script from_path $(ROOT_DIR)  -noshell -s erlang halt
	@chmod +x pn_suite_temp
	mv -f pn_suite_temp $(ERLC_PATH)/pn_suite

bench:
	@erl -pa ebin -run pn_bench bench -noshell -s erlang halt


# ***********************************
# ************** Tests **************
# ***********************************

all:
	@make paper
	@make csp2pn
	@make web
	@make yu1
	@make yu2
	@make rakow1
	@make rakow2
	@make loop
	@make bf
	@make bb

csp2pn:
	@echo "*************************************" 
	@echo "*********** Test CSP2PN *************"
	@echo "*************************************" 
	@./pn_suite examples/other/pn_CSP2PN.xml

paper:
	@echo "*************************************" 
	@echo "************ Test paper *************"
	@echo "*************************************" 
	@./pn_suite examples/other/pn_paper.xml

web:
	@echo "*************************************" 
	@echo "*********** Test external ***********"
	@echo "*************************************" 	
	@./pn_suite examples/other/pn_web.pnml

yu1:
	@echo "*************************************" 
	@echo "********** Test Yu et al 1 **********"
	@echo "*************************************" 	
	@./pn_suite examples/other/pn_yuetal1.xml

yu2:
	@echo "*************************************" 
	@echo "********** Test Yu et al 2 **********"
	@echo "*************************************" 	
	@./pn_suite examples/other/pn_yuetal2.xml

rakow1:
	@echo "*************************************" 
	@echo "************ Test rakow5 ************"
	@echo "*************************************" 	
	@./pn_suite examples/other/pn_rakowp5.xml


rakow2:
	@echo "*************************************" 
	@echo "************ Test rakow6 ************"
	@echo "*************************************" 	
	@./pn_suite examples/other/pn_rakowp6.xml

loop:
	@echo "*************************************" 
	@echo "************* Test loop *************"
	@echo "*************************************" 	
	@./pn_suite examples/other/loop.xml

bb:
	@echo "*************************************" 
	@echo "************** Test bb **************"
	@echo "*************************************" 	
	@./pn_suite examples/other/bif_backwards.xml

bf:
	@echo "*************************************" 
	@echo "************** Test bf **************"
	@echo "*************************************" 	
	@./pn_suite examples/other/bif_forwards.xml


p1:
	@echo "*************************************" 
	@echo "*********** Test problem1 ***********"
	@echo "*************************************" 	
	@./pn_suite examples/other/problem1.xml

i1:
	@echo "*************************************" 
	@echo "*********** Test improve1 ***********"
	@echo "*************************************" 	
	@./pn_suite examples/other/improve1.xml 
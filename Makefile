ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -o ebin src/*.erl 

clean:
	@rm -Rf ebin
	@rm -f *.dump
	@rm -Rf examples/output

install:
	@erl -pa ebin -run make_script from_path $(ROOT_DIR)  -noshell -s erlang halt
	@chmod +x pn_suite_temp
	@mv -f pn_suite_temp /usr/local/bin/pn_suite


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
	@./pn_suite examples/pn_CSP2PN.xml

paper:
	@echo "*************************************" 
	@echo "************ Test paper *************"
	@echo "*************************************" 
	@./pn_suite examples/pn_paper.xml

web:
	@echo "*************************************" 
	@echo "*********** Test external ***********"
	@echo "*************************************" 	
	@./pn_suite examples/pn_web.pnml

yu1:
	@echo "*************************************" 
	@echo "********** Test Yu et al 1 **********"
	@echo "*************************************" 	
	@./pn_suite examples/pn_yuetal1.xml

yu2:
	@echo "*************************************" 
	@echo "********** Test Yu et al 2 **********"
	@echo "*************************************" 	
	@./pn_suite examples/pn_yuetal2.xml

rakow1:
	@echo "*************************************" 
	@echo "************ Test rakow5 ************"
	@echo "*************************************" 	
	@./pn_suite examples/pn_rakowp5.xml


rakow2:
	@echo "*************************************" 
	@echo "************ Test rakow6 ************"
	@echo "*************************************" 	
	@./pn_suite examples/pn_rakowp6.xml

loop:
	@echo "*************************************" 
	@echo "************* Test loop *************"
	@echo "*************************************" 	
	@./pn_suite examples/loop.xml

bb:
	@echo "*************************************" 
	@echo "************** Test bb **************"
	@echo "*************************************" 	
	@./pn_suite examples/bif_backwards.xml

bf:
	@echo "*************************************" 
	@echo "************** Test bf **************"
	@echo "*************************************" 	
	@./pn_suite examples/bif_forwards.xml


p1:
	@echo "*************************************" 
	@echo "*********** Test problem1 ***********"
	@echo "*************************************" 	
	@./pn_suite examples/problem1.xml

i1:
	@echo "*************************************" 
	@echo "*********** Test improve1 ***********"
	@echo "*************************************" 	
	@./pn_suite examples/improve1.xml 
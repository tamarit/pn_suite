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
	@make script_install FILE=pn_suite
	@make script_install FILE=pn_slice
	@make script_install FILE=pn_prop

script_install:
	@erl -pa ebin -run make_script from_path $(ROOT_DIR) $(FILE) -noshell -s erlang halt
	@chmod +x "$(FILE)_temp"
	@mv -f "$(FILE)_temp" $(ERLC_PATH)/$(FILE)


select:
	@erl -pa ebin -run pn_select_html create -noshell -s erlang halt

# ***********************************
# *********** Benchmarking **********
# ***********************************


bench:
	@erl -pa ebin -run pn_bench bench -noshell -s erlang halt

bench_all:
	@make bench_2011 > 2011_res
	@make bench_2012 > 2012_res
	@make bench_2013 > 2013_res
	@make bench_2014 > 2014_res
	@make bench_2015 > 2015_res
	@make bench_2016 > 2016_res

bench_prop:
	@erl -pa ebin -run pn_bench_prop bench -noshell -s erlang halt

bench_2011:
	@erl -pa ebin -run pn_bench_prop bench 2011 -noshell -s erlang halt

bench_2012:
	@erl -pa ebin -run pn_bench_prop bench 2012 -noshell -s erlang halt

bench_2013:
	@erl -pa ebin -run pn_bench_prop bench 2013 -noshell -s erlang halt

bench_2014:
	@erl -pa ebin -run pn_bench_prop bench 2014 -noshell -s erlang halt

bench_2015:
	@erl -pa ebin -run pn_bench_prop bench 2015 -noshell -s erlang halt

bench_2016:
	@erl -pa ebin -run pn_bench_prop bench 2016 -noshell -s erlang halt

print:
	@erl -pa ebin -run pn_results_printer print -noshell -s erlang halt


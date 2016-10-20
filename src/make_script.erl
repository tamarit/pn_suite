-module(make_script).

-export([from_path/1]).

from_path(Path) ->
	Lines = 
		["#!/bin/bash"
		,""
		,"if [ $# != 1 ]"
		,"then" 
		,"\techo -e \"Usage:\n\tpn_suite PNML_FILE\""
		,"else"
		,"\terl -pa " ++ Path ++ "/ebin -run pn_suite main $1  -noshell -s erlang halt"
		,"fi"],
	   file:write_file("pn_suite_temp",  
        	list_to_binary(string:join(Lines, "\n"))).




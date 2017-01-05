-module(make_script).

-export([from_path/1]).

from_path([Path, Script]) ->
	ScriptLines = 
		pn_input:read_file_lines(Script),
	NewScriptLines = 
		substitute_path(ScriptLines, Path),
	file:write_file(Script ++ "_temp",  
        list_to_binary(string:join(NewScriptLines, "\n"))).

substitute_path([[$p,$a,$t,$h,$= | _] | T], Path) ->
	["path=\"" ++ Path ++ "\"" | T];
substitute_path([Other | T], Path) ->
	[Other | substitute_path(T, Path)];
substitute_path([], _) ->
	[].



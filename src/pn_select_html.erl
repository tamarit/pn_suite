-module( pn_select_html ).
 
-export( [create/0] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create select options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create() ->
    Directories = 
        [
            "other"
            "mcc_models/2011/FMS",
            "mcc_models/2011/Kanban",
            "mcc_models/2011/MAPK",
            "mcc_models/2011/Peterson",
            "mcc_models/2011/Philosophers",
            "mcc_models/2011/TokenRing"
        ],
    lists:map(fun create_options_dir/1, Directories).

create_options_dir(Dir) ->
    % io:format("examples/" ++ Dir ++ "\n"),
    {ok, FileList} = 
        file:list_dir("examples/" ++ Dir),
    NetFiles = 
        [ Dir ++ "/" ++ File
        ||  File <- FileList,
            length(File) >= 5,
            (string:substr(File, length(File) - 3, 4) == ".xml") or 
            (string:substr(File, length(File) - 4, 5) == ".pnml")],
    lists:map(fun create_option_file/1, NetFiles).

create_option_file(File) ->
    Opt = 
            "</option><option value=\"https://raw.githubusercontent.com/tamarit/pn_suite/master/examples/" 
        ++  File 
        ++  "\">" 
        ++  File 
        ++  "</option>\n",
    io:format(Opt).

-module( pn_bench ).
 
-export( [bench/0] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bench() ->
    Directories = 
        [
            % "examples/mcc_models/2011/FMS",
            "examples/other"
        ],
    Timeout = 
        1000,
    SlicesPerNet = 
        10,
    MaxSC = 
        5,
    start_bench(Directories, Timeout, SlicesPerNet, MaxSC).

algorithms() -> 
    [
        #slicer{
            name = "LLorens et al's slicer", 
            function = fun pn_slice:slice/2},
        #slicer{
            name = "LLorens et al's slicer improved", 
            function = fun pn_slice:slice_imp/2},
        #slicer{
            name = "Rakow's slicer CTL", 
            function = fun pn_rakow:slice_ctl/2},
        #slicer{
            name = "Rakow's slicer safety", 
            function = fun pn_rakow:slice_safety/2},
        #slicer{
            name = "Yu et al's slicer", 
            function = fun pn_yuetal:slice/2}
    ].

start_bench(Directories, Timeout, SlicesPerNet, MaxSC) ->
    {{Yea, Mon, Day}, {Hou, Min, Sec}} = 
        calendar:local_time(),
    TimeStr = 
        string:join(
            lists:map(
                fun(I) -> 
                    IStr = integer_to_list(I),
                    case length(IStr) of 
                        1 ->
                            [$0 | IStr];
                        _ ->
                            IStr
                end, 
                [Yea, Mon, Day, Hou, Min, Sec]),
            "_"),
    Filename = 
        "report_" ++ TimeStr ++ ".txt",
    {ok, OutDev} = 
        file:open(Filename, [write]),


    FinalDict = 
        lists:foldl(
            fun(Dir, CDict) ->
                DirDict = bench_dir(Dir, Timeout, SlicesPerNet, MaxSC, OutDev),     
                update_total_dict(CDict, DirDict)     
            end,
            new_alg_dict({0, 0}),
            Directories
            ),
        


    ListDict = 
        dict:to_list(FinalDict),
    Considered = 
        [Item || Item = {_, {N,_}} <- ListDict, N > 0],
    NotConsidered = 
        ListDict -- Considered,
    Processed = 
        [{A, (N / C)} || {A, {N,C}} <- Considered],
    Sorted = 
        lists:sort(
            fun({_, N1}, {_, N2}) ->
                N1 =< N2
            end,
            Processed),
    Classification = 
        case Sorted of 
            [] ->
                "";
            _ ->
                    "\nClassification (from smallest to biggest):\n"
                ++  string:join(
                        lists:map(
                            fun({A, N}) -> 
                                A ++ ": " ++ float_to_list(N)
                            end,
                            Sorted),
                        "\n")
        end,
    NotExecutedReport = 
        case NotConsidered of 
            [] ->
                "";
            _ ->
                    "Not executed, i.e. always timeouted:\n" 
                ++  string:join(
                        lists:map(
                            fun({A, _}) -> 
                                A
                            end,
                            NotConsidered),
                        "\n")
        end,
    FinalReport = 
            sep()
        ++  "      Final report"
        ++  sep()
        ++  Classification
        ++  "\n\n" 
        ++  NotExecutedReport
        ++  sep(),
    file:write(OutDev, list_to_binary(FinalReport)),
    file:close(OutDev).

sep() ->
    "\n************************\n".

update_total_dict(TotalDict, DirDict) ->  
    dict:map(
        fun(K, V = {T, Count}) ->
            case dict:fetch(K, DirDict) of 
                none ->
                    V;
                TD ->
                    {T + TD, Count + 1}
            end
        end,
        TotalDict).

new_alg_dict(Value) ->
    dict:from_list(
        lists:map(
            fun(#slicer{name = AN}) -> 
                {AN, Value} 
            end, 
            algorithms())).

bench_dir(Dir, Timeout, SlicesPerNet, MaxSC, OutDev) ->
    file:write(OutDev, list_to_binary(sep() ++ "Dir: " ++ Dir ++ sep() ++ "\n")),
    {ok, FileList} = 
        file:list_dir(Dir),
    NetFiles = 
        [ Dir ++ "/" ++ File
        ||  File <- FileList,
            length(File) >= 5,
            (string:substr(File, length(File) - 3, 4) == ".xml") or 
            (string:substr(File, length(File) - 4, 5) == ".pnml")],
    FDict = 
        lists:foldl(
            fun(File, _) ->
                bench_file(File, Timeout, SlicesPerNet, MaxSC, OutDev)
            end,
            new_alg_dict(none),
            NetFiles),
    %report dir info from fdict (like final) -> outline
    FDict.

bench_file(File, Timeout, SlicesPerNet, MaxSC0, OutDev) ->
    file:write(OutDev, list_to_binary("\nFile: " ++ File ++ sep() ++ "\n")),
    io:format("File: " ++ File ++ "\n"),
    PN = 
        pn_input:read_pn(File),
    % pn_output:print_lola(PN, ""),
    Dir = PN#petri_net.dir ++ "/output/",
    pn_lib:build_digraph(PN),
    % LOLAFile = 
    %     Dir ++  PN#petri_net.name ++ ".lola", 
    AptFile = Dir ++ PN#petri_net.name ++ ".apt",
    pn_output:print_apt(PN, ""),
    % CmdCon = "java -jar apt/apt.jar pn_convert pnml apt "  ++ File ++ " " ++ AptFile,
    % io:format("~p\n", [CmdCon]),
    % ConRes = os:cmd(CmdCon),
    % io:format("~p\n", [ConRes]),
    ResAnalyses = os:cmd("java -jar apt/apt.jar examine_pn "  ++ AptFile),
    io:format("~p\n", [ResAnalyses]),
    file:write(OutDev, list_to_binary("Properties:\n" ++ ResAnalyses)),
    % Vs = digraph:vertices(PN#petri_net.digraph),
    Ps = dict:fetch_keys(PN#petri_net.places),
    MaxSC = 
        case MaxSC0 > length(Ps) of 
            true ->
                length(Ps);
            false ->
                MaxSC0
        end,
    SCS = 
        build_scs(Ps, SlicesPerNet, MaxSC, []),
    lists:foldl(
        fun(SC, CDict) ->
            bench_sc(PN, SC, Timeout, OutDev, CDict)
        end,
        new_alg_dict(none),
        SCS).

build_scs(_, 0, _, Acc) ->
    Acc;
build_scs(Vs, SlicesPerNet, MaxSC, Acc) ->
    Size = rand:uniform(MaxSC),
    SC = build_sc(Vs, Size, []),
    build_scs(Vs, SlicesPerNet - 1, MaxSC, [SC | Acc]).

build_sc(_, 0, Acc) ->
    Acc;
build_sc(List, Max, Acc) ->
    case length(List) of 
        Max ->
            List;
        _ ->
            R = 
                rand:uniform(length(List)),
            C = 
                lists:nth(R, List),
            case lists:member(C, Acc) of 
                true ->
                    build_sc(List, Max, Acc);
                false ->
                    build_sc(List, Max - 1, [C | Acc])
            end 
    end.


bench_sc(PN, SC, Timeout, OutDev, _) ->
    file:write(OutDev, list_to_binary("\nSlicing criterion: " ++ pn_lib:format("~p\n\n", [SC]))),
    lists:foldl(
        fun(A, CDict) ->
            bench_fun(A, PN, SC, Timeout, OutDev, CDict)
        end,
        new_alg_dict(0),
        algorithms()).

bench_fun(#slicer{name = AN, function = AF}, PN, SC, Timeout, OutDev, Dict) ->
    Self = self(),
    spawn(
        fun() -> 
            Self!AF(PN, SC)
        end),
    Res = 
        receive
            Res0 ->
                Res0
        after
            Timeout ->
                none
        end,
    up_dict_and_fun_info(Res, PN, AN, SC, Dict, OutDev).

up_dict_and_fun_info(none, _, AN, _, Dict, OutDev) ->
    file:write(OutDev, list_to_binary(AN ++ ": timeouted\n")),
    Dict;
up_dict_and_fun_info(Res, PN, AN, SC, Dict, OutDev) ->
    Size = dict:size(Res#petri_net.places) + dict:size(Res#petri_net.transitions),
    FunOK = 
        fun() ->
            file:write(OutDev, list_to_binary(AN ++ ": " ++ integer_to_list(Size) ++ "\n")),
            NValue = dict:fetch(AN, Dict) + Size,
            dict:store(AN, NValue, Dict)
        end,
    % io:format("Res: ~p\n", [Res]),
    case Size of 
        0 ->
            pn_output:print_lola(PN, ""),
            Dir = PN#petri_net.dir ++ "/output/",
            LOLAFile = 
                Dir ++  PN#petri_net.name ++ ".lola", 
            case check_reachable_sc(SC, LOLAFile, Dir) of 
                true -> 
                    FunOK();
                false ->
                    file:write(OutDev, list_to_binary(AN ++ ": null when it should not be\n")),
                    Dict
            end;
        _ ->
            FunOK()
    end.

check_reachable_sc([], _, _) ->
    true;
check_reachable_sc([SC | SCs], File, Dir) ->
    JSONFile = 
        Dir ++ "output.json",
    os:cmd("lola " ++ File ++ " --formula=\"EF " ++ SC ++ " > 0\" --json=" ++ JSONFile),
    {ok, IODev} = file:open(JSONFile, [read]),
    [JSONContent|_] = pn_input:read_data(IODev),
    % io:format("~p\n", [JSONContent]),
    JSON = mochijson:decode(JSONContent),
    % io:format("~p\n", [JSON]),
    {struct, [{"analysis",{struct, [_, {"result", Reachable} | _]}} | _]}  = 
        JSON,
    io:format("~p\n", [Reachable]),
    case Reachable of 
        false -> 
            false;
        true ->
            check_reachable_sc(SCs, File, Dir)
    end.

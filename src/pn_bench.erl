-module( pn_bench ).
 
-export( [bench/0, build_scs/3] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timeout_analysis() ->
    2000.

bench() ->
    Directories = 
        [
            "other"
            % "mcc_models/2011/FMS",
            % "mcc_models/2011/Kanban",
            % "mcc_models/2011/MAPK",
            % "mcc_models/2011/Peterson",
            % "mcc_models/2011/Philosophers",
            % "mcc_models/2011/TokenRing",
            % "mcc_models/2012/CSRepetitions",
            % "mcc_models/2012/Echo",
            % "mcc_models/2012/Eratosthenes",
            % "mcc_models/2012/GlobalResAllocation",
            % "mcc_models/2012/LamportFastMutEx",
            % "mcc_models/2012/NeoElection",
            % "mcc_models/2012/PhilosophersDyn",
            % "mcc_models/2012/Planning",
            % "mcc_models/2012/Railroad",
            % "mcc_models/2012/Ring",
            % "mcc_models/2012/RwMutex",
            % "mcc_models/2012/SimpleLoadBal",
            % "mcc_models/2013/Dekker",
            % "mcc_models/2013/DrinkVendingMachine",
            % "mcc_models/2013/HouseConstruction",
            % "mcc_models/2013/IBMB2S565S3960",
            % "mcc_models/2013/PermAdmissibility",
            % "mcc_models/2013/QuasiCertifProtocol",
            % "mcc_models/2013/ResAllocation",
            % "mcc_models/2013/Vasy2003",
            % "mcc_models/2014/ARMCacheCoherence",
            % "mcc_models/2014/Angiogenesis",
            % "mcc_models/2014/CircadianClock",
            % "mcc_models/2014/CircularTrains",
            % "mcc_models/2014/DatabaseWithMutex",
            % "mcc_models/2014/Diffusion2D",
            % "mcc_models/2014/ERK",
            % "mcc_models/2014/MultiwaySync",
            % "mcc_models/2014/ParamProductionCell",
            % "mcc_models/2014/PolyORBLF",
            % "mcc_models/2014/PolyORBNT",
            % "mcc_models/2014/ProductionCell",
            % "mcc_models/2014/Solitaire",
            % "mcc_models/2014/UtahNoC",
            % "mcc_models/2015/BridgeAndVehicles",
            % "mcc_models/2015/HypercubeGrid",
            % "mcc_models/2015/IBM319",
            % "mcc_models/2015/IBM5964",
            % "mcc_models/2015/IBM703",
            % "mcc_models/2015/IOTPpurchase",
            % "mcc_models/2015/Parking",
            % "mcc_models/2015/PhaseVariation",
            % "mcc_models/2015/Raft",
            % "mcc_models/2015/SafeBus",
            % "mcc_models/2015/SmallOperatingSystem",
            % "mcc_models/2015/SquareGrid",
            % "mcc_models/2015/SwimmingPool",
            % "mcc_models/2016/AirplaneLD",
            % "mcc_models/2016/AutoFlight",
            % "mcc_models/2016/CloudDeployment",
            % "mcc_models/2016/DES",
            % "mcc_models/2016/DLCshifumi",
            % "mcc_models/2016/DNAwalker",
            % "mcc_models/2016/GPPP",
            % "mcc_models/2016/HypertorusGrid",
            % "mcc_models/2016/PaceMaker",
            % "mcc_models/2016/TCPcondis",
            % "mcc_models/2016/TriangularGrid"
        ],
    Timeout = 
        1000,
    SlicesPerNet = 
        10,
    MaxSC = 
        5,
    bench(Directories, Timeout, SlicesPerNet, MaxSC).

bench(Directories, Timeout, SlicesPerNet, MaxSC) ->
    Filename = 
        "report_" ++ get_time_string() ++ ".txt",
    {ok, OutDev} = 
        file:open(Filename, [write]),
    FinalDict = 
        lists:foldl(
            fun(Dir, CDict) ->
                DirDict = bench_dir(Dir, Timeout, SlicesPerNet, MaxSC, OutDev),     
                update_total_dict(CDict, DirDict)     
            end,
            new_alg_dict({{0, 0}, []}),
            Directories),
    final_report(FinalDict, "Final report", OutDev),
    file:close(OutDev).

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
            fun(File, CDict) ->
                DictFile = bench_file(File, Timeout, SlicesPerNet, MaxSC, OutDev),
                update_total_dict(CDict, DictFile)
            end,
            new_alg_dict({{0, 0}, []}),
            NetFiles),
    final_report(FDict, "Directory report", OutDev),
    FDict.

bench_file(File, Timeout, SlicesPerNet, MaxSC0, OutDev) ->
    file:write(OutDev, list_to_binary("\nFile: " ++ File ++ sep() ++ "\n")),
    io:format("Benchmarking with file " ++ File ++ "\n"),
    PN = 
        pn_input:read_pn(File),
    pn_lib:build_digraph(PN),
    {DictPropOri, ResAnalyses} = 
        pn_properties:apt_properties(PN, timeout_analysis()),
    file:write(OutDev, list_to_binary("Properties:\n" ++ ResAnalyses)),
    Ps = dict:fetch_keys(PN#petri_net.places),
    MaxSC = 
        case MaxSC0 > length(Ps) of 
            true ->
                length(Ps);
            false ->
                MaxSC0
        end,
    SCS = 
        build_scs(Ps, SlicesPerNet, MaxSC),
    FinalDict = 
        lists:foldl(
            fun(SC, CDict) ->
                DictSC = bench_sc(PN, SC, Timeout, OutDev, DictPropOri, CDict),
                update_total_dict(CDict, DictSC)
            end,
            new_alg_dict({{0, 0}, []}),
            SCS),
    final_report(FinalDict, "File report", OutDev),
    FinalDict.

bench_sc(PN, SC, Timeout, OutDev, DictPropOri, Dict) ->
    file:write(
        OutDev, 
        list_to_binary(
            pn_lib:format(
                "\nSlicing criterion: ~s\n\n", 
                [string:join(SC, ",")] ) ) ),
    ResAlg = 
        lists:map(
            fun(A) ->
                bench_fun(A, PN, SC, Timeout, OutDev, DictPropOri)
            end,
            pn_lib:algorithms()),
        % lists:map(
            % fun(A = #slicer{name = Name, function = Fun}) ->
            %     {Name, Fun(PN, SC)},
            %     file:write(OutDev, list_to_binary(pn_lib:format("~s: ~p\n", [Name, pn_lib:size(Fun(PN, SC))]))),
            %     bench_fun(A, PN, SC, Timeout, OutDev, DictPropOri),
            %     none
            % end,
            % pn_lib:algorithms()
            % ),
    case [R || R <- ResAlg, R /= none] of 
        [] ->
            Dict;
        NoNone ->
            {Max, _} = lists:max(NoNone),
            TotalChanged = 
                case NoNone of 
                    ResAlg ->
                        lists:foldl(
                            fun({_, IP}, PropAcc) ->
                                SIP = lists:sort(IP),
                                case PropAcc of 
                                    none ->
                                        [SIP];
                                    % repeated ->
                                    %     repeated;
                                    _ ->
                                        case lists:member(SIP, PropAcc) of 
                                            true ->
                                                PropAcc;
                                            false ->
                                                [SIP | PropAcc]
                                        end
                                end
                            end,
                            none,
                            ResAlg);
                    _ ->
                        none
                end,
            case TotalChanged of 
                none ->
                    ok;
                _ ->
                    file:write(OutDev, list_to_binary("Groups of changed properties: ")),
                    [file:write(OutDev, list_to_binary("\n\tGroup " ++ (integer_to_list(Id)) ++ ": " ++ string:join(Changed, ", "))) 
                    || {Id,Changed} <- lists:zip(lists:seq(1, length(TotalChanged)),TotalChanged)],
                    case length(TotalChanged) == length(ResAlg) of 
                        true ->
                            file:write(OutDev, list_to_binary("\nNOTE: Petri Net where each algorithm changes different properties.\n"));
                        false ->
                            file:write(OutDev, list_to_binary("\n"))
                    end
            end,
            NormRes = 
                lists:map(
                    fun
                        (none) ->
                            none;
                        ({V, IP}) -> 
                            % io:format("~p / ~p\n", [V, Max]),
                            {V / Max, IP}
                    end, 
                    ResAlg),
            lists:foldl(
                fun
                    ({_, none}, CDict) ->
                            CDict;
                    ({#slicer{name = A}, {V, IP}}, CDict) ->
                        {{OldValue, Count}, OldIP} = dict:fetch(A, CDict),
                        dict:store(A, {{OldValue + V, Count + 1}, IP ++ OldIP}, CDict)
                end,
                Dict,
                lists:zip(pn_lib:algorithms(), NormRes))
    end.

bench_fun(#slicer{name = AN, function = AF}, PN, SC, Timeout, OutDev, DictPropOri) ->
    Self = self(),
    pn_lib:flush(),
    % Res = AF(PN, SC),
    % file:write(OutDev, list_to_binary("\nPLACES: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(Res#petri_net.places)), ",") ++ "\nTRANSITIONS: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(Res#petri_net.transitions)), ",") ++ "\nSize:" ++  pn_lib:format("~p", [pn_lib:size(Res)]) ++ "\n")),
    Pid = 
        spawn(
            fun() -> 
                Self!AF(PN, SC),
                receive 
                    ok ->
                        ok
                end
            end),
    Res = 
        receive
            Res0 ->
                Res0,
                Res1 = pn_lib:new_pn_fresh_digraph(Res0),
                Pid!ok,
                Res1
        after
            Timeout ->
                exit(Pid, kill),
                none
        end,
    % file:write(OutDev, list_to_binary(pn_lib:format("FUNCTION: ~p\n", [AF]) )),
    store_fun_info_and_return_size(Res, PN, AN, SC, OutDev, DictPropOri).
    % none.

store_fun_info_and_return_size(none, _, AN, _, OutDev, _) ->
    file:write(OutDev, list_to_binary(AN ++ ": timeouted\n")),
    none;
store_fun_info_and_return_size(Res, PN, AN, SC, OutDev, DictPropOri) ->
    % io:format("~p\n", [Res]),
    % io:format("~p\n", [digraph:info(Res#petri_net.digraph)]),
    pn_lib:build_digraph(Res),
    NP = dict:size(Res#petri_net.places),
    NT = dict:size(Res#petri_net.transitions),
    Size = pn_lib:size(Res),
    % file:write(OutDev, list_to_binary("\nPLACES: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(Res#petri_net.places)), ",") ++ "\nTRANSITIONS: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(Res#petri_net.transitions)), ",") ++ "\nSize:" ++  pn_lib:format("~p", [pn_lib:size(Res)]) ++ "\n")),
    FunOK = 
        % fun() -> none end,
        fun() ->
            {Preserved, Changed} = 
                case DictPropOri of 
                    none ->
                        {[], []};
                    _ ->
                        case 
                            {
                                list_to_integer(dict:fetch("num_places", DictPropOri)), 
                                list_to_integer(dict:fetch("num_transitions", DictPropOri))
                            } 
                        of 
                            {NP, NT} ->
                                {dict:fetch_keys(DictPropOri), []};
                            _ ->
                                % pn_properties:compare_properties(DictPropOri, DictPropOri)
                                {DictSlice, _} = 
                                    pn_properties:apt_properties(Res, timeout_analysis()), 
                                % file:write(OutDev, list_to_binary("\nDICT: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(DictSlice)), ","))),
                                pn_properties:compare_properties(DictPropOri, DictSlice)
                        end
                end,
            PreservedStr = 
                case Changed of 
                    [] ->
                        "All";
                    _ ->
                        string:join(Preserved, ", ")
                end,
            InfoAlg = 
                [
                    "\t- Size: " ++ integer_to_list(Size),
                    "",
                    "\t- Preserved properties: " ++ PreservedStr,
                    "",
                    "\t- Changed properties: " ++ string:join(Changed, ", "),
                    ""
                ],
            file:write(OutDev, list_to_binary(AN ++ ":\n" ++ string:join(InfoAlg, "\n") ++ "\n")),
            {Size, Changed}
        end,
    % io:format("Res: ~p\n", [Res]),
    case Size of 
        0 ->
            pn_lib:build_digraph(PN),
            pn_output:print_lola(PN, ""),
            Dir = PN#petri_net.dir ++ "/output/",
            LOLAFile = 
                Dir ++  PN#petri_net.name ++ ".lola", 
            case pn_properties:check_reachable_sc(SC, LOLAFile, Dir, timeout_analysis()) of 
                false -> 
                    FunOK();
                true ->
                    file:write(OutDev, list_to_binary(AN ++ ": null when it should not be\n")),
                    none
            end;
        _ ->
            FunOK()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

final_report(Dict, Msg, OutDev) ->
    ListDict = 
        dict:to_list(Dict),
    Considered = 
        [Item || Item = {_, {{N,_}, _}} <- ListDict, N > 0],
    NotConsidered = 
        ListDict -- Considered,
    Processed = 
        [{A, {(N / C), IP}} || {A, {{N,C}, IP}} <- Considered],
    Sorted = 
        lists:sort(
            fun({_, {N1,_}}, {_, {N2, _}}) ->
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
                            fun({A, {N, CP}}) -> 
                                Changed = 
                                    lists:usort(CP),
                                NotChanged = 
                                    pn_properties:all_properties() -- Changed, 
                                NotChangedStr = 
                                    case Changed of 
                                        [] ->
                                            "All";
                                        _ ->
                                            string:join(NotChanged, ", ")
                                    end,
                                InfoAlg = 
                                    [
                                        A, 
                                        "\t- Size: " ++ pn_lib:format("~.3f", [N]) ,
                                        "",
                                        "\t- Preserved properties: " ++ NotChangedStr,
                                        "",
                                        "\t- Changed properties: " ++ string:join(Changed, ", "),
                                        ""
                                    ],
                                string:join(InfoAlg, "\n")
                            end,
                            Sorted),
                        "\n")
        end,
    NotExecutedReport = 
        case NotConsidered of 
            [] ->
                "";
            _ ->
                    "Not executed, i.e. always timeouted or null:\n" 
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
        ++  "\t" 
        ++  Msg
        ++  sep()
        ++  Classification
        ++  "\n\n" 
        ++  NotExecutedReport
        ++  sep(),
    file:write(OutDev, list_to_binary(FinalReport)).


sep() ->
    "\n************************\n".

update_total_dict(TotalDict, DirDict) ->  
    dict:map(
        fun(K, {{T, Count}, IP}) ->
            {{NT, NCount}, NIP} = dict:fetch(K, DirDict),
            {{T + NT, Count + NCount}, IP ++ NIP}
        end,
        TotalDict).

new_alg_dict(Value) ->
    dict:from_list(
        lists:map(
            fun(#slicer{name = AN}) -> 
                {AN, Value} 
            end, 
            pn_lib:algorithms())).

build_scs(Vs, SlicesPerNet, MaxSC) ->
    build_scs(Vs, SlicesPerNet, MaxSC, []).

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

get_time_string() ->
    {{Yea, Mon, Day}, {Hou, Min, Sec}} = 
        calendar:local_time(),
    string:join(
        lists:map(
            fun(I) -> 
                IStr = integer_to_list(I),
                case length(IStr) of 
                    1 ->
                        [$0 | IStr];
                    _ ->
                        IStr
                end
            end, 
            [Yea, Mon, Day, Hou, Min, Sec]),
        "_").
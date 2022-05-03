-module( pn_bench_prop ).
 
-export( [bench/0, bench/1] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timeout_analysis() ->
    15000.

directories() ->
    [
        % {"other", all}
        % "examples/other"
        % "other"
        %  "mcc_models/2011/FMS"
        % , "mcc_models/2011/Kanban"
        % , "mcc_models/2011/MAPK"
        % , "mcc_models/2011/Peterson"
        % , "mcc_models/2011/Philosophers"
        % , "mcc_models/2011/SharedMemory" 
        % , "mcc_models/2011/TokenRing"
        %  "mcc_models/2012/CSRepetitions"
        % , "mcc_models/2012/Echo"
        % , "mcc_models/2012/Eratosthenes"
        % , "mcc_models/2012/GlobalResAllocation"
        % , "mcc_models/2012/LamportFastMutEx"
        % , "mcc_models/2012/NeoElection"
        % , "mcc_models/2012/PhilosophersDyn"
        % , "mcc_models/2012/Planning"
        % , "mcc_models/2012/Railroad"
        % , "mcc_models/2012/Ring"
        % , "mcc_models/2012/RwMutex"
        % , "mcc_models/2012/SimpleLoadBal"
        %  "mcc_models/2013/Dekker"
        % , "mcc_models/2013/DrinkVendingMachine"
        % , "mcc_models/2013/HouseConstruction"
        % , "mcc_models/2013/IBMB2S565S3960"
        % , "mcc_models/2013/PermAdmissibility"
        % , "mcc_models/2013/QuasiCertifProtocol"
        % , "mcc_models/2013/ResAllocation"
        % , "mcc_models/2013/Vasy2003"
        %  "mcc_models/2014/ARMCacheCoherence"
        % , "mcc_models/2014/Angiogenesis"
        % , "mcc_models/2014/CircadianClock"
        % , "mcc_models/2014/CircularTrains"
        % , "mcc_models/2014/DatabaseWithMutex"
        % , "mcc_models/2014/Diffusion2D"
        % , "mcc_models/2014/ERK"
        % , "mcc_models/2014/MultiwaySync"
        % , "mcc_models/2014/ParamProductionCell"
        % , "mcc_models/2014/PolyORBLF"
        % , "mcc_models/2014/PolyORBNT"
        % , "mcc_models/2014/ProductionCell"
        % , "mcc_models/2014/Solitaire"
        % , "mcc_models/2014/UtahNoC"
        %  "mcc_models/2015/BridgeAndVehicles"
        % , "mcc_models/2015/HypercubeGrid"
        % , "mcc_models/2015/IBM319"
        % , "mcc_models/2015/IBM5964"
        % , "mcc_models/2015/IBM703"
        % , "mcc_models/2015/IOTPpurchase"
        % , "mcc_models/2015/Parking"
        % , "mcc_models/2015/PhaseVariation"
        % , "mcc_models/2015/Raft"
        % , "mcc_models/2015/SafeBus"
        % , "mcc_models/2015/SmallOperatingSystem"
        % , "mcc_models/2015/SquareGrid"
        % , "mcc_models/2015/SwimmingPool"
         "mcc_models/2016/AirplaneLD"
        , "mcc_models/2016/AutoFlight"
        , "mcc_models/2016/CloudDeployment"
        , "mcc_models/2016/DES"
        , "mcc_models/2016/DLCshifumi"
        , "mcc_models/2016/DNAwalker"
        , "mcc_models/2016/GPPP"
        , "mcc_models/2016/HypertorusGrid"
        , "mcc_models/2016/PaceMaker"
        , "mcc_models/2016/TCPcondis"
        , "mcc_models/2016/TriangularGrid"
    ].


bench([Year | _]) ->
    bench_common([Dir || Dir <- directories(), string:substr(Dir, 12, 4) == Year], Year).

bench() ->
    bench_common(directories(), "").

bench_common(Dirs, Filename) ->
    Timeout = 
        5000,
    SlicesPerNet = 
        % 20,
        20,
    MaxSC = 
        5,
        % 1,
    bench(Dirs, Timeout, SlicesPerNet, MaxSC, Filename).

bench(Directories, Timeout, SlicesPerNet, MaxSC, Filename0) ->
    Filename = 
        case Filename0 of 
            [] ->
                "report_" ++ get_time_string() ++ ".txt";
            _ ->
                Filename0
        end,
    {ok, OutDev} = 
        file:open(Filename, [write]),
    FinalDict = 
        lists:foldl(
            fun(Dir, CDict) ->
                bench_dir(Dir, Timeout, SlicesPerNet, MaxSC, OutDev, CDict)        
            end,
            new_alg_dict({0, 0}),
            Directories),
    file:close(OutDev),
    [io:format( "{~p, ~p},\n", [A, dict:to_list(DL)]) || {A, DL} <- dict:to_list(FinalDict)],
    ok.
    % final_report(FinalDict, "Final report", OutDev).

bench_dir([], _, _, _, _, TotalDict) ->
    TotalDict;
bench_dir(Dir0, Timeout, SlicesPerNet, MaxSC, OutDev, TotalDict) ->
    Dir = 
        "examples/" ++ Dir0,
    % file:write(OutDev, list_to_binary(sep() ++ "Dir: " ++ Dir ++ sep() ++ "\n")),
    {ok, FileList} = 
        file:list_dir(Dir),
    io:format("~p\n", [FileList]),
    MinLenght = 
        lists:foldl(
            fun(File, CMin) ->
                LF = length(File),
                case {LF < CMin, pn_input:is_pnml_file(File)} of 
                    {true, true} ->
                        LF;
                    _ ->
                        CMin
                end
            end,
            100,
            FileList),
    [Selected|_] = 
        lists:sort([File || File <- FileList, length(File) == MinLenght, pn_input:is_pnml_file(File)]),
    NetFile =  Dir ++ "/" ++ Selected,
    try
        bench_file(NetFile, Timeout, SlicesPerNet, MaxSC, OutDev, TotalDict)
    catch 
        _:_ ->
            file:write(OutDev, list_to_binary("\nFile " ++ NetFile ++ " cannot be analyzed." ++ "\n")),
            TotalDict
    end.

% get_selected_apt_properties(PN, TimeOut) -> 
%     ResDict = lists:foldl(
%         fun(Property, CurrentDict) ->
%             dict:store(Property, pn_properties:apt_property(Property, PN, TimeOut), CurrentDict)
%         end,
%         dict:new(),
%         ["num_places","num_transitions","num_arcs","num_tokens"]),
%     ResStr = lists:foldl(
%         fun(Key, CurrentStr) ->
%             CurrentStr + "\n" + io_lib:format("~p: ~p",[Key, dict:fetch(Key, ResDict)])
%         end,
%         "",
%         dict:fetch_keys(ResDict)),
%     {ResDict, ResStr}.

get_selected_apt_properties(PN) -> 
    ListForDict = 
        [
            {"num_places", dict:size(PN#petri_net.places)},
            {"num_transitions", dict:size(PN#petri_net.transitions)},
            {"num_arcs", length(PN#petri_net.arcs)},
            {"num_tokens", lists:sum([(dict:fetch(Place, PN#petri_net.places))#place.marking || Place <- dict:fetch_keys(PN#petri_net.places)])}
        ],
    ResStr = lists:foldl(
        fun({K,V}, Prev)->
            Prev ++ io_lib:format("~s: ~p\n", [K, V])
        end,
        "",
        ListForDict
        ),
    {dict:from_list(ListForDict), ResStr}.

bench_file(File, Timeout, SlicesPerNet, MaxSC0, OutDev, TotalDict) ->
    file:write(OutDev, list_to_binary("\nFile: " ++ File ++ sep() ++ "\n")),
    io:format("Benchmarking with file " ++ File ++ "\n"),
    PN = 
        pn_input:read_pn(File),
    pn_lib:build_digraph(PN),
    % build_lola(PN, ""),
    % io:format("~p\n", [dict:size(PN#petri_net.places)]),
    % io:format("~p\n", [dict:size(PN#petri_net.transitions)]),
    % io:format("~p\n", [length(PN#petri_net.arcs)]),
    % io:format("~p\n", [lists:sum([(dict:fetch(Place, PN#petri_net.places))#place.marking || Place <- dict:fetch_keys(PN#petri_net.places)])]),

    % {DictPropOri0, ResAnalyses} = 
    %     pn_properties:apt_properties(PN, timeout_analysis()),
    {DictPropOri0, ResAnalyses} = get_selected_apt_properties(PN),
    % io:format("~p\n", [{DictPropOri0, ResAnalyses}]),
    DictPropOri = 
        build_other_properties(PN, DictPropOri0, none, ""),
    case DictPropOri of 
        none -> 
            file:write(OutDev, list_to_binary("Property analysis timeouted.\n")),
            TotalDict; 
        _ ->
            file:write(OutDev, list_to_binary("Properties:\n" ++ ResAnalyses)),
            % file:write(OutDev, list_to_binary(dict:fetch("traps", DictPropOri))),
            % file:write(OutDev, list_to_binary(dict:fetch("siphons", DictPropOri))),
            % file:write(OutDev, list_to_binary(pn_lib:format("Deadlock: ~p\n" , [dict:fetch("deadlock", DictPropOri)]))),
            file:write(OutDev, list_to_binary(pn_lib:format("Size: ~p\n\n" , [dict:fetch("size", DictPropOri)]))),
            % Ps = dict:fetch_keys(PN#petri_net.places),
            Ps0 = [{P, dict:fetch(P, PN#petri_net.places)} || P <- dict:fetch_keys(PN#petri_net.places)],
            Ps = [P || {P, M} <- Ps0, M#place.marking == 0],
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
                    try
                        bench_sc(PN, SC, Timeout, OutDev, DictPropOri, CDict)
                    catch 
                        _:_ ->
                            file:write(OutDev, list_to_binary("\nSomething went wrong. Ingnoring results.\n")),
                            CDict
                    end
                end,
                TotalDict,
                SCS)
    end.

bench_sc(PN, SC, Timeout, OutDev, DictPropOri, Dict) ->
    file:write(
        OutDev, 
        list_to_binary(
            pn_lib:format(
                "\nSlicing criterion: ~s\n\n", 
                [string:join(SC, ",")] ) ) ),
    % io:format(
    %             "\nSlicing criterion: ~s\n\n", 
    %             [string:join(SC, ",")] ),
    ResAlg = 
        lists:foldl(
            fun
            (_, none) ->
                none;
            
            (A, Acc) ->
                ResFun = 
                    try 
                        bench_fun(A, PN, SC, Timeout, OutDev, DictPropOri)
                    catch 
                        _:_ ->
                            none 
                    end,
                case ResFun of 
                    none ->
                        none;
                    _ ->
                        [{A#slicer.name, ResFun} | Acc]
                end
            end,
            [],
            pn_lib:algorithms_bench()),
    case ResAlg of 
        none ->
            Dict;
        _ ->
            lists:foldl(
                fun
                    ({A, DictA}, CDict) ->
                        OldValue = dict:fetch(A, CDict),
                        dict:store(A, merge_prop_dict(OldValue, DictA), CDict)
                end,
                Dict,
                ResAlg)
    end.

bench_fun(#slicer{name = AN, function = AF}, PN, SC, Timeout, OutDev, DictPropOri) ->
    Self = self(),
    pn_lib:flush(),
    % Res = AF(PN, SC),
    % file:write(OutDev, list_to_binary("\nPLACES: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(Res#petri_net.places)), ",") ++ "\nTRANSITIONS: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(Res#petri_net.transitions)), ",") ++ "\nSize:" ++  pn_lib:format("~p", [pn_lib:size(Res)]) ++ "\n")),
    Pid = 
        spawn(
            fun() -> 
                TimeBeforeExecuting = erlang:monotonic_time(milli_seconds),
                TempRes = AF(PN, SC),
                TimeAfterExecuting = erlang:monotonic_time(milli_seconds),
                TimeExecuting = TimeAfterExecuting - TimeBeforeExecuting,
                Self!{TempRes, TimeExecuting},
                receive 
                    ok ->
                        ok
                end
            end),
    {Res, TimeInfo} = 
        receive
            {Res0, TimeInfo_} ->
                Res1 = pn_lib:new_pn_fresh_digraph(Res0),
                Pid!ok,
                {Res1, TimeInfo_}
        after
            Timeout ->
                exit(Pid, kill),
                pn_output:cmd_try("pkill python3"),
                {none, none}
        end,
    store_fun_info(Res, PN, AN, SC, OutDev, DictPropOri, TimeInfo).

store_fun_info(none, _, AN, _, OutDev, _, _) ->
    file:write(OutDev, list_to_binary(AN ++ ": timeouted\n")),
    none;
store_fun_info(Res, PN, AN, SC, OutDev, DictPropOri, TimeInfo) ->
    pn_lib:build_digraph(Res),
    FunOK = 
        fun() -> 
            case DictPropOri of 
                none ->
                    none;
                _ ->
                    % {DictSlice0, _} = 
                    %     pn_properties:apt_properties(Res, timeout_analysis()), 
                    {DictSlice0, _} = get_selected_apt_properties(Res), 
                    DictSlice = 
                        build_other_properties(Res, DictSlice0, TimeInfo, ".slice"),
                    % file:write(OutDev, list_to_binary("\nDICT: " ++ string:join(lists:map(fun(X) -> pn_lib:format("~p", [X]) end, dict:to_list(DictSlice)), ","))),
                    pn_properties:compare_properties_all(DictPropOri, DictSlice)
            end
        end,
    case pn_lib:size(Res) of 
        0 ->
            % build_lola(Res, ".slice"),
            Dir = PN#petri_net.dir ++ "/output/",
            LOLAFile = 
                Dir ++  PN#petri_net.name ++ ".slice.lola", 
            case pn_properties:check_reachable_sc(SC, LOLAFile, Dir, timeout_analysis()) of 
                false -> 
                    FunOK();
                true ->
                    file:write(
                        OutDev, 
                        list_to_binary(
                            AN ++ ": null when it should not be\n")),
                    none
            end;
        _ ->
            % build_lola(Res, ".slice"),
            FunOK()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sep() ->
    "\n************************\n".

new_alg_dict(Value) ->
    dict:from_list(
        lists:map(
            fun(#slicer{name = AN}) -> 
                {AN, create_alg_dict(Value)} 
            end, 
            pn_lib:algorithms_bench())).

create_alg_dict(Value) ->
    Props = 
        % pn_properties:all_properties() ++ ["siphons", "traps", "deadlock", "size", "time"],
        pn_properties:all_properties() ++ ["size", "time"],
        % ["num_places","num_transitions","num_arcs","num_tokens", "size", "time"],
    dict:from_list(
        lists:zip(
            Props,
            lists:duplicate(length(Props), Value) )
        ).

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

% siphons_and_traps(PN) ->
%     {
%         pn_properties:apt_property("siphons", PN, timeout_analysis()),
%         pn_properties:apt_property("traps", PN, timeout_analysis())
%     }.


build_lola(PN, Suffix) ->
    pn_lib:build_digraph(PN),
    pn_output:print_lola(PN, Suffix).

build_other_properties(_, none, _, _) ->
    none;
build_other_properties(PN, DictProp, TimeInfo, _) ->
% build_other_properties(PN, DictProp, TimeInfo, Suffix) ->
    % {SiphonsValue, TrapsValue} = 
    %     siphons_and_traps(PN),
    % Dir = PN#petri_net.dir ++ "/output/",
    % LOLAFile = 
    %     Dir ++  PN#petri_net.name ++ Suffix ++".lola",
    % DeadlockRes = 
    %     % pn_properties:check_formula("EF DEADLOCK", LOLAFile, Dir, timeout_analysis()),
    %     true,
    % io:format("~p\n", [DeadlockRes]),
    % case lists:any(fun(none) -> true; (_) -> false end, [SiphonsValue, TrapsValue, DeadlockRes]) of 
    case false of
        false -> 
            lists:foldl(
                fun({K, V}, CDict) ->
                    dict:store(K, V, CDict)
                end,
                DictProp,
                [
                    % {"siphons", SiphonsValue}, 
                    % {"traps", TrapsValue}, 
                    % {"deadlock", DeadlockRes},
                    {"size", pn_lib:size(PN)},
                    {"time", TimeInfo}
                ]);
        true ->
            none
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

merge_prop_dict(DictAlg, DictProp) -> 
    dict:fold(
        fun
            (K, {Count, CountTotal}, CDict) -> 
                % io:format("~p\n", [dict:fetch_keys(DictProp)]),
                % case K of
                %     "num_tokens" when Count > 1 ->
                %         io:format("Found a case"),
                %         0/0;
                %     _ -> 
                %         true 
                % end,
                case dict:find(K, DictProp) of 
                    error ->
                        dict:store(K, {Count, CountTotal}, CDict);
                    {ok, none} -> 
                        dict:store(K, {Count, CountTotal}, CDict);
                    {ok, {preserved, _}} ->
                        dict:store(K, {Count + 1, CountTotal + 1}, CDict);
                    {ok, {no_preserved, _, _}} ->
                        dict:store(K, {Count, CountTotal + 1}, CDict);
                    {ok, Num} ->
                        dict:store(K, {Count + Num, CountTotal + 1}, CDict)
                end
        end,
        dict:new(),
        DictAlg).



-module( pn_bench ).
 
-export( [bench/0] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timeout_analysis() ->
    2000.

bench() ->
    Directories = 
        [
            "examples/other"
            % ,"examples/mcc_models/2011/Peterson"
        ],
    Timeout = 
        1000,
    SlicesPerNet = 
        10,
    MaxSC = 
        5,
    bench(Directories, Timeout, SlicesPerNet, MaxSC).

algorithms() -> 
    [
        #slicer{
            name = "Llorens et al's slicer", 
            function = fun pn_slice:slice/2},
        #slicer{
            name = "Llorens et al's slicer improved", 
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
    io:format("File: " ++ File ++ "\n"),
    PN = 
        pn_input:read_pn(File),
    pn_lib:build_digraph(PN),
    {DictPropOri, ResAnalyses} = apt_properties(PN),
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
        build_scs(Ps, SlicesPerNet, MaxSC, []),
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
    file:write(OutDev, list_to_binary("\nSlicing criterion: " ++ pn_lib:format("~p\n\n", [SC]))),
    ResAlg = 
        lists:map(
            fun(A) ->
                bench_fun(A, PN, SC, Timeout, OutDev, DictPropOri)
            end,
            algorithms()),
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
                    [file:write(OutDev, list_to_binary("\n\t" ++ string:join(Changed, ", "))) || Changed <- TotalChanged],
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
                lists:zip(algorithms(), NormRes))
    end.

bench_fun(#slicer{name = AN, function = AF}, PN, SC, Timeout, OutDev, DictPropOri) ->
    Self = self(),
    flush(),
    Pid = 
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
                exit(Pid, kill),
                none
        end,
    store_fun_info_and_return_size(Res, PN, AN, SC, OutDev, DictPropOri).

store_fun_info_and_return_size(none, _, AN, _, OutDev, _) ->
    file:write(OutDev, list_to_binary(AN ++ ": timeouted\n")),
    none;
store_fun_info_and_return_size(Res, PN, AN, SC, OutDev, DictPropOri) ->
    % io:format("~p\n", [Res]),
    NP = dict:size(Res#petri_net.places),
    NT = dict:size(Res#petri_net.transitions),
    Size = NP + NT,
    FunOK = 
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
                                {DictSlice, _} = 
                                    apt_properties(Res), 
                                compare_properties(DictPropOri, DictSlice)
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
            case check_reachable_sc(SC, LOLAFile, Dir) of 
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
                                    all_properties() -- Changed, 
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
            algorithms())).

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

check_reachable_sc([], _, _) ->
    false;
check_reachable_sc([SC | SCs], File, Dir) ->
    JSONFile = 
        Dir ++ "output.json",
    os:cmd("lola " ++ File ++ " --formula=\"EF " ++ SC ++ " > 0\" --json=" ++ JSONFile),
    {ok, IODev} = file:open(JSONFile, [read]),
    [JSONContent|_] = pn_input:read_data(IODev),
    JSON = mochijson:decode(JSONContent),
    {struct, [{"analysis",{struct, [_, {"result", Reachable} | _]}} | _]}  = 
        JSON,
    % io:format("~s: ~p\n", [SC, Reachable]),
    case Reachable of 
        true -> 
            true;
        false ->
            check_reachable_sc(SCs, File, Dir)
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

apt_properties(PN) ->
    Dir = PN#petri_net.dir ++ "/output/",
    AptFile = Dir ++ PN#petri_net.name ++ ".apt",
    pn_lib:build_digraph(PN),
    pn_output:print_apt(PN, ""),
    % Self = self(),
    Cmd = 
        "java -jar apt/apt.jar examine_pn "  ++ AptFile,
    flush(),
    cmd_run(Cmd, timeout_analysis()).
    % Port = erlang:open_port({spawn, Cmd},[exit_status]),
    % receive
    %     {Port, {data, NewData}} -> loop(Port, [Data | NewData], Timeout);
    %     {Port, {exit_status, 0}} -> Data;
    %     {Port, {exit_status, S}} -> throw({commandfailed, S})
    % after Timeout ->
    %     throw(timeout)
    % end,
    % io:format("~p\n", [Cmd]),
    % Pid = 
    %     spawn(
    %         fun() ->
    %             Self!os:cmd(Cmd)
    %         end),
    % receive 
    %     ResAnalyses ->
    %         DictProperties = parse_properties(ResAnalyses),
    %         {DictProperties, ResAnalyses}
    % after 
    %     TO ->
    %         exit(Pid, kill),
    %         % io:format("Pids: ~s\n", [os:cmd("pgrep -P " ++ os:getpid())]),
    %         {none, "No analyzed (timeouted).\n\n"}
    % end.

parse_properties(PropStr) ->
    Lines = string:tokens(PropStr, "\n"),
    Pairs = 
        lists:map(
            fun(Line) ->
                [PropName, [_|Value]] = 
                    string:tokens(Line, ":"),
                {PropName, Value}
            end,
            Lines),
    dict:from_list(Pairs).

compare_properties(_, none) ->
    {[], []};
compare_properties(DictOri, DictSlice) ->
    dict:fold(
        fun(K, V, {CP, CC}) ->
            case dict:fetch(K, DictSlice) of 
                V ->
                    {[K | CP], CC};
                _ ->
                    {CP, [K | CC]}
            end
        end,
        {[], []},
        DictOri).

all_properties() ->
    [ 
        "backwards_persistent", "output_nonbranching", "bicf", 
        "strongly_connected", "free_choice", "pure", "plain", 
        "persistent", "strongly_live", "k-marking", "s_net", 
        "nonpure_only_simple_side_conditions", "bcf", "num_tokens",
        "asymmetric_choice", "safe", "weakly_live", 
        "restricted_free_choice", "homogeneous", "k-bounded",
        "bounded", "reversible", "conflict_free", 
        "weakly_connected", "num_places", "num_arcs", "simply_live",
         "num_transitions", "t_net", "num_labels", "isolated_elements"
    ].

flush() ->
    receive
        _ -> 
            flush()
    after 0 ->
        ok
    end.
    
cmd_run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    [PidOs] = 
        [PidOs0 || {os_pid,PidOs0} <- erlang:port_info(Port)],
    Res = cmd_loop(Port, [], Timeout),
    try 
        port_close(Port),
        os:cmd("kill -9 " ++ integer_to_list(PidOs))
    catch 
        _:_ ->
            ok
    end,
    Res.
    
cmd_loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> 
            cmd_loop(Port, [NewData | Data], Timeout);
        {Port, {exit_status, 0}} -> 
            ResAnalyses = string:join(lists:reverse(Data), ""),
            % io:format("~s\n", [ResAnalyses]),
            DictProperties = parse_properties(ResAnalyses),
            {DictProperties, ResAnalyses};
        {Port, {exit_status, _}} -> 
            {none, "No analyzed (error).\n\n"}
    after 
        Timeout ->

            {none, "No analyzed (timeouted).\n\n"}
    end.

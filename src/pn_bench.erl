-module( pn_bench ).
 
-export( [bench/0] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bench() ->
    Directories = 
        [
              "examples/mcc_models/2011/FMS"
            , "examples/other"
        ],
    Timeout = 
        1000,
    SlicesPerNet = 
        10,
    MaxSC = 
        5,
    start_bench(Directories, Timeout, SlicesPerNet, MaxSC).

start_bench(Directories, Timeout, SlicesPerNet, MaxSC) ->
    {{Yea, Mon, Day}, {Hou, Min, Sec}} = 
        calendar:local_time(),
    TimeStr = 
        string:join(
            lists:map(
                fun integer_to_list/1, 
                [Yea, Mon, Day, Hou, Min, Sec]),
            "_"),
    Filename = 
        "report_" ++ TimeStr ++ ".txt",
    {ok, OutDev} = 
        file:open(Filename, [write]),
    StartingDict = 
        dict:from_list(
            [{slice, {0,0}}, {slice_imp, {0,0}}, {yuetal, {0,0}},
             {rakow_ctl, {0,0}}, {rakow_safety, {0,0}}]),


    FinalDict = 
        start_bench(Directories, Timeout, SlicesPerNet, MaxSC, OutDev, StartingDict),


    ListDict = 
        dict:to_list(FinalDict),
    Considered = 
        [Item || Item = {_, {N,C}} <- ListDict, N > 0],
    NotConsidered = 
        ListDict -- Considered,
    Processed = 
        [{A, (N / C)} || Item = {A, {N,C}} <- Considered],
    Sorted = 
        lists:sort(
            fun({A1, N1}, {A2, N2}) ->
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
                                atom_to_list(A) ++ "" ++ float_to_list(N)
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
                                atom_to_list(A)
                            end,
                            NotConsidered),
                        "\n")
        end,
    FinalReport = 
            "\n************************\n"
        ++  "      Final report"
        ++  "\n************************\n"
        ++  Classification
        ++  "\n\n" 
        ++  NotExecutedReport
        ++  "\n\n************************\n",
    file:write(OutDev, list_to_binary(FinalReport)),
    file:close(OutDev).



start_bench(Directories, Timeout, SlicesPerNet, MaxSC, OutDev, Dict) ->
    Dict.

-module( pn_select_html ).
 
-export( [create/0] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create select options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create() ->
    Directories = 
        [
            "mcc_models/2011/FMS",
            "mcc_models/2011/Kanban",
            "mcc_models/2011/MAPK",
            "mcc_models/2011/Peterson",
            "mcc_models/2011/Philosophers",
            "mcc_models/2011/TokenRing",
            "mcc_models/2012/CSRepetitions",
            "mcc_models/2012/Echo",
            "mcc_models/2012/Eratosthenes",
            "mcc_models/2012/GlobalResAllocation",
            "mcc_models/2012/LamportFastMutEx",
            "mcc_models/2012/NeoElection",
            "mcc_models/2012/PhilosophersDyn",
            "mcc_models/2012/Planning",
            "mcc_models/2012/Railroad",
            "mcc_models/2012/Ring",
            "mcc_models/2012/RwMutex",
            "mcc_models/2012/SimpleLoadBal",
            "mcc_models/2013/Dekker",
            "mcc_models/2013/DrinkVendingMachine",
            "mcc_models/2013/HouseConstruction",
            "mcc_models/2013/IBMB2S565S3960",
            "mcc_models/2013/PermAdmissibility",
            "mcc_models/2013/QuasiCertifProtocol",
            "mcc_models/2013/ResAllocation",
            "mcc_models/2013/Vasy2003",
            "mcc_models/2014/ARMCacheCoherence",
            "mcc_models/2014/Angiogenesis",
            "mcc_models/2014/CircadianClock",
            "mcc_models/2014/CircularTrains",
            "mcc_models/2014/DatabaseWithMutex",
            "mcc_models/2014/Diffusion2D",
            "mcc_models/2014/ERK",
            "mcc_models/2014/MultiwaySync",
            "mcc_models/2014/ParamProductionCell",
            "mcc_models/2014/PolyORBLF",
            "mcc_models/2014/PolyORBNT",
            "mcc_models/2014/ProductionCell",
            "mcc_models/2014/Solitaire",
            "mcc_models/2014/UtahNoC",
            "mcc_models/2015/BridgeAndVehicles",
            "mcc_models/2015/HypercubeGrid",
            "mcc_models/2015/IBM319",
            "mcc_models/2015/IBM5964",
            "mcc_models/2015/IBM703",
            "mcc_models/2015/IOTPpurchase",
            "mcc_models/2015/Parking",
            "mcc_models/2015/PhaseVariation",
            "mcc_models/2015/Raft",
            "mcc_models/2015/SafeBus",
            "mcc_models/2015/SmallOperatingSystem",
            "mcc_models/2015/SquareGrid",
            "mcc_models/2015/SwimmingPool",
            "mcc_models/2016/AirplaneLD",
            "mcc_models/2016/AutoFlight",
            "mcc_models/2016/CloudDeployment",
            "mcc_models/2016/DES",
            "mcc_models/2016/DLCshifumi",
            "mcc_models/2016/DNAwalker",
            "mcc_models/2016/GPPP",
            "mcc_models/2016/HypertorusGrid",
            "mcc_models/2016/PaceMaker",
            "mcc_models/2016/TCPcondis",
            "mcc_models/2016/TriangularGrid",
            {"other", all}
        ],
    lists:map(fun create_options_dir/1, Directories).

create_options_dir({Dir, all}) ->
    % io:format("examples/" ++ Dir ++ "\n"),
    {ok, FileList} = 
        file:list_dir("examples/" ++ Dir),
    NetFiles = 
        [ Dir ++ "/" ++ File
        ||  File <- FileList,
            length(File) >= 5,
            (string:substr(File, length(File) - 3, 4) == ".xml") or 
            (string:substr(File, length(File) - 4, 5) == ".pnml")],
    lists:map(fun create_option_file/1, NetFiles);
create_options_dir(Dir) ->
    % io:format("examples/" ++ Dir ++ "\n"),
    {ok, FileList} = 
        file:list_dir("examples/" ++ Dir),
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
    NetFile = Dir ++ "/" ++ Selected,
    create_option_file(NetFile).

create_option_file(File) ->
    Opt = 
            "</option><option value=\"https://raw.githubusercontent.com/tamarit/pn_suite/master/examples/" 
        ++  File 
        ++  "\">" 
        ++  File 
        ++  "</option>\n",
    io:format(Opt).

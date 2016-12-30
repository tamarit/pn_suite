-module( pn_properties ).
 
-export( 
    [
        apt_properties/2, 
        compare_properties/2, compare_properties_all/2,
        all_properties/0, check_reachable_sc/4,
        parse_property_list/1, check_formula/4,
        apt_property/3, is_apt_property_preserved/4
    ] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


parse_property_list(PropsStr) ->
    case PropsStr of 
        [$a, $l, $g, $: |Alg] ->
            {alg, Alg};
        _ ->
            parse_property_list_aux(PropsStr)
    end.

parse_property_list_aux(PropsStr) ->
    TokPropsStr = string:tokens(PropsStr, ","),
    case 
        [   
            PS 
        ||  
            PS <- TokPropsStr, 
            lists:member(PS, all_properties())
        ] 
    of 
        TokPropsStr -> 
            TokPropsStr;
        PropsStrFiltered -> 
            Rest = TokPropsStr -- PropsStrFiltered,
            LolaExprs = 
                [   {lola, element(2, is_lola(P))} 
                ||  P <- Rest, element(1, is_lola(P))],
            STExprs =
                case length(LolaExprs) < length(Rest) of 
                    true -> 
                        Rest2 = Rest -- LolaExprs,
                        STExprs0 = 
                            [   {st, P}
                            ||  P <- Rest2, P == "siphons" orelse P == "traps"],
                        case length(STExprs0) < length(Rest2) of 
                            true ->
                                io:format("Some properties are unknown. They will be ignored.\n");
                            false ->
                                ok
                        end,
                        STExprs0;
                    false -> 
                        []
                end,       
            PropsStrFiltered ++ LolaExprs ++ STExprs
    end.

is_lola([$l, $o, $l, $a, $: | LolaExp]) ->
    {true, LolaExp};
is_lola(_) ->
    {false, []}.

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


compare_properties_all(_, none) ->
    none;
compare_properties_all(DictOri, DictSlice) ->
    FunST = 
        fun (K, V, CDict) -> 
            case dict:fetch(K, DictSlice) of 
                none -> 
                    dict:store(K, none, CDict); 
                VSStr -> 
                    case is_apt_property_preserved(V, VSStr, K) of 
                        true -> 
                            dict:store(K, {preserved, V}, CDict);
                        false -> 
                            dict:store(K, {no_preserved, V, VSStr}, CDict)
                    end
            end
        end,
    dict:fold(
        fun
            (K = "size", V, CDict) ->
                dict:store(K, dict:fetch(K, DictSlice) / V, CDict);
            (K = "time", _, CDict) ->
                dict:store(K, dict:fetch(K, DictSlice), CDict);
            (K, none, CDict) ->
                dict:store(K, none, CDict);
            ("siphons", V, CDict) ->
                FunST("siphons", V, CDict);
            ("traps", V, CDict) ->
                FunST("traps", V, CDict);
            (K, V, CDict) ->
                case dict:fetch(K, DictSlice) of 
                    none -> 
                        dict:store(K, none, CDict);  
                    V ->
                        dict:store(K, {preserved, V}, CDict);
                    OV ->
                        dict:store(K, {no_preserved, V, OV}, CDict)
                end
        end,
        dict:new(),
        DictOri).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% APT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apt_properties(PN, TimeOut) ->
    Dir = PN#petri_net.dir ++ "/output/",
    AptFile = Dir ++ PN#petri_net.name ++ ".apt",
    pn_output:print_apt(PN, ""),
    Cmd = 
        "java -jar apt/apt.jar examine_pn "  ++ AptFile,
    pn_lib:flush(),
    cmd_run(Cmd, fun cmd_loop_dict_building/3, TimeOut, {none, "No analyzed (error).\n\n"}).
    % Res = os:cmd(Cmd), 
    % {parse_properties(Res), Res}.
    % {lists:foldl(fun(X, Acc) -> dict:store(X, "0", Acc) end, dict:new(), all_properties()), ""}.

apt_property(Prop, PN, TimeOut) ->
    Dir = PN#petri_net.dir ++ "/output/",
    AptFile = Dir ++ PN#petri_net.name ++ ".apt",
    pn_output:print_apt(PN, ""),
    Cmd = 
        "java -jar apt/apt.jar " ++ Prop ++ " "  ++ AptFile,
    cmd_run(Cmd, fun cmd_loop/3, TimeOut, none).


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

is_apt_property_preserved(ST, PN, PNSlice, Timeout) ->
    [PropOri, PropSlice] = 
        lists:map(
            fun(N) ->
                StrProp = 
                    string:to_lower(
                        string:substr(
                            apt_property(ST, N, Timeout), length(ST) + 11)),
                % io:format("~s\n", [StrProp]),
                % io:format("~p\n", [erl_scan:string(StrProp ++ ".")]),
                {ok, Prop} = 
                    erl_parse:parse_term(
                        element(2, erl_scan:string(StrProp ++ "."))),
                lists:sort(
                    [   lists:sort(tuple_to_list(T)) 
                    ||  T <- tuple_to_list(Prop)])
            end,
            [PN, PNSlice]
            ),
    % io:format("~p - ~p\n", [PropOri, PropSlice]),
    PropOri == PropSlice.

is_apt_property_preserved(PropOriStr, PropSliceStr, PropStr) ->
    [PropOri, PropSlice] = 
        lists:map(
            fun(StrProp0) ->
                % io:format("~s\n", [StrProp0]),
                StrProp = 
                    string:to_lower(
                        string:substr(
                            StrProp0, length(PropStr) + 11)),
                % io:format("~s\n", [StrProp]),
                % io:format("~p\n", [erl_parse:parse_term(
                %         element(2, erl_scan:string(StrProp ++ ".")))]),
                {ok, Prop} = 
                    erl_parse:parse_term(
                        element(2, erl_scan:string(StrProp ++ "."))),
                lists:sort(
                    [   lists:sort(tuple_to_list(T)) 
                    ||  T <- tuple_to_list(Prop)])
            end,
            [PropOriStr, PropSliceStr]
            ),
    % io:format("~p - ~p\n", [PropOri, PropSlice]),
    PropOri == PropSlice.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOLA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_reachable_sc([], _, _, _) ->
    false;
check_reachable_sc([SC | SCs], File, Dir, TimeOut) ->
    case check_formula("EF " ++ SC ++ " > 0", File, Dir, TimeOut) of 
        true -> 
            true;
        none ->
            check_reachable_sc(SCs, File, Dir, TimeOut);
        false ->
            check_reachable_sc(SCs, File, Dir, TimeOut)
    end.

check_formula(Formula, File, Dir, TimeOut) ->
    JSONFile = 
        Dir ++ "output.json",
    OutFile = 
        Dir ++ "output.err",
    Cmd = 
        "lola " ++ File ++ " --formula=\"" ++ Formula ++ "\" --json=" ++ JSONFile ++ " 2> " ++ OutFile,
    Res = 
        cmd_run(Cmd, fun cmd_loop/3, TimeOut, none),
    case Res of 
        none -> 
            none;
        _ -> 
            {ok, IODev} = file:open(JSONFile, [read]),
            [JSONContent|_] = pn_input:read_data(IODev),
            JSON = mochijson:decode(JSONContent),
            Answer = 
                case JSON of 
                    {struct, [{"analysis",{struct, [_, {"result", Answer0} | _]}} | _]} -> 
                        Answer0; 
                    _ -> 
                        none 
                end,
            file:close(JSONFile),
            Answer
    end.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_run(Cmd, CmdLoop, Timeout, ErrorAns) ->
    try
        Port = erlang:open_port({spawn, Cmd},[exit_status]),
        [PidOs] = 
            [PidOs0 || {os_pid,PidOs0} <- erlang:port_info(Port)],
        Res = CmdLoop(Port, [], Timeout),
        try 
            port_close(Port),
            os:cmd("kill -9 " ++ integer_to_list(PidOs))
        catch 
            _:_ ->
                ok
        end,
        Res
    catch 
        _:_ ->
            ErrorAns
    end.

cmd_loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> 
            cmd_loop(Port, [NewData | Data], Timeout);
        {Port, {exit_status, 0}} -> 
            ResAnalyses = string:join(lists:reverse(Data), ""),
            ResAnalyses;
        {Port, {exit_status, _}} -> 
            none
    after 
        Timeout ->
            none
    end.
    
cmd_loop_dict_building(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> 
            cmd_loop_dict_building(Port, [NewData | Data], Timeout);
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
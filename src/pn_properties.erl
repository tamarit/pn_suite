-module( pn_properties ).
 
-export( 
    [
        apt_properties/2, compare_properties/2, 
        all_properties/0, check_reachable_sc/3,
        parse_property_list/1, check_formula/3,
        apt_property/2
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
    cmd_run(Cmd, TimeOut).
    % Res = os:cmd(Cmd), 
    % {parse_properties(Res), Res}.
    % {lists:foldl(fun(X, Acc) -> dict:store(X, "0", Acc) end, dict:new(), all_properties()), ""}.

apt_property(Prop, PN) ->
    Dir = PN#petri_net.dir ++ "/output/",
    AptFile = Dir ++ PN#petri_net.name ++ ".apt",
    pn_output:print_apt(PN, ""),
    Cmd = 
        "java -jar apt/apt.jar " ++ Prop ++ " "  ++ AptFile,
    os:cmd(Cmd).


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOLA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

check_formula(Formula, File, Dir) ->
    JSONFile = 
        Dir ++ "output.json",
    os:cmd("lola " ++ File ++ " --formula=\"" ++ Formula ++ "\" --json=" ++ JSONFile),
    {ok, IODev} = file:open(JSONFile, [read]),
    [JSONContent|_] = pn_input:read_data(IODev),
    JSON = mochijson:decode(JSONContent),
    {struct, [{"analysis",{struct, [_, {"result", Answer} | _]}} | _]}  = 
        JSON,
    % io:format("~p\n", [Answer]),
    Answer.
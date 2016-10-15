-module( pn_suite ).
 
-export( [main/1] ).

-include("pn.hrl").
 
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Args) ->
    XML = read_xml_document(hd(Args)),
    Name = 
        read_attribute(
            hd(xmerl_xpath:string("//net", XML)),
            "id"
        ),
    Places = 
        lists:map(
            fun extract_info_place/1, 
            xmerl_xpath:string("//place", XML)
        ),
    Transitions = 
        lists:map(
            fun extract_info_transition/1, 
            xmerl_xpath:string("//transition", XML)
        ),
    Arcs = 
        lists:map(
            fun extract_info_arc/1, 
            xmerl_xpath:string("//arc", XML)
        ),
    PN = 
        #petri_net{
            name = Name,
            places = build_dict(place, Places),
            transitions = build_dict(transition, Transitions),
            arcs = Arcs
        },
    io:format("Petri net " ++ Name ++ " successfully read.\n"),
    Op1 = "Run the Petri Net",
    Op2 = "Export the Petri Net",
    Op3 = "Slicing",
    Op4 = "Slicing (for a given transition sequence)",
    Op5 = "Slicing Yu et al",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2, Op3, Op4, Op5]),
    QuestionLines = 
            ["These are the available options: " | lists:reverse(Lines)]
        ++  ["What do you want to do?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    case dict:fetch(Answer, AnsDict) of 
        Op1 ->
            build_digraph(PN),
            PNBefExec = 
                set_enabled_transitions(PN),
            FunChoose = 
                ask_mode(),
            {PNFinal, Executed} = 
                run(PNBefExec, FunChoose, []),
            io:format(
                "Execution:\n~s\n", 
                [string:join([T || {T, _} <- Executed], ",")]),
            export(PNFinal);
        Op2 ->    
            export(PN);
        Op3 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            build_digraph(PN),
            PNSlice = slice(PN, SC),
            % SDG = sdg(PN, SC),
            % SDGS = sdg_sim(PN, SC),
            % bsg(PN, SDG, SC),
            % bsg_sim(PN, SDGS, SC),
            % io:format("ResSlice:\n[~s]\n[~s]\n", [string:join(PsS, ", "), string:join(TsS, ", ")]),
            export(PNSlice);
        Op4 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            build_digraph(PN), 
            PNBefExec = 
                set_enabled_transitions(PN),
            {SCT, Exec} = 
                ask_transitions_slicing_criterion(PNBefExec),
            io:format("Slicing criterion execution: [~s]\n", [string:join(SCT, ", ")]),
            RevExec = 
                lists:reverse([{none, PNBefExec} | Exec]),
            PNSlice = 
                slice_with_sequence(RevExec, SC, []),
            export(PNSlice);
        Op5 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            build_digraph(PN),
            SDG = sdg_sim(PN, SC),
            BSG = bsg_sim(PN, SDG, SC),
            PNSlice = filter_pn(PN, sdg_places_trans(BSG)),
            export(PNSlice)
    end,
    ok.

ask_mode() ->
    Question = 
            "Available modes:\n"
        ++  "0.- Manually\n"
        ++  "n.- n random steps (at most)\n"
        ++  "How do you want to run the PN? ",
    [_|Answer0] = 
        lists:reverse(io:get_line(standard_io, Question)),
    Answer =
        lists:reverse(Answer0),
    try 
        AnsInt = list_to_integer(Answer),
        case AnsInt of 
            0 ->
                fun ask_fired_transition/2;
            N ->
                ServerSeq = 
                    spawn(fun() -> server_random(N) end),
                fun(_, Enabled) ->
                    ServerSeq!{get_next, self(), Enabled},
                    receive 
                        {next, T} ->
                            T
                    end
                end
        end
    catch 
        _:_ ->
            ask_mode()
    end.

server_random(N) when N > 0 ->
    receive 
        {get_next, PidAns, Enabled} ->
            Next = 
                case Enabled of 
                    [] ->
                        none;
                    [{T, _}] -> 
                        T;
                    _ ->
                        {T, _} = 
                            lists:nth(
                                rand:uniform(length(Enabled)), 
                                Enabled),
                        T
                end,
            PidAns!{next, Next},
            case Next of 
                none ->
                    ok;
                _ ->
                    io:format("Seleced transition: ~s\n", [Next]),
                    server_random(N - 1)
            end  
    end;
server_random(_) ->
    receive 
        {get_next, PidAns, _} ->
            PidAns!{next, none}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing criterion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_slicing_criterion(#petri_net{places = Ps}) ->
    Places0 = 
        dict:fold(
            fun(K, #place{showed_name = SN}, Acc) ->
                [SN ++ " - " ++ K | Acc]
            end,
            [],
            Ps),
    % Places = lists:sort(dict:fetch_keys(Ps)),
    Places = lists:sort(Places0),
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun build_question_option/2,
            {1, [], [], dict:new()},
            Places),
    QuestionLines = 
            ["Select one or more places (separated by spaces): " | lists:reverse(Lines)]
        ++  ["What is the slicing criterion?: "],
    Answers = 
        get_answer_multiple(
            string:join(QuestionLines,"\n"), 
            lists:seq(1, length(Ans))),
    [lists:last(string:tokens(dict:fetch(A, AnsDict), " - ")) || A <- Answers].


ask_transitions_slicing_criterion(PN) ->
    Op1 = 
        "Read from a given sequence",
    Op2 = 
        "Build a new sequence",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2]),
    QuestionLines = 
            ["A sequence of transitions is needed: " | lists:reverse(Lines)]
        ++  ["What do you want to do?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    Seq = 
        case dict:fetch(Answer, AnsDict) of 
            Op1 ->
                read_transitions_sequence();
            Op2 ->
                build_transitions_sequence(PN)
        end,
    case check_execution(PN, Seq) of 
        {ok, {_, Exec}} -> 
            {Seq, Exec};
        false ->
            io:format("The execution is not valid.\n"),
            ask_transitions_slicing_criterion(PN)
    end.

read_transitions_sequence() ->
    [_|Answer0] = 
        lists:reverse(io:get_line(standard_io, "What is the execution? ")),
    Answer =
        lists:reverse(Answer0),
    string:tokens(Answer, ",").


build_transitions_sequence(#petri_net{transitions = Ts}) ->
    Transitions0 = 
        dict:fold(
            fun(K, #transition{showed_name = SN}, Acc) ->
                [SN ++ " - " ++ K | Acc]
            end,
            [],
            Ts),
    Transitions = lists:sort(Transitions0),
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun build_question_option/2,
            {1, [], [], dict:new()},
            Transitions),
    QuestionLines = 
            ["Select one or more transitions (separated by spaces): " | lists:reverse(Lines)]
        ++  ["What is the slicing criterion?: "],
    Answers = 
        get_answer_multiple(
            string:join(QuestionLines,"\n"), 
            lists:seq(1, length(Ans))),
    [lists:last(string:tokens(dict:fetch(A, AnsDict), " - ")) || A <- lists:reverse(Answers)].

server_sequence([H|T]) ->
    receive 
        {get_next, PidAns} ->
            PidAns!{next, H},
            server_sequence(T)
    end;
server_sequence([]) ->
    receive 
        {get_next, PidAns} ->
            PidAns!{next, none}
    end.

check_execution(PN, Seq) ->
    ServerSeq = 
        spawn(fun() -> server_sequence(Seq) end),
    FunChoose = 
        fun(_, Enabled) ->
            ServerSeq!{get_next, self()},
            receive 
                {next, N} -> 
                    case N of 
                        none ->
                            none;
                        _ ->
                            case lists:member(N, [K || {K, _} <- Enabled]) of 
                                true ->
                                    N;
                                false ->
                                    error
                            end 
                    end
            end
        end,
    case run(PN, FunChoose, []) of 
        error ->
            false;
        Exec ->
            {ok, Exec}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Export functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export(PN) ->
    print_net(PN, false, "svg"),
    SVG = read_xml_document(PN#petri_net.name ++ ".svg"),
    PNtoExport = extract_positions(SVG, PN),
    Op1 = "pdf",
    Op2 = "dot",
    Op3 = "PNML compatible with PIPE",
    Op4 = "Other formats",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2, Op3, Op4]),
    QuestionLines = 
            ["These are the available output formats: " | lists:reverse(Lines)]
        ++  ["What format do you need?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    case dict:fetch(Answer, AnsDict) of 
        Op3 -> 
            print_pnml(PNtoExport);
        Op4 ->
            ask_other_formats(PNtoExport);
        Format ->
            print_net(PNtoExport, false, Format)
    end.

ask_other_formats(PN) ->
    FormatsStr = lists:map(fun atom_to_list/1, formats()),
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun build_question_option/2,
            {1, [], [], dict:new()},
            FormatsStr),
    QuestionLines = 
            ["These are all the available output formats: " | lists:reverse(Lines)]
        ++  ["What format do you need?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    print_net(PN, false, dict:fetch(Answer, AnsDict)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Execution functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(PN = #petri_net{transitions = Ts}, FunChoose, Executed) ->
    % Extract enabled
    Enabled = 
        dict:fold(
            fun
                (K, #transition{enabled = true, showed_name = SN}, Acc) -> 
                    [{K, SN ++ " - " ++ K} | Acc];
                (_, _, Acc) ->
                    Acc 
            end,
            [],
            Ts),
    case FunChoose(PN, Enabled) of 
        none ->
            {PN, lists:reverse(Executed)};
        error ->
            error;
        Chosen ->
            % execute (place update)
            NPN = fire_transition(Chosen, PN),
            % transitions update 
            NPN2 = set_enabled_transitions(NPN),
            % recursive call
            run(NPN2, FunChoose, [{Chosen, NPN2} | Executed])
    end.

ask_fired_transition(PN, Enabled) ->
    % print
    print_net(PN),
    % ask which one (with additional options like finish, etc)
    case Enabled of 
        [] ->
            none;
        [{T, SN}] -> 
            io:format("Transition ~s is chosen.\n", [SN]),
            io:get_line(standard_io, ""),
            T;
        _ ->
            SortingFun = fun({_,V1}, {_, V2}) -> V1 < V2 end,
            {_, Lines, Ans, AnsDict} = 
                lists:foldl(
                    fun build_question_option/2,
                    {1, [], [], dict:new()},
                    lists:sort(SortingFun, Enabled)),
            EnhAns = ["f" | Ans],
            EnhAnsDict = dict:store(f, none,AnsDict),
            QuestionLines = 
                    ["The following transitions are enabled:" | lists:reverse(Lines)]
                ++  ["What is the next transition to be fired?" 
                     | ["[" ++ string:join(lists:reverse(EnhAns), "/") ++ "]: "]],
            Answer = 
                get_answer(string:join(QuestionLines,"\n"), [f | lists:seq(1, length(Ans))]),
            dict:fetch(Answer, EnhAnsDict)
    end.

% random_fired_transition(PN, Enabled) ->
%     case Enabled of 
%         [] ->
%             none;
%         [{T, SN}] -> 
%             T;
%         _ ->
%             {T, SN} = lists:nth(uniform(length(Enabled)), Enabled),
%             T
%     end.

set_enabled_transitions(
    PN = #petri_net{
        places = Ps, 
        transitions = Ts,
        digraph = G}) ->
    NTs = 
        dict:map(
            fun(K, V) ->
                InputPlaces = digraph:in_neighbours(G, K),
                Enabled = 
                    case InputPlaces of 
                        [] ->
                            true;
                        _ -> 
                            lists:all(
                                fun(X) -> X end,
                                [(dict:fetch(P, Ps))#place.marking > 0
                                 || P <- InputPlaces])
                    end,
                V#transition{enabled = Enabled}
            end,
            Ts),
    PN#petri_net{transitions = NTs}.


fire_transition(
    Transition, 
    PN = #petri_net{
        places = Ps, 
        digraph = G}) ->
    PlacesToInc = 
        digraph:out_neighbours(G, Transition),
    PlacesToDec = 
        digraph:in_neighbours(G, Transition),
    NPs = 
        dict:map(
            fun(K, V) ->
                case lists:member(K, PlacesToInc) of 
                    true ->
                        V#place{
                            marking = 
                                V#place.marking + 1
                        };
                    false ->
                        V
                end
            end,
            Ps),
    NPs2 = 
        dict:map(
            fun(K, V) ->
                case lists:member(K, PlacesToDec) of 
                    true ->
                        V#place{
                            marking = 
                                V#place.marking - 1
                        };
                    false ->
                        V
                end
            end,
            NPs),
    PN#petri_net{places = NPs2}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice(PN, SC) ->
    {PsB, TsB} = backward_slice(PN, SC, [], {[], []}),

    % Without intersection
    BPN = filter_pn(PN, {PsB, TsB}),
    {PsF, TsF} = forward_slice(BPN),
    filter_pn(BPN, {PsF, TsF}).

    % With intersection
    % {PsF, TsF} = forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % filter_pn(PN, {PsSet, TsSet}).
    
backward_slice(PN = #petri_net{digraph = G}, [P | W], Done, {PsS, TsS}) ->
    InTs = 
        digraph:in_neighbours(G, P),
    InPs = 
        lists:concat([digraph:in_neighbours(G, T) || T <- InTs]),
    % ORIGINAL
    % backward_slice(PN, W -- Done, [P | Done], {InPs ++ PsS, InTs ++ TsS}).
    % MODIFIED
    backward_slice(PN, lists:usort((W ++ InPs) -- Done), [P|Done], {PsS, InTs ++ TsS});
backward_slice(_, [], Done, {PsS, TsS}) ->
    {sets:from_list(PsS ++ Done), sets:from_list(TsS)}.
    
forward_slice(PN = #petri_net{places = Ps}) ->
    StartingPs = 
        dict:fold(
            fun
                (K, #place{marking = IM}, Acc) when IM > 0 ->
                    [K | Acc];
                (_, _, Acc) ->
                    Acc
            end,
            [],
            Ps),
    % io:format("StartingPs: ~p\n", [StartingPs]),
    #petri_net{transitions = NTs} 
        = set_enabled_transitions(PN),
    StartingTs = 
        dict:fold(
            fun
                (K, #transition{enabled = true}, Acc) -> 
                    [K | Acc];
                (_, _, Acc) ->
                    Acc 
            end,
            [],
            NTs),
    % io:format("StartingTs: ~p\n", [StartingTs]),
    forward_slice(
        PN,
        sets:from_list(StartingPs), 
        sets:from_list([]), 
        sets:from_list(StartingTs)).

forward_slice(PN = #petri_net{transitions = Ts, digraph = G}, W, R, V) ->
    case sets:to_list(V) of 
        [] ->
            {W, R};
        _ ->
            OutV = 
                lists:append(
                    [digraph:out_neighbours(G, P) 
                    || P <- sets:to_list(V)]),
            NW = sets:union(W, sets:from_list(OutV)),
            NR = sets:union(R, V),
            NV0 = 
                dict:fold(
                    fun(K, _, Acc) ->
                        case sets:is_element(K, NR) of 
                            true ->
                                Acc;
                            false ->
                                InK = 
                                    sets:from_list(
                                        digraph:in_neighbours(G, K)),
                                case sets:is_subset(InK, NW) of 
                                    true ->
                                        [K | Acc];
                                    false ->
                                        Acc
                                end
                        end
                    end,
                    [],
                    Ts),
            NV = sets:from_list(NV0),
            forward_slice(PN, NW, NR, NV)
    end.

filter_pn(
    PN = #petri_net{
        places = Ps, 
        transitions = Ts,
        arcs = As},
    {PsSet, TsSet}) ->
    FunFilter = 
        fun(Dict, Set) ->
            dict:fold(
                fun(K, V, CDict) ->
                    case sets:is_element(K, Set) of 
                        true ->
                            dict:store(K, V, CDict);
                        false ->
                            CDict
                    end
                end,
                dict:new(),
                Dict)
        end,
    PsS = FunFilter(Ps, PsSet),
    TsS = FunFilter(Ts, TsSet),
    AsS = 
        lists:foldl(
            fun(A = #arc{source = S, target = T}, Acc) ->
                case
                    (sets:is_element(S, PsSet) or sets:is_element(S, TsSet))
                    and
                    (sets:is_element(T, PsSet) or sets:is_element(T, TsSet))
                of 
                    true ->
                        [A | Acc];
                    false ->
                        Acc
                end
            end,
            [],
            As),
    PN#petri_net
        {
            places = PsS,
            transitions= TsS,
            arcs = AsS
        }.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % Slicing Improved
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% slice_imp(PN, SC) ->
%     {PsB, TsB} = backward_slice_imp(PN, SC, [], {[], []}),

%     % Without intersection
%     BPN = filter_pn(PN, {PsB, TsB}),
%     {PsF, TsF} = forward_slice_imp(BPN),
%     filter_pn(BPN, {PsF, TsF}).
    
%     % With intersection
%     % {PsF, TsF} = forward_slice(PN),
%     % PsSet = sets:intersection(PsB, PsF),
%     % TsSet = sets:intersection(TsB, TsF),
%     % filter_pn(PN, {PsSet, TsSet}).
    
% backward_slice_imp(PN = #petri_net{digraph = G}, [P | W], Done, {PsS, TsS}) ->
%     InTs = 
%         digraph:in_neighbours(G, P),
%     case InTs of 
%         [] -> 
%             backward_slice_imp(PN, lists:usort(W -- Done), [P | Done], {PsS, TsS});
%         [T] ->
%             InPs = digraph:in_neighbours(G, T),
%             backward_slice_imp(PN, lists:usort((W ++ InPs) -- Done), [P | Done], {PsS, [T | TsS]});
%         _ ->
%            % Anaydir bifurcacio
%            lists:foldl(
%                 fun({CPsS, CTsS, CDone}) ->
                    
%                 end
%                 {PsS, TsS, Done},
%                 InTs
%                 ),
%            backward_slice_imp(PN, lists:usort(W -- Done), [P | Done], {PsS, TsS});
%     % InPs = 
%     %     lists:concat([digraph:in_neighbours(G, T) || T <- InTs]),
%     % backward_slice_imp(PN, lists:usort((W ++ InPs) -- Done), [P|Done], {PsS, InTs ++ TsS});
% backward_slice_imp(_, [], Done, {PsS, TsS}) ->
%     {sets:from_list(PsS ++ Done), sets:from_list(TsS)}.
    
% forward_slice_imp(PN = #petri_net{places = Ps}) ->
%     StartingPs = 
%         dict:fold(
%             fun
%                 (K, #place{marking = IM}, Acc) when IM > 0 ->
%                     [K | Acc];
%                 (_, _, Acc) ->
%                     Acc
%             end,
%             [],
%             Ps),
%     % io:format("StartingPs: ~p\n", [StartingPs]),
%     #petri_net{transitions = NTs} 
%         = set_enabled_transitions(PN),
%     StartingTs = 
%         dict:fold(
%             fun
%                 (K, #transition{enabled = true}, Acc) -> 
%                     [K | Acc];
%                 (_, _, Acc) ->
%                     Acc 
%             end,
%             [],
%             NTs),
%     % io:format("StartingTs: ~p\n", [StartingTs]),
%     forward_slice_imp(
%         PN,
%         sets:from_list(StartingPs), 
%         sets:from_list([]), 
%         sets:from_list(StartingTs)).

% forward_slice_imp(PN = #petri_net{transitions = Ts, digraph = G}, W, R, V) ->
%     case sets:to_list(V) of 
%         [] ->
%             {W, R};
%         _ ->
%             OutV = 
%                 lists:append(
%                     [digraph:out_neighbours(G, P) 
%                     || P <- sets:to_list(V)]),
%             NW = sets:union(W, sets:from_list(OutV)),
%             NR = sets:union(R, V),
%             NV0 = 
%                 dict:fold(
%                     fun(K, _, Acc) ->
%                         case sets:is_element(K, NR) of 
%                             true ->
%                                 Acc;
%                             false ->
%                                 InK = 
%                                     sets:from_list(
%                                         digraph:in_neighbours(G, K)),
%                                 case sets:is_subset(InK, NW) of 
%                                     true ->
%                                         [K | Acc];
%                                     false ->
%                                         Acc
%                                 end
%                         end
%                     end,
%                     [],
%                     Ts),
%             NV = sets:from_list(NV0),
%             forward_slice_imp(PN, NW, NR, NV)
%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing (sequence trans.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_with_sequence(
    [{T_i, #petri_net{places = Ps_i, digraph = G}}, 
     {T_i_1, PN_i_1 = #petri_net{places = Ps_i_1}} 
     | T], 
    W,
    Slice) ->
    ChangingPs = 
        lists:foldl(
            fun(P, Acc) ->
                M_i = 
                    (dict:fetch(P, Ps_i))#place.marking,
                M_i_1 = 
                    (dict:fetch(P, Ps_i_1))#place.marking,
                % ORIGINAL
                % case (M_i_1 < M_i) of 
                % MODIFIED. Is what we want?
                case (M_i_1 =< M_i) of 
                    true ->
                        [P | Acc];
                    false ->
                        Acc
                end
            end,
            [],
            W),
    case ChangingPs of 
        [] ->
            slice_with_sequence([{T_i_1, PN_i_1} | T], W, Slice);
        _ ->
            InT_i = digraph:in_neighbours(G, T_i),
            slice_with_sequence(
                [{T_i_1, PN_i_1} | T], 
                lists:usort(W ++ InT_i), 
                [T_i | Slice])
    end;
slice_with_sequence(
    [{none, PN}], Ps, Ts) ->
    filter_pn(PN, {sets:from_list(Ps), sets:from_list(Ts)}).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % SDG
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%


% sdg(PN, SC) ->
%     SDG0 = #sdg{},
%     Nodes = [[P] || P <- SC],
%     [digraph:add_vertex(SDG0#sdg.digraph, N) 
%      || N <- Nodes],
%     SDG = SDG0#sdg{structural_nodes = [Nodes]},
%     FSDG = sdg_while(PN, SDG, Nodes),
%     file:write_file(
%         "sdg.dot", 
%         list_to_binary(sdg_to_dot(FSDG))),
%     FSDG.


% sdg_while(PN, SDG, [Q | T]) ->
%     % io:format("Entra amb ~p\n", [Q]),
%     {NSDG, NPending} = 
%         lists:foldl(
%             fun
%                 (P_i, {CSDG, CPending}) -> 
%                     sdg_forEveryPN([P_i], PN, CSDG, CPending)
%             end, 
%             {SDG, T}, 
%             Q),
%     sdg_while(PN, NSDG, NPending);
% sdg_while(_, SDG, []) ->
%     SDG.

% sdg_forEveryPN([P_i], PN = #petri_net{digraph = G}, SDG, Pending) ->
%     % io:format("P_i: ~p, Ts ~p\n", [P_i, digraph:in_neighbours(G, P_i)]),
%     lists:foldl(
%         fun(T_i, {CSDG, CPending}) -> 
%             sdg_forEveryT(T_i, [P_i], PN, CSDG, CPending) 
%         end, 
%         {SDG, Pending}, 
%         digraph:in_neighbours(G, P_i)).


% sdg_forEveryT(T_i, P_i, #petri_net{digraph = G}, FullSDG = #sdg{digraph = SDG}, Pending) ->
%     Q_ = lists:sort(digraph:in_neighbours(G, T_i)),
%     % io:format("P_i: ~p, T_i ~p, Q_ ~p\n", [P_i, T_i, Q_]),
%     Vs = digraph:vertices(SDG),
%     case search_equal(Vs, Q_) of 
%         {ok, N} ->
%             digraph:add_edge(SDG, N, P_i, T_i),
%             {FullSDG, Pending};
%         none ->
%             case search_intersection(Vs, Q_) of 
%                 {ok, N} ->
%                     digraph:add_vertex(SDG, Q_),
%                     [digraph:add_vertex(SDG, [P]) || P <- Q_],
%                     digraph:add_edge(SDG, Q_, P_i, T_i),
%                     {
%                         add_to_structurals(FullSDG, lists:usort([N, Q_] ++ [[P] || P <- Q_])),
%                         [[PQ_] || PQ_ <- Q_] ++ Pending
%                     };
%                 none ->
%                     digraph:add_vertex(SDG, Q_),
%                     [digraph:add_vertex(SDG, [P]) || P <- Q_],
%                     digraph:add_edge(SDG, Q_, P_i, T_i),
%                     {
%                         add_to_structurals(FullSDG, lists:usort([Q_ | [[P] || P <- Q_]])),
%                         [[PQ_] || PQ_ <- Q_] ++ Pending
%                     }
%             end
%     end.

% search_equal([C | _], C) ->
%     {ok, C};
% search_equal([_ | T], Q) ->
%     search_equal(T, Q);
% search_equal([], _) ->
%     none.

% search_intersection([C | T], Q) ->
%     case [P1 || P1 <- Q, P2 <- C, P1 == P2] of 
%         [] ->
%             search_intersection(T, Q);
%         _ ->
%             {ok, C} 
%     end;
% search_intersection([], _) ->
%     none.

% add_to_structurals(SDG = #sdg{structural_nodes = SN}, N) ->
%     case [Ns|| Ns <- SN, sets:intersection(sets:from_list(Ns), sets:from_list(N)) /= sets:new()] of 
%         [Ns] ->
%             SDG#sdg{structural_nodes = [lists:usort(N ++ Ns) | (SN -- [Ns])]};
%         _ ->
%             SDG#sdg{structural_nodes = [N | SN]}
%     end.



% sdg_to_dot(#sdg{digraph = G, structural_nodes = SN}) ->
%     % Vs = 
%     %     digraph:vertices(G),
%     Es = 
%         lists:usort(
%             [begin 
%                 {_, S, T, Label} = digraph:edge(G, E), 
%                 {S, T, Label} 
%              end 
%             || E <- digraph:edges(G)]),
%     {SNDot, _} = 
%         lists:mapfoldl(fun sdg_sn_to_dot/2, 1, SN),
%     EsDot = 
%         lists:map(fun sdg_edge_to_dot/1, Es),
%         "digraph \"SDG\"{\n"
%     ++  "ordering=out; ranksep=0.5;\n"
%     ++  SNDot ++ "\n"
%     ++  string:join(EsDot,"\n")
%     ++ "\n}"
%     .

% places_to_string(Ps) ->
%     string:join(Ps, "_").

% places_to_string_pp(Ps) ->
%     "[" ++ string:join(Ps, ", ") ++ "]".

% sdg_node_to_dot(Ps) ->
%     StrPs = places_to_string(Ps),
%     StrPsPP = places_to_string_pp(Ps),
%         StrPs 
%     ++ " [shape=ellipse, label=\"" ++ StrPsPP
%     ++  "\"];".

% sdg_sn_to_dot(SN, N) ->
%     SNDot = lists:map(fun sdg_node_to_dot/1, SN),
%     DotCluster = 
%             "subgraph cluster" ++ integer_to_list(N) ++ "{"
%         ++  string:join(SNDot,"\n") ++ "\n"
%         ++  "}",
%     {DotCluster, N + 1}.

% sdg_edge_to_dot({S, T, Label}) ->
%     places_to_string(S) ++ " -> "  ++ places_to_string(T) 
%     ++ " [label=\"" ++ Label
%     ++  "\"];".


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % BSG
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bsg(#petri_net{places = Ps}, #sdg{digraph = G_SDG, structural_nodes = SN}, SC) ->
%     % S = R' = (nodos en R con tokens) U (nodos compuestos cuyos todos sus lugares tiene tokens)
%     S0 = 
%         lists:concat([
%                     [ Node 
%                     ||  Node <- Group, 
%                         lists:all(
%                             fun(P) -> 
%                                 (dict:fetch(P, Ps))#place.marking > 0 
%                             end, 
%                             Node)] 
%                 || Group <- SN]),
%     % S =
%     %     lists:concat([sn_of_node(SN, V) || V <- S0]), 
%     S = S0,
%     % io:format("S: ~p\n", [S]),
%     BSG = #sdg{},
%     G_BSG = BSG#sdg.digraph,
%     lists:map(
%         fun(N) -> 
%             digraph:add_vertex(G_BSG, N) 
%         end, 
%         S),
%     bsg_while_S(G_SDG, G_BSG, SN, S),
%     Vs = digraph:vertices(G_BSG),
%     % if \forall r \in R' -> r \not\in SlicingCriterion
%         % BSG = \empty
%     InSC = [[P || P <- V, lists:member(P, SC)] || V <- Vs],
%     case InSC of 
%         [] ->
%             #sdg{};
%         _ ->  
%             NSN = 
%                 [[N || N <- Group, lists:member(N, Vs)] || Group <- SN],
%             % ConcatNSN = 
%             %     lists:usort(lists:concat(NSN)),
%             % io:format("ConcatNSN: ~p\n", [ConcatNSN]),
%             % [
%             %     [ begin 
%             %         {E, V, Target, Label} = digraph:edge(G_SDG, E), 
%             %         io:format("Target: ~p\n", [Target]),
%             %         case lists:member(Target, ConcatNSN) of 
%             %             true -> 
%             %                 digraph:add_edge(G_BSG, V, Target, Label); 
%             %             false -> 
%             %                 ok 
%             %         end 
%             %      end 
%             %     || E <- digraph:out_edges(G_SDG, V)] 
%             % || V <- ConcatNSN],
%             FBSG = BSG#sdg{structural_nodes = NSN},
%             file:write_file(
%                 "bsg.dot", 
%                 list_to_binary(sdg_to_dot(FBSG))),
%             % clean structural_nodes leaving only the nodes in R'
%             FBSG
%     end.

% % while (S no est evacio y S tenga successor nodes)
%     % foreach s_i \in S
%         % Elegir un t_i de los arcos de salida de s_i
%             % foreach s_j al que se llega desde el arco etiquetado con t_i
% bsg_while_S(G_SDG, G_BSG, SN, [S_i | S0]) -> 
%     % io:format("Entra : ~p\n", [S_i]),
%     digraph:add_vertex(G_BSG, S_i),
%     OE = digraph:out_edges(G_SDG, S_i),

%     R = digraph:vertices(G_SDG),
%     R_ = digraph:vertices(G_BSG),
%     SKs0 = [N || N <- R, sets:intersection(sets:from_list(S_i), sets:from_list(N)) /= sets:new()] -- [S_i],
%     % io:format("SKs0: ~p\n", [{S_i,R, SKs0}]),
%     % io:format("R_: ~p\n", [R_]),
%     SKs = [S_k || S_k <- SKs0, lists:member(S_k -- S_i, R_)],
%     % io:format("SKs: ~p\n", [SKs]),
%     S = 
%         case SKs of 
%             [] ->
%                 S0;
%             [S_k | _] ->
%                 digraph:add_vertex(G_BSG, S_k),
%                 % FromSNk = sn_of_node(SN, S_k),
%                 FromSNk = [S_k],
%                 lists:usort(FromSNk ++ S0) 
%         end,

%     % S = S0,

%     % OE_Info = 
%     %     [digraph:edge(G_SDG, E) || E <- OE],
%     case OE of 
%         [] -> 
%             bsg_while_S(G_SDG, G_BSG, SN, S);
%         [OneOE|_] ->
%             {OneOE, S_i, _, T_i} = digraph:edge(G_SDG, OneOE),
%             SJs = 
%                 [element(3, digraph:edge(G_SDG, E)) 
%                  || E <- digraph:edges(G_SDG), 
%                     element(4, digraph:edge(G_SDG, E)) == T_i],
%             NS = 
%                 lists:foldl(
%                     fun (S_j, CS) ->
%                         bsg_process_s_j(G_SDG, G_BSG, SN, S_i, S_j, T_i, CS)
%                     end,
%                     S,
%                     SJs),
%             bsg_while_S(G_SDG, G_BSG, SN, NS)
%     end;
% bsg_while_S(_, _, _, []) -> 
%     ok.


% bsg_process_s_j(G_SDG, G_BSG, _, S_i, S_j, T_i, S0) ->
%     % io:format("S_j: ~p\n", [S_j]),
%     % R = digraph:vertices(G_SDG),
%     % R_ = digraph:vertices(G_BSG),
%     % SKs0 = [N || N <- R, sets:intersection(sets:from_list(S_j), sets:from_list(N)) /= sets:new()] -- [S_j],
%     % io:format("SKs0: ~p\n", [{S_j,R, SKs0}]),
%     % SKs = [S_k || S_k <- SKs0, lists:member(S_k -- [S_j], R_)],
%     % io:format("SKs: ~p\n", [SKs]),
%     % S = 
%     %     case SKs of 
%     %         [] ->
%     %             S0;
%     %         [S_k | _] ->
%     %             digraph:add_vertex(G_BSG, S_k),
%     %             % FromSNk = sn_of_node(SN, S_k),
%     %             FromSNk = [S_k],
%     %             lists:usort(FromSNk ++ S0) 
%     %     end,

%     S = S0,

%     case digraph:vertex(G_BSG, S_j) of 
%         {S_j, _} ->
%             case [E || E <- digraph:out_edges(G_BSG, S_i), element(3, digraph:edge(G_SDG, E)) ==  S_j] of 
%                 [] ->
%                     digraph:add_edge(G_BSG, S_i, S_j, T_i),
%                     % io:format("No estava arc\n"),
%                     S;
%                 _ ->
%                     % io:format("Res que añadir\n"),
%                     S 
%             end;
%         false ->    
%             % io:format("Tot nou\n"),
%             digraph:add_vertex(G_BSG, S_j),
%             digraph:add_edge(G_BSG, S_i, S_j, T_i),
%             % FromSNj = sn_of_node(SN, S_j),
%             FromSNj = [S_j],
%             lists:usort(FromSNj ++ S) 
%     end.
%     % case s_j \not\in R' of 
%         % true ->
%             % R' = R' U {s_j} 
%             % S = S U {s_j}
%             % Añadir a E' el arco s_i -t_i-> s_j 
%         % false ->
%             % case s_j \in R' and (s_i, s_j) \not\in E' of 
%                 % true ->
%                     % Añadir a E' el arco s_i -t_i-> s_j 
%                 % false ->
%                     % case s_j is part of of an s_k \in R and (s_k - s_j) \in R' of
%                         % true ->
%                             % R' = R' U {s_k}
%                             % S = S U {s_k}
%                         % false -> 
%                             % do_nothing
%     % end


% % sn_of_node(SNs, N) ->
% %     hd([SN || SN <- SNs, lists:member(N, SN)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SDG Simplification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sdg_sim(PN, SC) ->
    SDG = #sdg{},
    Nodes = [[P] || P <- SC],
    [digraph:add_vertex(SDG#sdg.digraph, N) 
     || N <- Nodes],
    sdg_process_sim(PN#petri_net.digraph, SDG#sdg.digraph, Nodes),
    % file:write_file(
    %     "sdg_sim.dot", 
    %     list_to_binary(sdg_to_dot_sim(SDG))),
    SDG.

sdg_process_sim(G, SDG, [Q | Pending]) ->
    NPending = 
        lists:foldl(
            fun
                (P_i, CPending1) -> 
                    lists:foldl(
                        fun(T_i, CPending2) -> 
                            sdg_add_edges(T_i, [P_i], G, SDG, CPending2) 
                        end, 
                        CPending1, 
                        digraph:in_neighbours(G, P_i))
            end, 
            Pending, 
            Q),
    % io:format("Entra amb Q: ~p\n", [{Q, NPending}]),
    sdg_process_sim(G, SDG, NPending);
sdg_process_sim(_, _, []) ->
    ok.

sdg_add_edges(T_i, P_i, G, SDG, Pending) ->
    Q_ = lists:sort(digraph:in_neighbours(G, T_i)),
    IsIn = digraph:vertex(SDG, Q_),
    digraph:add_vertex(SDG, Q_),
    [digraph:add_vertex(SDG, [P]) || P <- Q_],
    digraph:add_edge(SDG, Q_, P_i, T_i),
    case IsIn of 
        {Q_, _} ->
            Pending;
        false ->

            [[P] || P <- Q_] ++ Pending
    end.

% Simplemente tira hacia atras desde el SC, y va metiendo los lugares y transiciones que se encuentra hasta que no puede tirar mas hacia atras
% la unica diferencia con el nuestro es que mete un nodo con todos los lugares a los cuales se llega desde uan transición especifica.

% sdg_to_dot_sim(#sdg{digraph = G}) ->
%     Vs = 
%         digraph:vertices(G),
%     Es = 
%         lists:usort([begin {_, S, T, Label} = digraph:edge(G, E), {S, T, Label} end || E <- digraph:edges(G)]),
%     VsDot = 
%         lists:map(fun sdg_node_to_dot/1, Vs),
%     EsDot = 
%         lists:map(fun sdg_edge_to_dot/1, Es),

%         "digraph \"SDG\"{\n"
%     ++  "ordering=out; ranksep=0.5;\n"
%     ++  string:join(VsDot,"\n") ++ "\n"
%     ++  string:join(EsDot,"\n") ++ "\n"
%     ++ "}"
%     .


sdg_places_trans(#sdg{digraph = G}) ->
    PsInSDG = 
        lists:concat([[P || P <- V ] || V <- digraph:vertices(G)]),
    % io:format("~p\n", [PsInSDG]),
    Ps = 
        sets:from_list(PsInSDG),
    Ts = 
        sets:from_list(
            [element(4, digraph:edge(G, E))
            || E <- digraph:edges(G)]),
    {Ps, Ts}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % BSG Simplification
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%

bsg_sim(#petri_net{places = Ps}, #sdg{digraph = SDG}, SC) ->
    S = 
        [ V ||  V <- digraph:vertices(SDG),                         
                lists:all(
                    fun(P) -> 
                        (dict:fetch(P, Ps))#place.marking > 0 
                    end, 
                    V)],
    % io:format("S: ~p\n", [S]),
    BSG = #sdg{},
    G_BSG = BSG#sdg.digraph,
    lists:map(
        fun(N) -> 
            digraph:add_vertex(G_BSG, N) 
        end, 
        S),
    bsg_while_S_sim(SDG, G_BSG, S),
    Vs = digraph:vertices(G_BSG),
    InSC = [[P || P <- V, lists:member(P, SC)] || V <- Vs],
    case InSC of 
        [] ->
            #sdg{};
        _ ->  
            % file:write_file(
            %     "bsg_sim.dot", 
            %     list_to_binary(sdg_to_dot_sim(BSG))),
            BSG
    end.

% Partir de nodes amb marcat

% Cuando procesa un lugar coge tanto ese lugar como todos los lugares a los que se puede llegar con las transciones del bslice que salgan de ese lugar 
% Si p_i -> t -> * coger todas las p_k tal que p_k -> t -> *, es decir todos los lugares que hacen falta para habilitar la transicion
% para toda transicion y para todo lugar desde esa transicion añadirla. Los lugares que no estuviesen hay que recorrelos

bsg_while_S_sim(G_SDG, G_BSG, [S_i | S0]) -> 
    % io:format("Entra : ~p\n", [S_i]),
    digraph:add_vertex(G_BSG, S_i),
    R = digraph:vertices(G_SDG),
    R_ = digraph:vertices(G_BSG),
    SKs0 = [N || N <- R, sets:intersection(sets:from_list(S_i), sets:from_list(N)) /= sets:new()],
    % io:format("SKs0: ~p\n", [{S_i,R, SKs0}]),
    % io:format("R_: ~p\n", [R_]),
    SKs = [begin digraph:add_vertex(G_BSG, S_k), S_k end  || S_k <- SKs0, lists:member(S_k -- S_i, R_)],
    % io:format("SKs: ~p\n", [SKs]),
    S = SKs ++ S0,
    OE = digraph:out_edges(G_SDG, S_i),
    case OE of 
        [] -> 
            bsg_while_S_sim(G_SDG, G_BSG, S);
        _ ->
            NS =
                lists:foldl(
                    fun(OneOE, CS1) ->
                        {OneOE, S_i, _, T_i} = digraph:edge(G_SDG, OneOE),
                        SJs = 
                            [element(3, digraph:edge(G_SDG, E)) 
                             || E <- digraph:edges(G_SDG), 
                                element(4, digraph:edge(G_SDG, E)) == T_i],
                        lists:foldl(
                            fun (S_j, CS2) ->
                                bsg_process_s_j_sim(G_BSG, S_i, S_j, T_i, CS2)
                            end,
                            CS1,
                            SJs)
                    end,
                    S,
                    % OE),
                    % [lists:last(OE)]),
                    [hd(OE)]),
            bsg_while_S_sim(G_SDG, G_BSG, NS)
    end;
bsg_while_S_sim(_, _, []) -> 
    ok.


bsg_process_s_j_sim(G_BSG, S_i, S_j, T_i, S) ->
    % io:format("S_j: ~p\n", [S_j]),
    IsIn = digraph:vertex(G_BSG, S_j),
    digraph:add_vertex(G_BSG, S_j),
    digraph:add_edge(G_BSG, S_i, S_j, T_i),
    case IsIn of 
        {S_j, _} ->
            S;
        false ->    
            lists:usort([S_j | S]) 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build internal structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_dict(Type, List) ->
    lists:foldl(
        fun(E, CDict) ->
            dict:store(extract_key(Type, E), E, CDict)
        end,
    dict:new(),
    List).

extract_key(place, E) ->
    E#place.name;
extract_key(transition, E) ->
    E#transition.name.

build_digraph(
    #petri_net{
        places = Ps0, 
        transitions = Ts0,
        arcs = As,
        digraph = G}) ->
    Ps = get_value_list_from_dict(Ps0),
    Ts = get_value_list_from_dict(Ts0),
    lists:map(
        fun (P) -> 
            digraph:add_vertex(G, P#place.name)
        end,
        Ps),
    lists:map(
        fun (T) -> 
            digraph:add_vertex(G, T#transition.name)
        end,
        Ts),
    lists:map(
        fun (A) -> 
            digraph:add_edge(G, A#arc.source, A#arc.target)
        end,
        As).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info extractors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_positions(
    SVG,
    PN = #petri_net{
        places = Ps, 
        transitions = Ts}) ->
    Gs = xmerl_xpath:string("//*[local-name() = 'g']", SVG),
    ContentsGs = [C || #xmlElement{content = C} <- Gs],
    NPs = 
        dict:map(
            fun(K, V) ->
                Pos = extract_position_contents(K, ellipse, cx, cy, ContentsGs),
                V#place{position = Pos}
            end,
            Ps
            ),
    NTs = 
        dict:map(
            fun(K, V) ->
                Pos = extract_position_contents(K, text, x, y, ContentsGs),
                V#transition{position = Pos}
            end,
            Ts
            ),
    PN#petri_net{places = NPs, transitions = NTs}.

extract_position_contents(K, Fig, X, Y, [Cs | Tail]) ->
    case 
        [0 
        ||  #xmlElement{
                name = title, 
                content = [#xmlText{value = Text}]} <- Cs, 
            Text == K] 
    of 
        [] ->
            extract_position_contents(K, Fig, X, Y, Tail);
        _ ->
            [Atts] = 
                [Atts0 
                ||  #xmlElement{name = Fig0, attributes = Atts0} <- Cs, 
                    Fig == Fig0],
            [CX] = 
                [V || #xmlAttribute{name = X0, value = V} <- Atts, X0 == X],
            [[_|CY]] = 
                [V || #xmlAttribute{name = Y0, value = V} <- Atts, Y0 == Y],
            {CX, CY}
    end;
extract_position_contents(_, _, _, _, []) ->
    {"0", "0"}.

extract_info_place(T) ->
    Name = 
        string:strip(read_attribute(T, "id"), both, $ ),
    #place{
        name = 
            Name,
        showed_name = 
            string:strip(read_value_or_text(T, "name", Name)),
        marking = 
            str2int(case string:tokens(read_value_or_text(T, "initialMarking", "0"), ",") of 
                [H] ->
                    H;
                L ->
                    lists:last(L)
            end)
    }.

extract_info_transition(T) ->
    #transition{
        name = 
            string:strip(read_attribute(T, "id"), both, $ ),
        showed_name = 
            string:strip(
                read_value_or_text(T, "name", ?UNNAMED), 
                both, 
                $ )
    }.

extract_info_arc(T) ->
    #arc{
        name = 
            string:strip(read_attribute(T, "id"), both, $ ),
        source =  
            string:strip(read_attribute(T, "source"), both, $ ),
        target = 
            string:strip(read_attribute(T, "target"), both, $ )
    }.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helping functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_attribute(T, Att) ->
    (hd(xmerl_xpath:string("//@" ++ Att, T)))#xmlAttribute.value.

read_value_or_text(T, Tag, DefaultValue) ->
    case xmerl_xpath:string("//" ++ Tag ++ "/text", T) of 
        [V] ->
            case V#xmlElement.content of 
                [HC|_] ->
                    HC#xmlText.value;
                _ ->
                    DefaultValue
            end;
        [] ->
            case xmerl_xpath:string("//" ++ Tag ++ "/value", T)  of 
                [V] ->
                    case V#xmlElement.content of 
                        [HC|_] ->
                            HC#xmlText.value; 
                        _ ->
                            DefaultValue 
                    end;
                [] ->
                    DefaultValue
            end
    end.

str2int(Str) ->
    element(1,string:to_integer(Str)).

get_value_list_from_dict(Dict) ->
    lists:map(
        fun({_,V}) ->
            V
        end,
        dict:to_list(Dict)).

get_answer(Message, Answers) ->
   [_|Answer] = 
     lists:reverse(io:get_line(standard_io, Message)),
   AtomAnswer = 
        try 
            list_to_integer(lists:reverse(Answer))
        catch 
            _:_ ->
                try 
                    list_to_atom(lists:reverse(Answer))
                catch 
                    _:_ -> get_answer(Message,Answers)
                end
        end,
   case lists:member(AtomAnswer, Answers) of
        true -> AtomAnswer;
        false -> get_answer(Message, Answers)
   end.

get_answer_multiple(Message, Answers) ->
   [_|Answer] = 
     lists:reverse(io:get_line(standard_io, Message)),
   As = string:tokens(lists:reverse(Answer), " ,"),
   ValidAs = 
       lists:foldl(
            fun(A, Acc) ->
                check_answer(A, Answers, Acc)
            end,
            [],
            As),
   case ValidAs of 
        [] -> 
            get_answer_multiple(Message, Answers);
        _ ->
            ValidAs
    end.


check_answer(A, Answers, Acc) ->
    AtomAnswer = 
        try 
            list_to_integer(A)
        catch 
            _:_ ->
                try 
                    list_to_atom(A)
                catch 
                    _:_ -> none
                end
        end,
   case lists:member(AtomAnswer, Answers) of
        true -> [AtomAnswer | Acc];
        false -> []
   end.

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

build_question_option({O, Name}, {N, Lines, Answers, Dict}) ->
    NLines = 
        [format("~p .- ~s", [N, Name]) |Lines],
    {N + 1, NLines, [format("~p", [N]) | Answers], dict:store(N, O, Dict)};
build_question_option(Other, {N, Lines, Answers, Dict}) ->
    build_question_option({Other, Other}, {N, Lines, Answers, Dict}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_dot(
    #petri_net{
        name = Name,
        places = Ps0, 
        transitions = Ts0,
        arcs = As},
    ShowEnabled) ->

    Ps = get_value_list_from_dict(Ps0),
    Ts = get_value_list_from_dict(Ts0),

    PsDot = 
        lists:map(fun place_to_dot/1, Ps),
    TsDot = 
        lists:map(
            fun(T) -> 
                transition_to_dot(T, ShowEnabled) 
            end, 
            Ts),
    AsDot = 
        lists:map(fun arc_to_dot/1, As),

        "digraph \"" ++ Name ++ "\"{\n"
    ++  "ordering=out; ranksep=0.5;\n" % root=false 0;\n",
    ++  string:join(PsDot,"\n")
    ++  string:join(TsDot,"\n")
    ++  string:join(AsDot,"\n")
    ++ "\n}"
    .

place_to_dot(
    #place
    {
        name = N,
        showed_name = SN,
        marking = IM
    }) ->
    Filled = 
        case IM of 
            0 -> 
                "";
            _ -> 
                " style=filled color=\"blue\" fontcolor=\"white\" fillcolor=\"blue\""
        end,
        N ++ " [shape=ellipse, label=\"" ++ SN ++ " - " ++ N
    ++  "\\l(" ++ integer_to_list(IM) ++ ")\""++ Filled ++ "];".

transition_to_dot(
    #transition
    {
        name = N,
        showed_name = SN,
        enabled = Enabled
    },
    ShowEnabled) ->
    Filled = 
        case (ShowEnabled and Enabled) of 
            true -> 
                " style=filled color=\"red\" fontcolor=\"white\" fillcolor=\"red\"";
            false -> 
                ""
        end,
    N ++ " [shape=box, label=\"" ++ SN ++ " - "  ++ N ++ "\"" ++ Filled ++ "];".

arc_to_dot(
    #arc
    {
        source = S,
        target = T
    }) ->
    S ++ " -> "  ++ T.

print_net(PN) ->
    print_net(PN, true, "pdf").   

print_net(PN, ShowEnabled, Format) ->
    file:write_file(
        PN#petri_net.name ++ ".dot", 
        list_to_binary(to_dot(PN, ShowEnabled))),
    os:cmd(
            "dot -T" ++ Format ++ " "++ PN#petri_net.name ++ ".dot > "
        ++  PN#petri_net.name ++"." ++ Format).

formats() ->
    [bmp, canon, cgimage, cmap, cmapx, cmapx_np, dot, 
     eps, exr, fig, gd, gd2, gif, gv, icns, ico, imap, 
     imap_np, ismap, jp2, jpe, jpeg, jpg, pct, pdf, pic, 
     pict, plain, 'plain-ext', png, pov, ps, ps2, psd, sgi, 
     svg, svgz, tga, tif, tiff, tk, vml, vmlz, vrml, 
     webp, x11, xdot, 'xdot1.2', 'xdot1.4', xlib].

% print_all_formats(PN) ->
%     Formats = formats(),
%     lists:map(
%         fun(Format) -> 
%             StrFormat = atom_to_list(Format),
%             os:cmd("dot -T" ++ StrFormat ++ " "++ PN#petri_net.name ++".dot > formats/"++ PN#petri_net.name ++"." ++ StrFormat) 
%         end,
%         Formats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PNML  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_pnml(PN) ->
    file:write_file(
        PN#petri_net.name ++ "_PIPE.pnml", 
        list_to_binary(to_pnml(PN))).

to_pnml(
    #petri_net{
        name = Name,
        places = Ps0, 
        transitions = Ts0,
        arcs = As}) ->
    Header = 
        ["<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>",
         "<pnml>",
         "<net id=\"" ++ Name ++ "\" type=\"P/T net\">"],
    Foot = 
        ["</net>", "</pnml>"],
    Ps = get_value_list_from_dict(Ps0),
    Ts = get_value_list_from_dict(Ts0),

    PsPNML = 
        lists:map(fun place_to_pnml/1, Ps),
    TsPNML = 
        lists:map(fun transition_to_pnml/1, Ts),
    AsPNML = 
        lists:map(fun arc_to_pnml/1, As),
    string:join(
            Header 
        ++ lists:append(PsPNML)
        ++ lists:append(TsPNML)
        ++ lists:append(AsPNML)
        ++ Foot,
        "\n").

place_to_pnml(
    #place
    {
        name = N,
        showed_name = SN,
        marking = IM,
        position = {X, Y}
    }) ->
    [
        "\t<place id=\"" ++ N ++ "\">",
        "\t\t<graphics>",
        "\t\t\t<position x=\"" ++ X ++ "\" y=\"" ++ Y ++ "\"/>",
        "\t\t</graphics>",
        "\t\t<name>",
        "\t\t\t<value>" ++ SN ++ "</value>",
        "\t\t\t<graphics/>",
        "\t\t</name>",
        "\t\t<initialMarking>",
        "\t\t\t<value>" ++ integer_to_list(IM)  ++ "</value>",
        "\t\t\t<graphics>",
        "\t\t\t\t<offset x=\"0.0\" y=\"0.0\"/>",
        "\t\t\t</graphics>",
        "\t\t</initialMarking>",
        "\t</place>"
    ].

transition_to_pnml(
    #transition
    {
        name = N,
        showed_name = SN,
        position = {X, Y}
    }) ->
    [
        "\t<transition id=\"" ++ N ++ "\">",
        "\t\t<graphics>",
        "\t\t\t<position x=\"" ++ X ++ "\" y=\"" ++ Y ++ "\"/>",
        "\t\t</graphics>",
        "\t\t<name>",
        "\t\t\t<value>" ++ SN ++ "</value>",
        "\t\t\t<graphics/>",
        "\t\t</name>",
        "\t\t<orientation>",
        "\t\t\t<value>0</value>",
        "\t\t</orientation>",
        "\t\t<rate>",
        "\t\t\t<value>1.0</value>",
        "\t\t</rate>",
        "\t\t<timed>",
        "\t\t\t<value>false</value>",
        "\t\t</timed>",
        "\t</transition>"
    ].

arc_to_pnml(
    #arc
    {
        source = S,
        target = T
    }) ->
    [
        "\t<arc id=\"from " ++ S ++ " to " ++ T++ "\" source=\"" ++ S ++ "\" target=\"" ++ T ++ "\">",
        "\t\t<graphics/>",
        "\t\t<inscription>",
        "\t\t\t<value>1</value>",
        "\t\t\t<graphics/>",
        "\t\t</inscription>",
        "\t</arc>"
    ].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_xml_document(File) ->
    {ok, InpDev} = 
        file:open(File, [read]),
    {XML, []} = 
        xmerl_scan:string(
            read_data(InpDev), 
            [{encoding, "iso-10646-utf-1"}]),
    XML.

read_data(Device) ->
    Binary = read(Device),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    lists:concat([binary_to_list(R) || R <- Res]).

-define(BLK_SIZE, 16384).

read(Device) ->
    ok = io:setopts(Device, [binary]),
    read(Device, <<>>).

read(Device, Acc) ->
    case file:read(Device, ?BLK_SIZE) of
        {ok, Data} ->
            read(Device, <<Acc/bytes, Data/bytes>>);
        eof ->
            Acc
    end.
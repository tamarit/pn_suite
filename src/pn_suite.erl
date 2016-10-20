-module( pn_suite ).
 
-export( [main/1] ).

-include("pn.hrl").
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Args) ->
    PN = pn_input:read_pn(hd(Args)),
    io:format(
        "Petri net ~s successfully read from directory:\n\t ~s\n\n", 
        [PN#petri_net.name, PN#petri_net.dir]),
    Op1 = "Run the Petri Net",
    Op2 = "Export the Petri Net",
    Op3 = "Slicing",
    Op4 = "Slicing improved",
    Op5 = "Slicing (for a given transition sequence)",
    Op6 = "Slicing Yu et al",
    Op7 = "Slicing Rakow CTL",
    Op8 = "Slicing Rakow Safety",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2, Op3, Op4, Op5, Op6, Op7, Op8]),
    QuestionLines = 
            ["These are the available options: " | lists:reverse(Lines)]
        ++  ["What do you want to do?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        pn_lib:get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    case dict:fetch(Answer, AnsDict) of 
        Op1 ->
            pn_lib:build_digraph(PN),
            PNBefExec = 
                pn_run:set_enabled_transitions(PN),
            FunChoose = 
                ask_mode(),
            {PNFinal, Executed} = 
                pn_run:run(PNBefExec, FunChoose, []),
            io:format(
                "Execution:\n~s\n", 
                [string:join([T || {T, _} <- Executed], ",")]),
            export(PNFinal, "_run");
        Op2 ->    
            export(PN, "");
        Op3 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN),
            PNSlice = pn_slice:slice(PN, SC),
            export(PNSlice, "_slc");
        Op4 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN),
            PNSlice = pn_slice:slice_imp(PN, SC),
            export(PNSlice, "_slc_imp");
        Op5 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN), 
            PNBefExec = 
                pn_run:set_enabled_transitions(PN),
            {SCT, Exec} = 
                ask_transitions_slicing_criterion(PNBefExec),
            io:format("Slicing criterion execution: [~s]\n", [string:join(SCT, ", ")]),
            RevExec = 
                lists:reverse([{none, PNBefExec} | Exec]),
            PNSlice = 
                pn_slice_seq:slice_with_sequence(RevExec, SC, []),
            export(PNSlice, "_slc_trs");
        Op6 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN),
            SDG = pn_yuetal:sdg_sim(PN, SC),
            BSG = pn_yuetal:bsg_sim(PN, SDG, SC),
            pn_yuetal:write_sdg(SDG, "sdg_sim.dot"),
            pn_yuetal:write_sdg(BSG, "bsg_sim.dot"),
            PNSlice = pn_lib:filter_pn(PN, pn_yuetal:sdg_places_trans(BSG)),
            export(PNSlice, "_slc_yu");
        Op7 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN),
            PNSlice = pn_rakow:slice_ctl(PN, SC),
            export(PNSlice, "_slc_rakow_ctl");
        Op8 ->
            SC0 = ask_slicing_criterion(PN),
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN),
            PNSlice = pn_rakow:slice_safety(PN, SC),
            export(PNSlice, "_slc_rakow_safety")
    end,
    ok.

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
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            Places),
    QuestionLines = 
            ["Select one or more places (separated by spaces): " | lists:reverse(Lines)]
        ++  ["What is the slicing criterion?: "],
    Answers = 
        pn_lib:get_answer_multiple(
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
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2]),
    QuestionLines = 
            ["A sequence of transitions is needed: " | lists:reverse(Lines)]
        ++  ["What do you want to do?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        pn_lib:get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
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
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            Transitions),
    QuestionLines = 
            ["Select one or more transitions (separated by spaces): " | lists:reverse(Lines)]
        ++  ["What is the slicing criterion?: "],
    Answers = 
        pn_lib:get_answer_multiple(
            string:join(QuestionLines,"\n"), 
            lists:seq(1, length(Ans))),
    [lists:last(string:tokens(dict:fetch(A, AnsDict), " - ")) || A <- lists:reverse(Answers)].


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
    case pn_run:run(PN, FunChoose, []) of 
        error ->
            false;
        Exec ->
            {ok, Exec}
    end.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
                fun pn_run:ask_fired_transition/2;
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
% Export functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export(PN, Suffix) ->
    PNtoExport = pn_input:read_pos_from_svg(PN),
    Op1 = "pdf",
    Op2 = "dot",
    Op3 = "PNML compatible with PIPE",
    Op4 = "LoLa",
    Op5 = "Other formats",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2, Op3, Op4, Op5]),
    QuestionLines = 
            ["These are the available output formats: " | lists:reverse(Lines)]
        ++  ["What format do you need?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        pn_lib:get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    case dict:fetch(Answer, AnsDict) of 
        Op3 -> 
            pn_output:print_pnml(PNtoExport);
        Op4 -> 
            pn_output:print_lola(PNtoExport);
        Op5 ->
            ask_other_formats(PNtoExport, Suffix);
        Format ->
            pn_output:print_net(PNtoExport, false, Format, Suffix)
    end.

ask_other_formats(PN, Suffix) ->
    FormatsStr = lists:map(fun atom_to_list/1, pn_output:formats()),
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            FormatsStr),
    QuestionLines = 
            ["These are all the available output formats: " | lists:reverse(Lines)]
        ++  ["What format do you need?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        pn_lib:get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    pn_output:print_net(PN, false, dict:fetch(Answer, AnsDict), Suffix).











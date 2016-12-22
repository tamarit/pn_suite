-module( pn_suite ).
 
-export( 
    [
        main/1, web/1, web_convert/1, 
        slice_prop_preserving/1, prop_preservation/1
    ]).

-include("pn.hrl").
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Args) ->
    PN = pn_input:read_pn(hd(Args)),
    io:format(
        "Petri net ~s successfully read.\n\n", [PN#petri_net.name]),
    % io:format(
    %     "Petri net ~s successfully read from directory:\n\t ~s\n\n", 
    %     [PN#petri_net.name, PN#petri_net.dir]),
    Op1 = "Run the Petri Net",
    Op2 = "Export the Petri Net",
    Op3 = "Slicing Llorens et al",
    Op4 = "Slicing Llorens et al (improved)",
    Op5 = "Slicing Llorens et al (for a given transition sequence)",
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
            slicing_common(PN, fun pn_slice:slice/2, "_slc");
        Op4 ->
            slicing_common(PN, fun pn_slice:slice_imp/2, "_slc_imp");
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
            slicing_common(PN, fun pn_yuetal:slice/2, "_slc_yu");
        Op7 ->
            slicing_common(PN, fun pn_rakow:slice_ctl/2, "_slc_rakow_ctl");
        Op8 ->
            slicing_common(PN, fun pn_rakow:slice_safety/2, "_slc_rakow_safety")
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slicing_common(PN, SliceFun, Suffix) ->
    SC0 = ask_slicing_criterion(PN),
    SC = lists:usort(SC0),
    io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
    pn_lib:build_digraph(PN),
    PNSlice = SliceFun(PN, SC),
    export(PNSlice, Suffix).

slicing_common(PN, SliceFun, SC, Suffix) ->
    pn_lib:build_digraph(PN),
    PNSlice = SliceFun(PN, SC),
    PNtoExport = pn_input:read_pos_from_svg(PNSlice),
    pn_output:print_pnml(PNtoExport, Suffix).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing criterion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_slicing_criterion(#petri_net{places = Ps}) ->
    Places0 = 
        dict:fold(
            fun (K, #place{showed_name = K}, Acc) -> 
                    [{K, K} | Acc];
                (K, #place{showed_name = SN}, Acc) -> 
                    [{K, SN ++ " - " ++ K} | Acc]
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
            fun 
                (K, #transition{showed_name = K}, Acc) ->
                    [ K | Acc];   
                (K, #transition{showed_name = SN}, Acc) ->
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

parse_sc(PN, SCStr) -> 
    PsKeys = 
        dict:fetch_keys(PN#petri_net.places),
    SCParsed = string:tokens(SCStr, " ,"),
    case [P || P <- SCParsed, lists:member(P, PsKeys)] of 
        SCParsed ->
            SCParsed;
        SCFiltered ->
            io:format("The slicing criterion contains unknown places. They will be ignored.\n"),       
            SCFiltered
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Web
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


web([File, Alg, TimeoutStr, SCStr]) ->
    {PN, SC0, Timeout} = 
        try 
            PN0 = 
                pn_input:read_pn(File),
            io:format(
                "Petri net ~s successfully read.\n\n", [PN0#petri_net.name]),
            Timeout0 = 
                try 
                   case list_to_integer(TimeoutStr) of
                    Num when (is_number(Num) andalso Num =< 2000 ) -> 
                        Num;
                    _ -> 
                        io:format("Timeout set to default value, i.e. 50 milisec.\n"),
                        50
                   end
                catch 
                    _:_ ->
                        io:format("Timeout set to default value, i.e. 50 milisec.\n"),
                        50
                end,
            SCParsed = parse_sc(PN0, SCStr),
            {PN0, SCParsed, Timeout0}
        catch 
            _:_ ->
                io:format("The input Petri net cannot be read.\n"),
                io:format("error_format\n"),
                {error, error,error}
        end,
    case PN of 
        error ->
            ok;
        _ ->
            SC = lists:usort(SC0),
            io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
            pn_lib:build_digraph(PN),
            % TODO: Improve this using or extending pn_lib:algorithms()
            FunSlice = 
                case Alg of 
                    "llorens_imp" ->
                        io:format("Slicing using Llorens et al. improved.\n"),
                        fun pn_slice:slice_imp/2;
                    "rakow_ctl" ->
                        io:format("Slicing using Rakow CTL.\n"),
                        fun pn_rakow:slice_ctl/2;
                    "rakow_safety" ->
                        io:format("Slicing using Rakow safety.\n"),
                        fun pn_rakow:slice_safety/2;
                    "yu" ->
                        io:format("Slicing using Yu et al.\n"),
                        fun pn_yuetal:slice/2;
                    _ ->
                        io:format("Slicing using Llorens et al.\n"),
                        fun pn_slice:slice/2
                end,
            case SC of
                [] ->
                    PNtoExport = 
                        pn_input:read_pos_from_svg_web(PN),
                    pn_output:print_pnml_file(
                        PNtoExport, 
                        "pn_slicer_slice.xml"),
                    io:format("1");                
                _ -> 
                    Self = self(),
                    spawn(fun() -> Self!FunSlice(PN, SC) end),
                    receive 
                        PNSlice -> 
                            pn_output:print_pnml_file(
                                pn_input:read_pos_from_svg_web(PNSlice), 
                                "pn_slicer_slice.xml"),
                            io:format("1")
                    after 
                        Timeout ->
                            io:format("Execution cut due to timeout.\n"),
                            io:format("0")
                    end
            end
    end.

web_convert(Format) ->
    case Format of 
        "xml" ->
            ok;
        "lola" ->
            PN = pn_input:read_pn("pn_slicer_slice.xml"),
            pn_lib:build_digraph(PN),
            pn_output:print_lola_file(
                PN, 
                "pn_slicer_slice.lola");
        "apt" ->
            PN = pn_input:read_pn("pn_slicer_slice.xml"),
            pn_lib:build_digraph(PN),
            pn_output:print_apt_file(
                PN, 
                "pn_slicer_slice.apt");   
        _ ->
            PN = pn_input:read_pn("pn_slicer_slice.xml"),
            pn_lib:build_digraph(PN),
            pn_output:print_net_file(
                PN,
                "pn_slicer_slice", Format)      
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
    Op5 = "APT",
    Op6 = "Other formats",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2, Op3, Op4, Op5, Op6]),
    QuestionLines = 
            ["These are the available output formats: " | lists:reverse(Lines)]
        ++  ["What format do you need?" 
             | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        pn_lib:get_answer(string:join(QuestionLines,"\n"), lists:seq(1, length(Ans))),
    case dict:fetch(Answer, AnsDict) of 
        Op3 -> 
            pn_output:print_pnml(PNtoExport, Suffix);
        Op4 -> 
            pn_lib:build_digraph(PN),
            pn_output:print_lola(PNtoExport, Suffix);
        Op5 -> 
            pn_lib:build_digraph(PN),
            pn_output:print_apt(PNtoExport, Suffix);
        Op6 ->
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Property-preserving slice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_prop_preserving(Args) -> 
    TimeoutAnalysis = 5000,
    [PNFile, SCStr, PropsStr | _] = Args,
    % io:format("~p\n", [{PNFile, PropsStr, SCStr}]),
    {PN, SizeOriginal, PropOriginal} = 
        create_pn_and_prop(PNFile, TimeoutAnalysis, false),
    SC = 
        parse_sc(PN, SCStr),
    io:format("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
    PropsParsed = 
        pn_properties:parse_property_list(PropsStr),
    case PropsParsed of 
        {alg, Alg} ->
            case Alg of 
                "llorens_imp" ->
                    io:format("Slicing using Llorens et al. improved.\n"),
                    slicing_common(PN, fun pn_slice:slice_imp/2, SC, "_slc_imp");
                "rakow_ctl" ->
                    io:format("Slicing using Rakow CTL.\n"),
                    slicing_common(PN, fun pn_rakow:slice_ctl/2, SC, "_slc_rakow_ctl");
                "rakow_safety" ->
                    io:format("Slicing using Rakow safety.\n"),
                    slicing_common(PN, fun pn_rakow:slice_safety/2, SC, "_slc_rakow_safety");
                "yu" ->
                    io:format("Slicing using Yu et al.\n"),
                    slicing_common(PN, fun pn_yuetal:slice/2, SC, "_slc_yu");
                _ ->
                    io:format("Slicing using Llorens et al.\n"),
                    slicing_common(PN, fun pn_slice:slice/2, SC, "_slc")
            end;
        _ ->
            Slices0 = 
                lists:map(
                    fun(#slicer{name = Name, function = Fun}) ->
                        PNS = Fun(PN, SC),
                        pn_lib:build_digraph(PNS),
                        {Name, PNS}
                    end,
                    pn_lib:algorithms() ),
            PNtoExportOri = 
                pn_input:read_pos_from_svg(PN),
            pn_output:print_lola(PNtoExportOri, "_temp_ori"),
            Slices = 
                lists:filter(
                    fun({_, PNSlice}) ->
                        PNtoExportSlice = 
                            pn_input:read_pos_from_svg(PNSlice),
                        pn_output:print_lola(PNtoExportSlice, "_temp_slice"),
                        {PropSlice, _} = 
                            pn_properties:apt_properties(PNSlice, TimeoutAnalysis),
                        are_properties_preserved(PropsParsed, PropOriginal, PropSlice, PN, PNSlice, TimeoutAnalysis)
                    end,
                    Slices0
                    ),
            lists:foldl(
                fun({Name, PNSlice}, Acc) ->
                    PNtoExport = 
                        pn_input:read_pos_from_svg(PNSlice),
                    Suffix = 
                        "_" ++ integer_to_list(Acc),
                    pn_output:print_pnml(PNtoExport, Suffix),
                    pn_output:print_net(PNtoExport, false, "pdf", Suffix),
                    ReductionStr = 
                        float_to_list(100 - (100 * pn_lib:size(PNSlice) / SizeOriginal),[{decimals,2}]),
                    io:format("~p.- ~s -> Reduction: ~s %\n", [Acc, Name, ReductionStr]),
                    Acc + 1
                end,
                1,
                Slices
                ),
            ok
    end.

create_pn_and_prop(PNFile, Timeout, Silent) ->
    PN = pn_input:read_pn(PNFile),
    pn_lib:build_digraph(PN),
    case Silent of 
        false -> 
            io:format(
                "Petri net named ~s successfully read.\n", [PN#petri_net.name]);
        true ->
            ok 
    end,
    {PropOriginal, _} = 
        pn_properties:apt_properties(PN, Timeout),
    {PN, pn_lib:size(PN), PropOriginal}.

are_properties_preserved(PropList, DictOri, DictMod, PN, PNSlice, TimeoutAnalysis) ->
    lists:all(
        fun
            ({lola, LolaFormula}) ->
                is_lola_property_preserved(LolaFormula, PN, TimeoutAnalysis);
            ({st, ST}) ->
                pn_properties:is_apt_property_preserved(ST, PN, PNSlice, TimeoutAnalysis);
            (Prop) -> 
                is_property_preserved(Prop, DictOri, DictMod) 
        end, 
        PropList).

is_property_preserved(Prop, DictOri, DictMod) ->
    dict:fetch(Prop, DictOri) == dict:fetch(Prop, DictMod).

is_lola_property_preserved(Formula, PN, TimeoutAnalysis) ->
    Dir = PN#petri_net.dir ++ "/output/",
    LOLAFile1 = 
        Dir ++  PN#petri_net.name ++ "_temp_ori.lola", 
    LOLAFile2 = 
        Dir ++  PN#petri_net.name ++ "_temp_slice.lola", 
    pn_properties:check_formula(Formula, LOLAFile1, Dir, TimeoutAnalysis) 
    == 
    pn_properties:check_formula(Formula, LOLAFile2, Dir, TimeoutAnalysis). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Property-preservation info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_preservation(Args) -> 
    TimeoutAnalysis = 5000,
    [PNFile1, PNFile2 | TailArgs] = Args,
    [{PN1, _, D1}, {PN2, _, D2}] = 
        lists:map(
            fun(PN) -> 
                create_pn_and_prop(PN, TimeoutAnalysis, true) 
            end, 
            [PNFile1, PNFile2]),
    case TailArgs of 
        [] ->    
            {Preserved, Changed} =
                pn_properties:compare_properties(D1, D2),
            {PreservedStr, ChangedStr} = 
                case Changed of 
                    [] ->
                        {"All", "None"};
                    _ ->
                        {string:join(Preserved, ", "), string:join(Changed, ", ")}
                end,
            InfoAlg = 
                [
                    "Preserved properties:\n" ++ PreservedStr,
                    "",
                    "Changed properties:\n" ++ ChangedStr
                ],
            io:format("~s\n", [string:join(InfoAlg, "\n")]);
        [PropsStr | _] ->
            PropsParsed = 
                pn_properties:parse_property_list(PropsStr),
            Preserved = 
                are_properties_preserved(PropsParsed, D1, D2, PN1, PN2, TimeoutAnalysis),
            io:format("~p\n", [Preserved])
    end.



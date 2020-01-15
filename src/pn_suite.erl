-module( pn_suite ).
 
-export( 
    [
        main/1, web/1, web_convert/0, web_convert/2, 
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
    Op3 = "Slicing Llorens et al.",
    Op4 = "Slicing Llorens et al. (precise)",
    % Op10 = "Slicing Llorens et al. (precise single)",
    Op5 = "Slicing Llorens et al. (for a given transition sequence)",
    Op6 = "Slicing Yu et al.",
    Op7 = "Slicing Rakow CTL",
    Op8 = "Slicing Rakow Safety",
    Op9 = "Slicing Khan",
    {_, Lines, Ans, AnsDict} = 
        lists:foldl(
            fun pn_lib:build_question_option/2,
            {1, [], [], dict:new()},
            [Op1, Op2, Op3, Op4, Op5, Op6, Op7, Op8, Op9]),
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
            slicing_common(PN, fun pn_slice:slice_imp/2, "_slc_prec");
        % Op10 ->
        %     slicing_common(PN, fun pn_slice:slice_imp_single_gen/2, "_slc_prec_single");
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
            slicing_common(PN, fun pn_rakow:slice_safety/2, "_slc_rakow_safety");
        Op9 ->
            slicing_common(PN, fun pn_khan:slice_abs/2, "_slc_khan_abs")
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

% slicing_common(PN, SliceFun, SC, Suffix) ->
%     pn_lib:build_digraph(PN),
%     PNSlice = SliceFun(PN, SC),
%     PNtoExport = pn_input:read_pos_from_svg(PNSlice),
%     pn_output:print_pnml(PNtoExport, Suffix),
%     pn_lib:size(PNtoExport).

slicing_common_with_print(PN, SliceFun, SC, Suffix) ->
    pn_lib:build_digraph(PN),
    PNSlice = SliceFun(PN, SC),
    PNtoExport = pn_input:read_pos_from_svg(PNSlice),
    pn_output:print_pnml(PNtoExport, Suffix),
    pn_output:print_net(PNtoExport, false, "pdf", Suffix, SC),
    pn_lib:size(PNtoExport).

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
            {SCParsed, ""};
        SCFiltered ->      
            {SCFiltered, "Warning: The slicing criterion contains unknown places. They will be ignored.\n\tUnknown places: " ++ lists:join(" ", SCParsed -- SCFiltered)}
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
            {SCParsed, Warnings} = parse_sc(PN0, SCStr),
            io:format("~s\n", [Warnings]),
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
                    "llorens_prec" ->
                        io:format("Slicing using Llorens et al. (precise).\n"),
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
                % [] ->
                %     PNtoExport = 
                %         pn_input:read_pos_from_svg_web(PN),
                %     pn_output:print_pnml_file(
                %         PNtoExport, 
                %         "pn_slicer_slice.xml"),
                %     io:format("1");                
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

web_convert() ->
    PN = pn_input:read_pn("pn_slicer_temp.xml"),
    pn_lib:build_digraph(PN),
    pn_output:print_net_file(
        PN,
        "pn_slicer_net", "pdf").  

web_convert(Format, SC) ->
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
            {SCParsed, _} = parse_sc(PN, SC),
            % io:format("SC: ~p\n", [SC]),
            % io:format("SCParsed: ~p\n", [SCParsed]),
            pn_lib:build_digraph(PN),
            pn_output:print_net_file(
                PN,
                "pn_slicer_slice", Format, SCParsed)      
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
                    io:format("Selected transition: ~s\n", [Next]),
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
    % PNtoExport = pn_input:read_pos_from_svg(PN),
    PNtoExport = PN,
    Op1 = "pdf",
    Op2 = "dot",
    Op3 = "PNML (compatible with PIPE)",
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
            PNtoExport2 = pn_input:read_pos_from_svg(PNtoExport),
            pn_output:print_pnml(PNtoExport2, Suffix);
        Op4 -> 
            pn_lib:build_digraph(PN),
            pn_output:print_lola(PNtoExport, Suffix);
        Op5 -> 
            pn_lib:build_digraph(PN),
            pn_output:print_apt(PNtoExport, Suffix);
        Op6 ->
            PNtoExport2 = pn_input:read_pos_from_svg(PNtoExport),
            ask_other_formats(PNtoExport2, Suffix);
        Format ->
            PNtoExport2 = pn_input:read_pos_from_svg(PNtoExport),
            pn_output:print_net(PNtoExport2, false, Format, Suffix)
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
    % io:format("Args: ~p\n", [Args]),
    {PNSUITEPath, PNFile, SCStr, PropsStr, TailArgs} = 
        case Args of 
            [PNSUITEPath0, PNFile0, SCStr0, PropsStr0 , "JSON" | _] ->
                {PNSUITEPath0, PNFile0, SCStr0, PropsStr0, "JSON"};
            [PNSUITEPath0, PNFile0, SCStr0, "JSON" | _] ->
                {PNSUITEPath0, PNFile0, SCStr0, "", "JSON"};
            [PNSUITEPath0, PNFile0, SCStr0, PropsStr0 | TailArgs0] ->
                {PNSUITEPath0, PNFile0, SCStr0, PropsStr0, []};
            [PNSUITEPath0, PNFile0, SCStr0 | _] ->
                {PNSUITEPath0, PNFile0, SCStr0, "", []}
        end,
    % [PNSUITEPath, PNFile, SCStr, PropsStr | TailArgs] = Args,
    % io:format("~p\n", [{PNFile, PropsStr, SCStr, TailArgs}]),
    {PN, SizeOriginal, PropOriginal} = 
        create_pn_and_prop(PNFile, TimeoutAnalysis, TailArgs /= [], PNSUITEPath),
    {SC,WarningsSC} = 
        parse_sc(PN, SCStr),
    {PropsParsed, Warnings} = 
        pn_properties:parse_property_list(PropsStr),
    Printer = 
        fun(Str, Data) ->
            case TailArgs of 
                [] ->
                    io:format(Str, Data);
                _ ->
                    ok 
            end
        end,
    Printer("~s\n", [WarningsSC]),
    Printer("Slicing criterion: [~s]\n", [string:join(SC, ", ")]),
    Printer("~s\n", [Warnings]),
    case PropsParsed of 
        {alg, Alg} ->
            {SizeSlice, Suffix} = 
                case Alg of 
                    "llorens_prec" ->
                        Printer("Slicing using Llorens et al. (precise).\n", []),
                        Suffix0 = "_slc_prec",
                        Fun = fun pn_slice:slice_imp/2, 
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "llorens_pg" ->
                        Printer("Slicing using Llorens et al. (precise). GENERIC VERSION.\n", []),
                        Suffix0 = "_slc_pg",
                        Fun = fun pn_slice:slice_imp_gen/2, 
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "llorens_ps" ->
                        Printer("Slicing using Llorens et al. (precise) SINGLE VERSION.\n", []),
                        Suffix0 = "_slc_ps",
                        Fun = fun pn_slice:slice_imp_single/2, 
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "llorens_psg" ->
                        Printer("Slicing using Llorens et al. (precise) SINGLE & GENERIC VERSION.\n", []),
                        Suffix0 = "_slc_psg",
                        Fun = fun pn_slice:slice_imp_single_gen/2, 
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "rakow_ctl" ->
                        Printer("Slicing using Rakow CTL.\n", []),
                        Suffix0 = "_slc_rakow_ctl",
                        Fun = fun pn_rakow:slice_ctl/2,
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "rakow_safety" ->
                        Printer("Slicing using Rakow safety.\n", []),
                        Suffix0 = "_slc_rakow_safety",
                        Fun = fun pn_rakow:slice_safety/2,
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "yu" ->
                        Printer("Slicing using Yu et al.\n", []),
                        Suffix0 = "_slc_yu",
                        Fun = fun pn_yuetal:slice/2,
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "yu_ori" ->
                        Printer("Slicing using Yu et al. (ORIGINAL)\n", []),
                        Suffix0 = "_slc_yu_ori",
                        Fun = fun pn_yuetal:slice_no_sim/2,
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    "llorens_gen" ->
                        Printer("Slicing using Llorens et al. (GENERIC)\n", []),
                        Suffix0 = "_slc_gen",
                        Fun = fun pn_slice:slice_gen/2,
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0};
                    _ ->
                        Printer("Slicing using Llorens et al.\n", []),
                        Suffix0 = "_slc",
                        Fun = fun pn_slice:slice/2,
                        {slicing_common_with_print(PN, Fun, SC, Suffix0), Suffix0}
                end,
            % pn_output:print_net(PN, false, "pdf", Suffix),
            ReductionStr = 
                float_to_list(100 - (100 * SizeSlice / SizeOriginal),[{decimals,2}]),
            json_output_alg(TailArgs, {SC, ReductionStr, [Warnings, WarningsSC], PN#petri_net.name, PNFile, [{Alg, Suffix, no_calc}]});
        _ ->
            Slices0 = 
                lists:map(
                    fun(#slicer{name = Name, short_name = SName, function = Fun}) ->
                        PNS = Fun(PN, SC),
                        pn_lib:build_digraph(PNS),
                        {Name, SName, PNS}
                    end,
                    pn_lib:algorithms() ),
            PNtoExportOri = 
                pn_input:read_pos_from_svg(PN),
            pn_output:print_lola(PNtoExportOri, "_temp_ori"),
            Slices = 
                lists:filter(
                    fun({_, _, PNSlice}) ->
                        PNtoExportSlice = 
                            pn_input:read_pos_from_svg(PNSlice),
                        pn_output:print_lola(PNtoExportSlice, "_temp_slice"),
                        {PropSlice, _} = 
                            pn_properties:apt_properties(PNSlice, TimeoutAnalysis, PNSUITEPath),
                        are_properties_preserved(PropsParsed, PropOriginal, PropSlice, PN, PNSlice, TimeoutAnalysis, PNSUITEPath)
                    end,
                    Slices0
                    ),
            {Res , _} = 
                lists:mapfoldl(
                    fun({Name, ShortName, PNSlice}, Acc) ->
                        PNtoExport = 
                            pn_input:read_pos_from_svg(PNSlice),
                        Suffix = 
                            "_" ++ integer_to_list(Acc),
                        pn_output:print_pnml(PNtoExport, Suffix),
                        pn_output:print_net(PNtoExport, false, "pdf", Suffix),
                        ReductionStr = 
                            float_to_list(100 - (100 * pn_lib:size(PNSlice) / SizeOriginal),[{decimals,2}]),
                        Printer("~p.- ~s -> Reduction: ~s %\n", [Acc, Name, ReductionStr]),
                        {{ShortName, "_" ++ integer_to_list(Acc), ReductionStr} , Acc + 1}
                    end,
                    1,
                    Slices
                    ),
            json_output_alg(TailArgs, {SC, PropsParsed, [Warnings, WarningsSC], PN#petri_net.name, PNFile, Res})
    end.

create_pn_and_prop(PNFile, Timeout, Silent, PNSUITEPath) ->
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
        pn_properties:apt_properties(PN, Timeout, PNSUITEPath),
    {PN, pn_lib:size(PN), PropOriginal}.

are_properties_preserved(PropList, DictOri, DictMod, PN, PNSlice, TimeoutAnalysis, PNSUITEPath) ->
    % io:format("~p\n", [PropList]),
    lists:all(
        fun
            ({lola, LolaFormula}) ->
                is_lola_property_preserved(LolaFormula, PN, TimeoutAnalysis);
            ({st, ST}) ->
                pn_properties:is_apt_property_preserved(ST, PN, PNSlice, TimeoutAnalysis, PNSUITEPath);
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

json_output_alg([], _) ->
    ok;
json_output_alg(_, {SC, PropsParsed, [WarningsProp, WarningsSC], PNName, PNFile, Res}) ->
    JSON =
        {struct,[
            {"slicing_criterion", 
                {array,[{struct, [{"place", P}]} || P <- SC]}},
            {"preserved_properties", 
                case PropsParsed of 
                    none ->
                        {array,[]};
                    _ ->
                        STs = 
                            [P || {st, P} <- PropsParsed],
                        LOLAs = 
                            [("lola:" ++ P) || {lola, P} <- PropsParsed],
                        Rest = 
                            PropsParsed 
                            -- 
                            ([P || P = {st, _} <- PropsParsed] 
                             ++ 
                             [P || P = {lola, _} <- PropsParsed] ),
                        PrinteableProps = 
                            STs ++ LOLAs ++ Rest,
                        {array,
                            [{struct, [{"property", P}]} || P <- PrinteableProps]}
                end
            },
            {"warnings_parsing_slicing_criterion", WarningsSC},
            {"warnings_parsing_properties", WarningsProp},
            {"petri_net", 
                {struct, 
                    [{"name", PNName}, 
                    {"file", PNFile}]
                }},
            {"slices", 
                {array,
                    [{struct, [
                        {"algorithm", Alg}, 
                        {"reduction", Reduction}, 
                        {"output_file", "output/" ++ PNName ++ Suffix ++ ".pnml"}
                    ]} 
                    || {Alg, Suffix, Reduction} <- Res]
                }}
        ]},
    io:format("~s",[mochijson:encode(JSON)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Property-preservation info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_preservation(Args) -> 
    TimeoutAnalysis = 5000,
    [PNSUITEPath, PNFile1, PNFile2 | TailArgs] = Args,
    [{PN1, _, D1}, {PN2, _, D2}] = 
        lists:map(
            fun(PN) -> 
                create_pn_and_prop(PN, TimeoutAnalysis, true, PNSUITEPath) 
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
            {PropsParsed, _} = 
                pn_properties:parse_property_list(PropsStr),
            Preserved = 
                are_properties_preserved(PropsParsed, D1, D2, PN1, PN2, TimeoutAnalysis, PNSUITEPath),
            io:format("~p\n", [Preserved])
    end.



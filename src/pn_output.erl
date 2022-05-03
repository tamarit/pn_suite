-module( pn_output ).
 
-export( [  print_net_run/1, 
            print_net/4, print_net/5,
            print_net_file/3, print_net_file/4,
            print_pnml/2, print_pnml_file/2,
            print_lola/2, print_lola_file/2, 
            print_apt/2, print_apt_file/2, 
            formats/0,
            cmd_try/1,
            print_sc_file/2] ).

-include("pn.hrl").
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_dot(
    #petri_net{
        name = Name,
        places = Ps0, 
        transitions = Ts0,
        arcs = As},
    ShowEnabled,
    Highlighted) ->

    Ps = pn_lib:get_value_list_from_dict(Ps0),
    Ts = pn_lib:get_value_list_from_dict(Ts0),

    PsDot = 
        lists:map(fun(P) -> place_to_dot(P, Highlighted) end, Ps),
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
    },
    HighlightedList) ->
    Filled = 
        case IM of 
            0 -> 
                "";
            _ -> 
                " penwidth=8"
                % " style=filled color=\"blue\" fontcolor=\"white\" fillcolor=\"blue\""
        end,
    Highlighted = 
        case lists:member(N, HighlightedList) of 
            false -> 
                "";
            true -> 
                % color=\"blue\"
                " style=filled fontcolor=\"white\" fillcolor=\"blue\""
        end,       
    Name = 
        if 
            SN == N -> SN;
            true -> SN ++ " (id: "  ++ N ++ ")"
        end,
        N ++ " [shape=ellipse, label=\"" ++ Name
    ++  "\\l(" ++ integer_to_list(IM) ++ ")\""++ Filled ++ Highlighted ++ "];".

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
    Name = 
        if 
            SN == N -> SN;
            true -> SN ++ " (id: "  ++ N ++ ")"
        end,
    N ++ " [shape=box, label=\"" ++ Name ++ "\"" ++ Filled ++ "];".

arc_to_dot(
    #arc
    {
        source = S,
        target = T
    }) ->
    S ++ " -> "  ++ T.

print_net_run(PN) ->
    print_net(PN, true, "pdf", "_run").   

print_net_file(PN, File, Format, SC) ->
    print_net_file_common(PN, File, Format, SC).
print_net_file(PN, File, Format) ->
    print_net_file_common(PN, File, Format, []).


print_net_file_common(PN, File, Format, Highlighted) ->
    file:write_file(
        "pn_slicer_temp.dot",  
        list_to_binary(to_dot(PN, false, Highlighted))),
    cmd_try( "dot -T" ++ Format ++ " " 
        ++  "pn_slicer_temp.dot > "
        ++   File ++ "." ++ Format),
    cmd_try("rm -f pn_slicer_temp.dot").

print_net(PN, ShowEnabled, Format, Suffix, Highlighted) ->
    Output = PN#petri_net.dir ++ "/output",
    % io:format("~p\n", [Output]),
    cmd_try("mkdir " ++ Output),
    file:write_file(
            Output ++ "/" 
        ++  PN#petri_net.name ++ "_temp.dot",  
        list_to_binary(to_dot(PN, ShowEnabled, Highlighted))),
    cmd_try(
            "dot -T" ++ Format ++ " " 
        ++  Output ++ "/" 
        ++  PN#petri_net.name ++ "_temp.dot > "
        ++  Output ++ "/"  
        ++  PN#petri_net.name ++ Suffix ++ "." ++ Format),
    cmd_try(
            "rm -f " ++  Output ++ "/" 
        ++  PN#petri_net.name ++ "_temp.dot").

print_net(PN, ShowEnabled, Format, Suffix) ->
    print_net(PN, ShowEnabled, Format, Suffix, []).

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
%             cmd_try("dot -T" ++ StrFormat ++ " "++ PN#petri_net.name ++".dot > formats/"++ PN#petri_net.name ++"." ++ StrFormat) 
%         end,
%         Formats).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PNML  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_pnml_file(PN, File) ->
    file:write_file(
        File, 
        list_to_binary(to_pnml(PN))).

print_pnml(PN, Suffix) ->
    cmd_try("mkdir " ++ PN#petri_net.dir ++ "/output"),
    file:write_file(
            PN#petri_net.dir ++ "/output/" 
        ++  PN#petri_net.name ++ Suffix ++ ".pnml", 
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
    Ps = pn_lib:get_value_list_from_dict(Ps0),
    Ts = pn_lib:get_value_list_from_dict(Ts0),

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
% LOLA format  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_lola_file(PN, File) ->
    file:write_file(
        File, 
        list_to_binary(to_lola(PN))).

print_lola(PN, Suffix) ->
    cmd_try("mkdir " ++ PN#petri_net.dir ++ "/output"),
    file:write_file(
            PN#petri_net.dir ++ "/output/" 
        ++  PN#petri_net.name ++ Suffix ++ ".lola", 
        list_to_binary(to_lola(PN))).


to_lola(
    #petri_net{
        places = Ps0, 
        transitions = Ts0,
        digraph = G}) ->
    Ps = pn_lib:get_value_list_from_dict(Ps0),
    Ts = pn_lib:get_value_list_from_dict(Ts0),
    PsStr0 = lists:map(fun place_to_lola/1, Ps),
    PsStr = "PLACE\n\t" ++ string:join(PsStr0, ", ") ++ ";\n",
    MStr0 = marking_to_lola(Ps, []),
    MStr = "MARKING\n\t" ++ string:join(MStr0, ", ") ++ ";\n",
    TsStr = lists:map(fun(T) -> transition_to_lola(T, G) end, Ts),
    string:join([PsStr, MStr | TsStr], "\n").



place_to_lola(#place{name = N}) ->
    N.

marking_to_lola([#place{name = N, marking = M} | Ps], Acc) ->
    NAcc = 
        case M of 
            0 ->
                Acc;
            _ ->
                [(N ++ ": " ++ integer_to_list(M)) | Acc]
        end,
    marking_to_lola(Ps, NAcc);
marking_to_lola([], Acc) ->
    Acc.

transition_to_lola(#transition{name = T}, G) ->
    FunPlace = 
        fun(P) -> 
            P ++ ": 1" 
        end,
    InTStr = 
        lists:map(FunPlace, digraph:in_neighbours(G, T)), 
    OutTStr = 
        lists:map(FunPlace, digraph:out_neighbours(G, T)), 
    Header = "TRANSITION " ++ T,
    Consume = "\tCONSUME " ++ string:join(InTStr, ", ") ++ ";",
    Produce = "\tPRODUCE " ++ string:join(OutTStr, ", ") ++ ";",
    string:join([Header, Consume, Produce], "\n") ++ "\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% APT format  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_apt_file(PN, File) ->
    file:write_file(
        File, 
        list_to_binary(to_apt(PN))).

print_apt(PN, Suffix) ->
    cmd_try("mkdir " ++ PN#petri_net.dir ++ "/output"),
    file:write_file(
            PN#petri_net.dir ++ "/output/" 
        ++  PN#petri_net.name ++ Suffix ++ ".apt", 
        list_to_binary(to_apt(PN))).


to_apt(
    #petri_net{
        name = Name,
        places = Ps0, 
        transitions = Ts0,
        digraph = G}) ->
    NameStr = ".name \"" ++ Name ++ "\"",
    TypeStr = ".type LPN",
    Empty = "",
    Ps = pn_lib:get_value_list_from_dict(Ps0),
    Ts = pn_lib:get_value_list_from_dict(Ts0),
    PsStr0 = lists:map(fun place_to_apt/1, Ps),
    PsStr = ".places\n" ++ string:join(PsStr0, "\n"),
    TsStr0 = lists:map(fun transition_to_apt/1, Ts),
    TsStr = ".transitions\n" ++ string:join(TsStr0, "\n"),
    FlowStr0 = lists:map(fun(T) -> flow_to_apt(T, G) end, Ts),
    FlowStr = ".flows\n" ++ string:join(FlowStr0, "\n") ,
    IMStr0 = "{" ++ string:join(marking_to_apt(Ps, []), ", ") ++ "}",
    IMStr = ".initial_marking " ++ IMStr0,
    string:join(
        [NameStr, TypeStr, Empty, PsStr, Empty, TsStr, 
         Empty, FlowStr, Empty, IMStr], 
        "\n").



place_to_apt(#place{name = N}) ->
    N.

transition_to_apt(#transition{name = N, showed_name = SN}) ->
    N ++ "[label=\"" ++ SN ++ "\"]".

flow_to_apt(#transition{name = T}, G) ->
    FunPlace = fun(P) -> "1*" ++  P end,
    InTStr = lists:map(FunPlace, digraph:in_neighbours(G, T)),
    OutTStr = lists:map(FunPlace, digraph:out_neighbours(G, T)),
    Consume = "{" ++ string:join(InTStr, ", ") ++ "}",
    Produce = "{" ++ string:join(OutTStr, ", ") ++ "}",
    T ++ ": " ++ Consume ++ " -> " ++ Produce.

marking_to_apt([#place{name = N, marking = M} | Ps], Acc) ->
    NAcc = 
        case M of 
            0 ->
                Acc;
            _ ->
                [(integer_to_list(M) ++ "*" ++ N) | Acc]
        end,
    marking_to_apt(Ps, NAcc);
marking_to_apt([], Acc) ->
    Acc.


cmd_try(Cmd) ->
    try 
        Res = os:cmd(Cmd),
        % io:format("~p\n", [Res]),
        Res
    catch 
        _:_ -> 
            % io:format("Something went wrong when executing command: ~p\n", [Cmd]),
            ok
    end.

print_sc_file(SC, Filename) ->
    % io:format("~s~n",[string:join(SC, ",")]),
    {ok, OutDev} = 
        file:open(Filename, [write]),
    file:write(
        OutDev, 
        list_to_binary(
            pn_lib:format(
                "~s", 
                [string:join(SC, ",")] ) ) ),
    file:close(OutDev).
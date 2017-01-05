-module( pn_input ).
 
-export( [  read_pn/1, read_pos_from_svg/1, 
            read_pos_from_svg_web/1, read_data/1,
            read_file_lines/1,
            is_pnml_file/1] ).

-include("pn.hrl").
 
-include_lib("xmerl/include/xmerl.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read PN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_pn(File) ->
    XML = read_xml_document(File),
    Name0 = 
        read_attribute(
            hd(xmerl_xpath:string("//net", XML)),
            "id"
        ),
    Name = 
        case Name0 of 
            "" ->
                [$.|RestName] = 
                    lists:dropwhile(
                        fun($.) ->
                                false;
                            (_) ->
                                true
                        end,
                        lists:reverse(Name0)),
                lists:reverse(RestName);
            _ ->
                Name0
        end,
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
    #petri_net{
        name = Name,
        places = build_dict(place, Places),
        transitions = build_dict(transition, Transitions),
        arcs = Arcs,
        dir = filename:dirname(filename:absname(File))
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read Posotion from SVG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_pos_from_svg(PN) ->
	Suffix = "_temp",
	pn_output:print_net(PN, false, "svg", Suffix),
    SVG = 
        read_xml_document(
                PN#petri_net.dir ++ "/output/" 
            ++  PN#petri_net.name ++  Suffix ++ ".svg"),
    os:cmd(
            "rm -f " ++  PN#petri_net.dir ++ "/output/"
        ++  PN#petri_net.name ++ "_temp.svg"),
    extract_positions(SVG, PN).

read_pos_from_svg_web(PN) ->
    pn_output:print_net_file(PN, "pn_slicer_temp", "svg"),
    SVG = 
        read_xml_document("pn_slicer_temp.svg"),
    os:cmd("rm -f pn_slicer_temp.svg"),
    extract_positions(SVG, PN).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_file_lines(File) ->
    {ok, InpDev} = 
        file:open(File, [read]),
    Res = 
        read_data(InpDev),
    file:close(InpDev),
    Res.

read_xml_document(File) ->
    {ok, InpDev} = 
        file:open(File, [read]),
    {XML, []} = 
        xmerl_scan:string(
            lists:concat(read_data(InpDev)), 
            [{encoding, "iso-10646-utf-1"}]),
    file:close(InpDev),
    XML.

read_data(Device) ->
    Binary = read(Device),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [binary_to_list(R) || R <- Res].

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

is_pnml_file(File) ->
        (length(File) >= 5)
    andalso
        (
            (string:substr(File, length(File) - 3, 4) == ".xml") 
        orelse 
            (string:substr(File, length(File) - 4, 5) == ".pnml")
        ).
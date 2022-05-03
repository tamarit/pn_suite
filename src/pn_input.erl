-module( pn_input ).
 
-export( [  read_pn/1, read_pos_from_svg/1, 
            read_pos_from_svg_web/1, read_data/1,
            read_file_lines/1,
            is_pnml_file/1,
            read_json/1] ).

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
% Read JSON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% {struct,[{"directed",true},
%          {"multigraph",false},
%          {"graph",{struct,[]}},
%          {"nodes",
%           {array,[{struct,[{"id",{array,["transition","T2"]}}]},
%                   {struct,[{"id",{array,["place","P3"]}}]},
%                   {struct,[{"id",{array,["place","P4"]}}]},
%                   {struct,[{"id",{array,["transition","T3"]}}]},
%                   {struct,[{"id",{array,["place","P2"]}}]},
%                   {struct,[{"id",{array,["transition","T1"]}}]},
%                   {struct,[{"id",{array,["place","P1"]}}]},
%                   {struct,[{"id",{array,["transition","T0"]}}]},
%                   {struct,[{"id",{array,["place","P0"]}}]}]}},
%          {"links",
%           {array,[{struct,[{"source",{array,["transition","T2"]}},
%                            {"target",{array,["place","P3"]}}]},
%                   {struct,[{"source",{array,["place","P4"]}},
%                            {"target",{array,["transition","T2"]}}]},
%                   {struct,[{"source",{array,["transition","T3"]}},
%                            {"target",{array,["place","P4"]}}]},
%                   {struct,[{"source",{array,["place","P2"]}},
%                            {"target",{array,["transition","T2"]}}]},
%                   {struct,[{"source",{array,["transition","T1"]}},
%                            {"target",{array,["place","P2"]}}]},
%                   {struct,[{"source",{array,["place","P1"]}},
%                            {"target",{array,["transition","T1"]}}]},
%                   {struct,[{"source",{array,["place","P1"]}},
%                            {"target",{array,["transition","T3"]}}]},
%                   {struct,[{"source",{array,["transition","T0"]}},
%                            {"target",{array,["place","P1"]}}]},
%                   {struct,[{"source",{array,["place","P0"]}},
%                            {"target",{array,["transition","T0"]}}]}]}}]}

read_json(JSONFile) -> 
    {ok, IODev} = file:open(JSONFile, [read]),
    [JSONContent|_] = read_data(IODev),
    file:close(JSONFile),
    % io:format("~p\n", [JSONContent]),
    JSON = mochijson:decode(JSONContent),
    % io:format("~p\n", [JSON]),
    Name = "temp_slice_pn",
    % Name = JSONFile,
        % case Name0 of 
        %     "" ->
        %         [$.|RestName] = 
        %             lists:dropwhile(
        %                 fun($.) ->
        %                         false;
        %                     (_) ->
        %                         true
        %                 end,
        %                 lists:reverse(Name0)),
        %         lists:reverse(RestName);
        %     _ ->
        %         Name0
        % end,
    {struct, [_, _, _, {"nodes", {array, Nodes}}, {"links", {array, Links}}]} = JSON,
    Places = [#place{
        name = replace_minus(
            string:strip(PlaceName, both, $ )),
        showed_name = replace_minus(
            string:strip(PlaceName, both, $ )),
        marking = PlaceMarking
        } || {struct,[{"id",{array,["place",PlaceName, PlaceMarking]}}]} <- Nodes],
    Transitions = [#transition{
        name = replace_minus(
            string:strip(TransitionName, both, $ )),
        showed_name = replace_minus(
            string:strip(TransitionName, both, $ ))
        } || {struct,[{"id",{array,["transition",TransitionName]}}]} <- Nodes],
    Arcs1 = [#arc{
        name = replace_minus(
            string:strip(Source ++ "-" ++ Target, both, $ )),
        source = replace_minus(
            string:strip(Source, both, $ )),
        target = replace_minus(
            string:strip(Target, both, $ ))
        } || {struct,[{"source",{array,[_,Source,_]}}, {"target",{array,[_,Target]}}]} <- Links],
    Arcs2 = [#arc{
        name = replace_minus(
            string:strip(Source ++ "-" ++ Target, both, $ )),
        source = replace_minus(
            string:strip(Source, both, $ )),
        target = replace_minus(
            string:strip(Target, both, $ ))
        } || {struct,[{"source",{array,[_,Source]}}, {"target",{array,[_,Target,_]}}]} <- Links],
    #petri_net{
        name = Name,
        places = build_dict(place, Places),
        transitions = build_dict(transition, Transitions),
        arcs = Arcs1 ++ Arcs2,
        dir = filename:dirname(filename:absname(JSONFile))
    }.

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
        replace_minus(
            string:strip(read_attribute(T, "id"), both, $ )),
    #place{
        name = 
            Name,
        showed_name = 
            replace_minus(
                string:strip(read_value_or_text(T, "name", Name))),
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
            replace_minus(
                string:strip(read_attribute(T, "id"), both, $ )),
        showed_name = 
            replace_minus(
                string:strip(
                    read_value_or_text(T, "name", ?UNNAMED), 
                    both, 
                    $ ))
    }.

extract_info_arc(T) ->
    #arc{
        name = 
            replace_minus(
                string:strip(read_attribute(T, "id"), both, $ )),
        source =  
            replace_minus(
                string:strip(read_attribute(T, "source"), both, $ )),
        target = 
            replace_minus(
                string:strip(read_attribute(T, "target"), both, $ ))
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

replace_minus(L) ->
    lists:map(
        fun
            ($-) ->
                $_;
            (Other) ->
                Other
        end,
        L).

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
    % io:format("~p\n", [File]),
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
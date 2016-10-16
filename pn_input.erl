-module( pn_input ).
 
-export( [read_pn/1, read_pos_from_svg/1, build_digraph/1, get_value_list_from_dict/1] ).

-include("pn.hrl").
 
-include_lib("xmerl/include/xmerl.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read PN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_pn(File) ->
    XML = read_xml_document(File),
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
    #petri_net{
        name = Name,
        places = build_dict(place, Places),
        transitions = build_dict(transition, Transitions),
        arcs = Arcs
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read Posotion from SVG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_pos_from_svg(PN) ->
	Suffix = "_temp",
	pn_ouput:print_net(PN, false, "svg", Suffix),
    SVG = read_xml_document(PN#petri_net.name ++  Suffix ++ ".svg"),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Input
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
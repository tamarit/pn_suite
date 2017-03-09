-module( pn_lib ).

-export( 
    [
    	build_question_option/2, 
    	get_value_list_from_dict/1, 
    	build_digraph/1,
    	check_answer/3,
    	get_answer_multiple/2,
    	get_answer/2,
    	filter_pn/2,
        format/2,
        flush/0,
        algorithms/0,
        size/1,
        new_pn_fresh_digraph/1

    ] ).

-include("pn.hrl").


build_digraph(
    #petri_net{
        places = Ps0, 
        transitions = Ts0,
        arcs = As,
        digraph = G}) ->
    digraph:del_edges(G, digraph:edges(G)),
    digraph:del_vertices(G, digraph:vertices(G)),
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

flush() ->
    receive
        _ -> 
            flush()
    after 0 ->
        ok
    end.

algorithms() -> 
    [
        #slicer{
            name = "Llorens et al.'s slicer (precise)", 
            short_name = "llorens_prec",
            function = fun pn_slice:slice_imp/2},
        #slicer{
            name = "Llorens et al.'s slicer", 
            short_name = "llorens",
            function = fun pn_slice:slice/2},
        #slicer{
            name = "Rakow's slicer CTL",
            short_name = "rakow_ctl", 
            function = fun pn_rakow:slice_ctl/2},
        #slicer{
            name = "Yu et al.'s slicer", 
            short_name = "yu",
            function = fun pn_yuetal:slice/2},
        #slicer{
            name = "Rakow's slicer safety", 
            short_name = "rakow_safety",
            function = fun pn_rakow:slice_safety/2}
    ].

size(#petri_net{places = Ps, transitions = Ts}) ->
    dict:size(Ps) + dict:size(Ts).

new_pn_fresh_digraph(PN = #petri_net{digraph = G}) ->
    NG = digraph_utils:subgraph(G, digraph:vertices(G)),
    PN#petri_net{digraph = NG}.

-module( pn_khan ).
 
-export( [slice_abs/2] ).

-include("pn.hrl").

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract slicing algorithm for Low-level Petri nets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_abs(PN0, SC) ->
    PN = #petri_net{transitions = T, digraph = G} = 
        pn_lib:new_pn_fresh_digraph(PN0),
    PDone = 
        sets:new(), 
    SetSC = 
        sets:from_list(SC),
    T_ = 
        sets:from_list(
            lists:filter(
                fun(K) -> 
                    InK = 
                        sets:from_list(digraph:in_neighbours(G, K)),
                    OutK = 
                        sets:from_list(digraph:out_neighbours(G, K)),
                    Union = 
                        sets:union(InK, OutK),
                    IntSC = 
                        sets:intersection(SetSC, Union),
                    SomeIOPlaceInSC = 
                        IntSC /= sets:new(),
                    IntSCLoops = 
                        [sets:is_element(P, InK) xor sets:is_element(P, OutK) 
                        || P <- sets:to_list(IntSC)],
                    SomeIOPlaceHasNoLoop = 
                        lists:any(fun(Bool) -> Bool end, IntSCLoops),
                    SomeIOPlaceInSC and SomeIOPlaceHasNoLoop  
                end,
                dict:fetch_keys(T))),
    % io:format("T_: ~p\n", [lists:sort(sets:to_list(T_))]),
    P_ =
        sets:fold(
            fun(CT, CP_) ->
                sets:union(
                    sets:from_list(digraph:in_neighbours(G, CT)),
                    CP_)
            end,
            SetSC,
            T_),
    % io:format("P_: ~p\n", [lists:sort(sets:to_list(P_))]),
    [{Ps, Ts}] = 
        pn_lib:slice_rec(
            PN, 
            P_, 
            T_, 
            PDone, 
            fun(I, _, _, _) -> 
                sets:from_list(I)
            end,
            % Discard loops as in the algorithm
            fun(I, O) -> I xor O end,
            false),
    Slice1 = 
        pn_lib:filter_pn(PN, {Ps, Ts}),
    {Slice2, Ps1, Ts1} = 
        neutral_trans_removal(Slice1, SC),
    pn_lib:filter_pn(Slice2, {Ps1, Ts1}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Neutral Transitions Removal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neutral_trans_removal(PN = #petri_net{transitions = T}, SC) ->
    {NPN, Removed} = 
        neutral_trans_removal(
            dict:fetch_keys(T), 
            PN, 
            SC, 
            false),
    case Removed of 
        false ->
            #petri_net{transitions = NT, places = NP} = 
                NPN,
            {
                NPN, 
                sets:from_list(dict:fetch_keys(NP)), 
                sets:from_list(dict:fetch_keys(NT))
            };
        true ->
            neutral_trans_removal(NPN, SC)
    end.


neutral_trans_removal([], PN, _, Removed) ->
    {PN, Removed};
neutral_trans_removal([T | TsPending], PN, SC, Removed) ->
    #petri_net{digraph = G, places = Ps, transitions = Ts, arcs = As} = 
        PN,
    {NPN, NRemoved} = 
        case 
            {digraph:in_neighbours(G, T),
             digraph:out_neighbours(G, T)}
        of
            {[P], [P_]} ->
                case digraph:out_neighbours(G, P) of 
                    [T] ->
                        case (lists:member(P, SC) or lists:member(P_, SC)) of 
                            false -> 
                                PlaceP = dict:fetch(P, Ps),
                                PlaceP_ = dict:fetch(P_, Ps),
                                NPlaceP_ = 
                                    PlaceP_#place{
                                        marking = 
                                                PlaceP#place.marking
                                            +   PlaceP_#place.marking
                                    },
                                NPs = 
                                    dict:erase(P, Ps),
                                NTs = 
                                    dict:erase(T, Ts),
                                NPs1 = 
                                    dict:store(P_, NPlaceP_, NPs),
                                % We assume here that the internal while of the neutral transtions removal is connecting all the t' that arrives to p to p', taking into account that t' can already be conected to p' 
                                NAs = 
                                    lists:foldl(
                                        fun(A = #arc{source = TA, target = PA}, Acc) ->
                                            case PA of 
                                                P -> 
                                                    [#arc{source = TA, target = P_} | Acc];
                                                _ ->
                                                    [A | Acc]
                                            end 
                                        end,
                                        [],
                                        As),
                                NPN_ = 
                                    PN#petri_net{
                                        places = NPs1, 
                                        transitions = NTs, 
                                        arcs = NAs
                                    },
                                pn_lib:build_digraph(NPN_),
                                {NPN_, true};
                            true ->
                               {PN, Removed}
                        end;
                    _ ->
                        {PN, Removed}
                end;
            _ ->
                {PN, Removed}
        end,
    neutral_trans_removal(TsPending, NPN, SC, NRemoved).

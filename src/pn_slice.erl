-module( pn_slice ).
 
-export( [slice/2, slice_imp/2] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice(PN, SC) ->
    {PsB, TsB} = backward_slice(PN, SC, [], {[], []}),

    % Without intersection
    BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
    {PsF, TsF} = forward_slice(BPN),
    pn_lib:filter_pn(BPN, {PsF, TsF}).

    % With intersection
    % {PsF, TsF} = forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).
    
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
        = pn_run:set_enabled_transitions(PN),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing Improved
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_imp(PN, SC) ->
    ListOfPsBTsB = backward_slice_imp(PN, SC, [], {[], []}),
    % [ io:format("~p\n", [{P, [{T, {sets:to_list(PsT), sets:to_list(TsT)}} || {T, {PsT, TsT}} <- BI]}]) || {P, BI} <- Bif],
    % io:format("Bif: ~p\n", [Bif]),
    % io:format("~p\n", [ListOfPsBTsB]),

    % Without intersection
    ListOfPsFTsF=
        lists:map(
            fun({PsB, TsB}) ->
                BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
                {BPN, forward_slice_imp(BPN)}
            end,
            ListOfPsBTsB),
    % io:format("~p\n", [ListOfPsFTsF]),
    ListOfPsFTsFFIltered = 
        filterWOSC(ListOfPsFTsF, sets:from_list(SC)),
    {BPN, {PsF, TsF}} = 
        take_smallest_net(ListOfPsFTsFFIltered, PN),
    pn_lib:filter_pn(BPN, {PsF, TsF}).
    
    % With intersection
    % {PsF, TsF} = forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).

filterWOSC(List, SC) ->
    lists:foldl(
        fun(Elem = {_, {Ps, _}}, Acc) ->
            case sets:intersection(SC, Ps) == [] of 
                false ->
                    [Elem | Acc];
                true ->
                    Acc 
            end
        end,
        [],
        List). 

take_smallest_net(List, OriPN) ->
    WithSize = 
        lists:map(
            fun({PN, Net = {Ps, Ts}}) ->
                {sets:size(Ps) +  sets:size(Ts),
                 PN,
                 Net}
            end,
            List),
    Sorted = 
        lists:sort(
            fun({Size1, _, _}, {Size2, _, _}) ->
                Size1 < Size2
            end,
            WithSize),
    case first_not_zero(Sorted) of 
        none ->
            {OriPN, {sets:new(), sets:new()}};
        {_,PN, Smallest} ->
            {PN, Smallest}
    end.
    
first_not_zero([H = {0, _, _}]) ->
    H;
first_not_zero([{0, _, _} | T]) ->
    first_not_zero(T);
first_not_zero([H | _]) ->
    H;
first_not_zero([]) ->
    none.

backward_slice_imp(PN = #petri_net{digraph = G}, [P | W], Done, {PsS, TsS}) ->
    InTs = 
        digraph:in_neighbours(G, P),
    NDone = 
        [P | Done],
    NPs = 
        [P | PsS],
    case InTs of 
        [] -> 
            backward_slice_imp(PN, lists:usort(W -- NDone), NDone, {NPs, TsS});
        [T] ->
            InPs = digraph:in_neighbours(G, T),
            backward_slice_imp(PN, lists:usort((W ++ InPs) -- NDone), NDone, {NPs, [T | TsS]});
        _ ->
            % All branches
            lists:concat(
               lists:map(
                    fun(T) ->
                        InPs = digraph:in_neighbours(G, T),
                        backward_slice_imp(
                            PN, 
                            lists:usort((InPs ++ W) -- NDone), 
                            NDone, 
                            {NPs, [T | TsS]}) 
                    end,
                    InTs))
    end;
backward_slice_imp(_, [], _, {PsS, TsS}) ->
    [{sets:from_list(PsS), sets:from_list(TsS)}].

% remove_useless({Ps, Ts}) ->
%     InterPs = sets:intersection(Ps),
%     InterTs = sets:intersection(Ts),
%     DiffPs = 
%         lists:map(
%             fun(P) ->
%                 sets:subtract(P, InterPs)
%             end,
%             Ps),
%     DiffTs = 
%         lists:map(
%             fun(T) ->
%                 sets:subtract(T, InterPs)
%             end,
%             Ts),
%     [{KPs, KTs} | _] = 
%         lists:sort(
%             fun({DP1,DT1}, {DP2,DT2}) ->
%                 (sets:size(DP1) + sets:size(DT1)) =< (sets:size(DP2) + sets:size(DT2))
%             end,
%             lists:zip(DiffPs, DiffTs)),
%     {sets:to_list(InterPs), sets:union(InterPs, KPs), sets:union(InterTs, KTs)}.
    
forward_slice_imp(PN = #petri_net{places = Ps}) ->
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
        = pn_run:set_enabled_transitions(PN),
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
    forward_slice_imp(
        PN,
        sets:from_list(StartingPs), 
        sets:from_list([]), 
        sets:from_list(StartingTs)).

forward_slice_imp(PN = #petri_net{transitions = Ts, digraph = G}, W, R, V) ->
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
            forward_slice_imp(PN, NW, NR, NV)
    end.
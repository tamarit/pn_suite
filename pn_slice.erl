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
    {{PsB, TsB}, _, _} = backward_slice_imp(PN, SC, [], {[], []}, []),
    % [ io:format("~p\n", [{P, [{T, {sets:to_list(PsT), sets:to_list(TsT)}} || {T, {PsT, TsT}} <- BI]}]) || {P, BI} <- Bif],
    % io:format("Bif: ~p\n", [Bif]),

    % Without intersection
    BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
    {PsF, TsF} = forward_slice_imp(BPN),
    pn_lib:filter_pn(BPN, {PsF, TsF}).
    
    % With intersection
    % {PsF, TsF} = forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).
    
backward_slice_imp(PN = #petri_net{digraph = G}, [P | W], Done, {PsS, TsS}, Bif) ->
    InTs = 
        digraph:in_neighbours(G, P),
    NDone = 
        [P | Done],
    NPs = 
        [P | PsS],
    case InTs of 
        [] -> 
            backward_slice_imp(PN, lists:usort(W -- NDone), NDone, {NPs, TsS}, Bif);
        [T] ->
            InPs = digraph:in_neighbours(G, T),
            backward_slice_imp(PN, lists:usort((W ++ InPs) -- NDone), NDone, {NPs, [T | TsS]}, Bif);
        _ ->
            % Anaydir bifurcacio
            BifInfo =
               lists:map(
                    fun(T) ->
                        InPs = digraph:in_neighbours(G, T),
                        {{CPs, CTs}, CDone, CBif} = 
                            backward_slice_imp(PN, lists:usort(InPs -- NDone), NDone, {NPs, [T | TsS]}, Bif),
                        % {FCPs, FCTs} = 
                        %     { sets:subtract(CPs, sets:from_list(NPs)),
                        %       sets:subtract(CTs, sets:from_list([T | TsS]))},
                        {T, {CPs, CTs}, CBif, CDone}       
                    end,
                    InTs),
            {FPs0, FTs0} = 
                lists:unzip([PsTs || {_, PsTs, _, _} <- BifInfo]),
            FBifInfo =
                [ {T, { sets:subtract(CPs, sets:from_list(NPs)),
                        sets:subtract(CTs, sets:from_list(TsS))}} 
                 || {T, {CPs, CTs} , _, _} <- BifInfo],
            NBif = 
                [ {P, FBifInfo} 
                | lists:concat([BifBI || {_, _ , BifBI, _} <- BifInfo])],
            % io:format("~p\n", [{P, lists:concat([BifBI || {_, _ , BifBI, _} <- BifInfo])}]), 
            % FDone = 
            %     lists:usort([DoneBI || {_, _ , _, DoneBI} <- BifInfo]),
            % FPs = sets:to_list(sets:union(FPs0)),
            % FTs = sets:to_list(sets:union(FTs0)),

            {UInter, FPs1, FTs1} = remove_useless({FPs0, FTs0}),
            FPs = sets:to_list(FPs1),
            FTs = sets:to_list(FTs1),
            FDone = lists:usort(UInter ++ NDone), 
            backward_slice_imp(PN, lists:usort(W -- FDone), FDone, {FPs, FTs}, NBif)
    end;
    % InPs = 
    %     lists:concat([digraph:in_neighbours(G, T) || T <- InTs]),
    % backward_slice_imp(PN, lists:usort((W ++ InPs) -- Done), [P|Done], {PsS, InTs ++ TsS});
backward_slice_imp(_, [], Done, {PsS, TsS}, Bif) ->
    {{sets:from_list(PsS), sets:from_list(TsS)}, lists:sort(Done), lists:usort(Bif)}.

remove_useless({Ps, Ts}) ->
    InterPs = sets:intersection(Ps),
    InterTs = sets:intersection(Ts),
    DiffPs = 
        lists:map(
            fun(P) ->
                sets:subtract(P, InterPs)
            end,
            Ps),
    DiffTs = 
        lists:map(
            fun(T) ->
                sets:subtract(T, InterPs)
            end,
            Ts),
    [{KPs, KTs} | _] = 
        lists:sort(
            fun({DP1,DT1}, {DP2,DT2}) ->
                (sets:size(DP1) + sets:size(DT1)) =< (sets:size(DP2) + sets:size(DT2))
            end,
            lists:zip(DiffPs, DiffTs)),
    {sets:to_list(InterPs), sets:union(InterPs, KPs), sets:union(InterTs, KTs)}.
    
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
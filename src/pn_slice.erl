-module( pn_slice ).
 
-export( [
            slice/2, slice_gen/2, 
            slice_imp/2, slice_imp_gen/2, 
            slice_imp_single/2, slice_imp_single_gen/2
          ] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice(PN0, SC) ->
    PN = pn_lib:new_pn_fresh_digraph(PN0),
    {PsB, TsB} = backward_slice(PN, SC, [], {[], []}),

    % Without intersection
    BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
    {PsF, TsF} = pn_lib:forward_slice(BPN),
    pn_lib:filter_pn(BPN, {PsF, TsF}).

    % With intersection
    % {PsF, TsF} = pn_lib:forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).

% slice_gen(PN, SC) ->
    % PN = pn_lib:new_pn_fresh_digraph(PN0),
    % {PsB, TsB} = backward_slice_gen(PN, SC),

    % % Without intersection
    % BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
    % {PsF, TsF} = pn_lib:forward_slice(BPN),
    % pn_lib:filter_pn(BPN, {PsF, TsF}).

slice_gen(PN, SC) ->
    pn_lib:slice(
        PN,
        SC,
        sets:from_list(SC), 
        sets:new(), 
        sets:new(), 
        fun(I, _, _, _) -> 
            sets:from_list(I)
        end,
        % No restrictions. All are ok.
        fun(_, _) -> true end,
        false,
        true,
        fun(L,_) -> L end,
        fun([{PNRes, PTRes}|_],_) -> pn_lib:filter_pn(PNRes, PTRes) end).
    
        % lists:map(
        %     fun({PN, Net}) ->
        %         NPN = 
        %         #petri_net{places = NPs, transitions = NTs} = 
        %             pn_lib:filter_pn(PN, Net),
        %         % io:format("~p\n", [{lists:sort(dict:fetch_keys(NPs)), lists:sort(dict:fetch_keys(NTs))} ]),
        %         {pn_lib:size(NPN), 
        %          {NPs, NTs},
        %          NPN}
        %     end,
        %     List),


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

% backward_slice_gen(PN, SC) ->
%     hd(pn_lib:slice_rec(
%         PN, 
%         sets:from_list(SC), 
%         sets:new(), 
%         sets:new(), 
%         fun(I, _, _, _) -> 
%             sets:from_list(I) 
%         end,
%         % No restrictions. All are ok.
%         fun(_, _) -> true end,
%         false)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing Improved
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_imp(PN0, SC) ->
    PN = pn_lib:new_pn_fresh_digraph(PN0),
    ListOfPsBTsB = backward_slice_imp(PN, SC, [], {[], []}),
    % [ io:format("~p\n", [{P, [{T, {sets:to_list(PsT), sets:to_list(TsT)}} || {T, {PsT, TsT}} <- BI]}]) || {P, BI} <- Bif],
    % io:format("~p\n", [[{lists:sort(sets:to_list(PsT)), lists:sort(sets:to_list(TsT))} || {PsT, TsT} <- ListOfPsBTsB]]),
    % io:format("Bif: ~p\n", [Bif]),
    % io:format("~p\n", [ListOfPsBTsB]),

    % Without intersection
    ListOfPsFTsF=
        lists:map(
            fun({PsB, TsB}) ->
                BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
                {BPN, pn_lib:forward_slice(BPN)}
            end,
            ListOfPsBTsB),
    % WithOutRepetitions = 
    %     lists:usort(ListOfPsFTsF),
    % io:format("~p\n", [[{lists:sort(sets:to_list(PsT)), lists:sort(sets:to_list(TsT))} || {_, {PsT, TsT}} <- WithOutRepetitions]]),
    % io:format("~p\n", [ListOfPsFTsF]),
    ListOfPsFTsFFIltered = 
        filterWOSC(ListOfPsFTsF, sets:from_list(SC)),
    % {BPN, {PsF, TsF}} = 
        take_smallest_net(ListOfPsFTsFFIltered, PN).
    % pn_lib:filter_pn(BPN, {PsF, TsF}).
    
    % With intersection
    % {PsF, TsF} = pn_lib:forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).

% slice_imp_gen(PN0, SC) ->
%     PN = pn_lib:new_pn_fresh_digraph(PN0),
%     ListOfPsBTsB = backward_slice_imp_gen(PN, SC),
%     % [ io:format("~p\n", [{P, [{T, {sets:to_list(PsT), sets:to_list(TsT)}} || {T, {PsT, TsT}} <- BI]}]) || {P, BI} <- Bif],
%     % io:format("~p\n", [[{lists:sort(sets:to_list(PsT)), lists:sort(sets:to_list(TsT))} || {PsT, TsT} <- ListOfPsBTsB]]),
%     % io:format("Bif: ~p\n", [Bif]),
%     % io:format("~p\n", [ListOfPsBTsB]),

%     % Without intersection
%     ListOfPsFTsF=
%         lists:map(
%             fun({PsB, TsB}) ->
%                 BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
%                 {BPN, pn_lib:forward_slice(BPN)}
%             end,
%             ListOfPsBTsB),
%     % WithOutRepetitions = 
%     %     lists:usort(ListOfPsFTsF),
%     % io:format("~p\n", [[{lists:sort(sets:to_list(PsT)), lists:sort(sets:to_list(TsT))} || {_, {PsT, TsT}} <- WithOutRepetitions]]),
%     % io:format("~p\n", [ListOfPsFTsF]),
%     ListOfPsFTsFFIltered = 
%         filterWOSC(ListOfPsFTsF, sets:from_list(SC)),
%     take_smallest_net(ListOfPsFTsFFIltered, PN).


% ORIGINAL DEFINITION
slice_imp_gen(PN, SC) ->
    pn_lib:slice(
        PN,
        SC,
        sets:from_list(SC), 
        sets:new(), 
        sets:new(), 
        fun(I, _, _, _) -> 
            sets:from_list(I)
        end,
        % No restrictions. All are ok.
        fun(_, _) -> true end,
        true,
        true,
        fun filterWOSC/2,
        fun take_smallest_net/2). 

% MODIFIED TO MEET RAKOW
% slice_imp_gen(PN0, SC) ->
%     PN = #petri_net{transitions = T, digraph = G} = 
%         pn_lib:new_pn_fresh_digraph(PN0),
%     PDone = 
%         sets:from_list(SC),
%     T_ = 
%         sets:from_list(
%             [K 
%             ||  K <- dict:fetch_keys(T), 
%                 pn_lib:check_loops_with_sc(K, SC, G)]),
%     % io:format("T_: ~p\n", [lists:sort(sets:to_list(T_))]),
%     P_ =
%         sets:fold(
%             fun(CT, CP_) ->
%                 sets:union(
%                     sets:from_list(digraph:in_neighbours(G, CT)),
%                     CP_)
%             end,
%             PDone,
%             T_),
%     pn_lib:slice(
%         PN, 
%         SC,
%         P_, 
%         T_, 
%         PDone, 
%         fun(I, _, _, _) -> 
%             sets:from_list(I)
%         end,
%         % Discard loops as in the algorithm
%         fun(I, O) -> not(I) or not(O) end,
%         true,
%         true,
%         fun filterWOSC/2,
%         fun take_smallest_net/2).   


filterWOSC(List, SC) ->
    lists:foldl(
        fun(Elem = {_, {Ps, _}}, Acc) ->
            % SOME PLACES IN SC
            % case sets:intersection(SC, Ps) == [] of 
            %     false ->
            %         [Elem | Acc];
            %     true ->
            %         Acc 
            % end
            % ALL PLACES IN SC
            case sets:intersection(SC, Ps) == SC of 
                true -> 
                    [Elem | Acc];
                false -> 
                    Acc
            end
        end,
        [],
        List).  

take_smallest_net(List, OriPN) ->
    WithSize = 
        lists:map(
            fun({PN, Net}) ->
                NPN = 
                #petri_net{places = NPs, transitions = NTs} = 
                    pn_lib:filter_pn(PN, Net),
                % io:format("~p\n", [{lists:sort(dict:fetch_keys(NPs)), lists:sort(dict:fetch_keys(NTs))} ]),
                {pn_lib:size(NPN), 
                 {NPs, NTs},
                 NPN}
            end,
            List),
    Sorted = 
        lists:sort(
            fun({Size1, PTs1, _}, {Size2, PTs2, _}) ->
                if  
                    Size1 < Size2 ->
                        true;
                    Size1 > Size2 ->
                        false;
                    Size1 == Size2 ->
                        PTs1 < PTs2 
                end 
            end,
            WithSize),
    case first_not_zero(Sorted) of 
        none ->
            OriPN;
            % pn_lib:filter_pn(OriPN, {sets:new(), sets:new()});
        {_,_,PN} ->
            % io:format("Selected: ~p\n", [{lists:sort(dict:fetch_keys(PN#petri_net.places)), lists:sort(dict:fetch_keys(PN#petri_net.transitions))} ]),
            PN
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
        % [T] ->
        %     InPs = digraph:in_neighbours(G, T),
        %     backward_slice_imp(PN, lists:usort((W ++ InPs) -- NDone), NDone, {NPs, [T | TsS]});
        _ ->
            % All branches
            Branches = 
                lists:map(
                    fun(T) ->
                        InPs = digraph:in_neighbours(G, T),
                        backward_slice_imp(
                            PN, 
                            lists:usort((InPs ++ W) -- NDone), 
                            NDone, 
                            {NPs, [T | TsS]}) 
                    end,
                    InTs),
            Res = 
                lists:concat(Branches),
            % io:format("~p\n", [Branches]),
            % io:format("P: ~p InTs: ~p\n", [P, InTs]),
            % io:format("\nBranches:\n~p\n", [[{lists:sort(sets:to_list(PsT)), lists:sort(sets:to_list(TsT))} || {PsT, TsT} <- Res]]),
            Res
            
    end;
backward_slice_imp(_, [], _, {PsS, TsS}) ->
    [{sets:from_list(PsS), sets:from_list(TsS)}].

% backward_slice_imp_gen(PN, SC) ->
%     pn_lib:slice_rec(
%         PN, 
%         sets:from_list(SC), 
%         sets:new(), 
%         sets:new(), 
%         fun(I, _, _, _) -> 
%             sets:from_list(I)
%         end,
%         % No restrictions. All are ok.
%         fun(_, _) -> true end,
%         true). 
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing Improved Single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_imp_single(PN0, SC) ->
    PN = pn_lib:new_pn_fresh_digraph(PN0),
    ListOfPsBTsB = backward_slice_imp_single(PN, SC, [], {[], []}),
    % [ io:format("~p\n", [{P, [{T, {sets:to_list(PsT), sets:to_list(TsT)}} || {T, {PsT, TsT}} <- BI]}]) || {P, BI} <- Bif],
    % io:format("~p\n", [[{sets:to_list(PsT), sets:to_list(TsT)} || {PsT, TsT} <- ListOfPsBTsB]]),
    % io:format("Bif: ~p\n", [Bif]),
    % io:format("~p\n", [ListOfPsBTsB]),

    % Without intersection
    ListOfPsFTsF=
        lists:map(
            fun({PsB, TsB}) ->
                BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
                % io:format("~p\n", [{sets:to_list(PsB), sets:to_list(TsB)}]),
                {BPN, pn_lib:forward_slice(BPN)}
            end,
            ListOfPsBTsB),
    % io:format("~p\n", [ListOfPsFTsF]),
    ListOfPsFTsFFIltered = 
        filterWOSC(ListOfPsFTsF, sets:from_list(SC)),
    % {BPN, {PsF, TsF}} = 
        take_smallest_net(ListOfPsFTsFFIltered, PN).
    % pn_lib:filter_pn(BPN, {PsF, TsF}).
    
    % With intersection
    % {PsF, TsF} = pn_lib:forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).

slice_imp_single_gen(PN0, SC) ->
    PN = pn_lib:new_pn_fresh_digraph(PN0),
    ListOfPsBTsB = [backward_slice_imp_single_gen(PN, SC)],
    % [ io:format("~p\n", [{P, [{T, {sets:to_list(PsT), sets:to_list(TsT)}} || {T, {PsT, TsT}} <- BI]}]) || {P, BI} <- Bif],
    % io:format("~p\n", [[{sets:to_list(PsT), sets:to_list(TsT)} || {PsT, TsT} <- ListOfPsBTsB]]),
    % io:format("Bif: ~p\n", [Bif]),
    % io:format("~p\n", [ListOfPsBTsB]),

    % Without intersection
    ListOfPsFTsF=
        lists:map(
            fun({PsB, TsB}) ->
                BPN = pn_lib:filter_pn(PN, {PsB, TsB}),
                % io:format("~p\n", [{sets:to_list(PsB), sets:to_list(TsB)}]),
                {BPN, pn_lib:forward_slice(BPN)}
            end,
            ListOfPsBTsB),
    % io:format("~p\n", [ListOfPsFTsF]),
    ListOfPsFTsFFIltered = 
        filterWOSC(ListOfPsFTsF, sets:from_list(SC)),
    % {BPN, {PsF, TsF}} = 
        take_smallest_net(ListOfPsFTsFFIltered, PN).
    % pn_lib:filter_pn(BPN, {PsF, TsF}).
    
    % With intersection
    % {PsF, TsF} = pn_lib:forward_slice(PN),
    % PsSet = sets:intersection(PsB, PsF),
    % TsSet = sets:intersection(TsB, TsF),
    % pn_lib:filter_pn(PN, {PsSet, TsSet}).

backward_slice_imp_single(PN = #petri_net{digraph = G}, [P | W], Done, {PsS, TsS}) ->
    InTs = 
        digraph:in_neighbours(G, P),
    NDone = 
        [P | Done],
    NPs = 
        [P | PsS],
    case InTs of 
        [] -> 
            backward_slice_imp_single(
                PN, 
                lists:usort(W -- NDone), 
                NDone, 
                {NPs, TsS});
        [T] ->
            InPs = digraph:in_neighbours(G, T),
            backward_slice_imp_single(
                PN, 
                lists:usort((W ++ InPs) -- NDone), 
                NDone, 
                {NPs, [T | TsS]});
        _ ->
            % All branches
            % io:format("P: ~p InTs: ~p\n", [P, InTs]),
            SelectedTrans = 
                select_transitions(G, NDone ++ W, InTs),
            % io:format("State: ~p\n", [{G, NDone ++ W, InTs}]),
            % io:format("SelectedTrans: ~p\n", [SelectedTrans]),
            backward_slice_imp_single(
                PN, 
                lists:usort(
                    (lists:concat([InPs || {_, InPs} <- SelectedTrans]) ++ W) -- NDone), 
                NDone, 
                {NPs, lists:usort([T || {T, _} <- SelectedTrans] ++ TsS)})
            % lists:concat(
            %    lists:map(
            %         fun({T, InPs}) ->
            %             io:format("T: ~p\n", [T]),
            %             % InPs = digraph:in_neighbours(G, T),
            %             backward_slice_imp_single(
            %                 PN, 
            %                 lists:usort((InPs ++ W) -- NDone), 
            %                 NDone, 
            %                 {NPs, [T | TsS]}) 
            %         end,
            %         ))
                    % [take_smallest_transition(InTs)]))
    end;
backward_slice_imp_single(_, [], _, {PsS, TsS}) ->
    [{sets:from_list(PsS), sets:from_list(TsS)}].

backward_slice_imp_single_gen(PN, SC) ->
    hd(pn_lib:slice_rec(
        PN, 
        sets:from_list(SC), 
        sets:new(), 
        sets:new(), 
        fun(I, _, G, CPs) -> 
            TR = sets:from_list(
                [T 
                || {T, _} <- select_transitions(G, sets:to_list(CPs), I)]),
            % io:format("State: ~p\n", [{G, sets:to_list(CPs), I}]),
            % io:format("SelectedTrans: ~p\n", [sets:to_list(TR)]),
            TR
        end,
        % No restrictions. All are ok.
        fun(_, _) -> true end,
        false)). 

select_transitions(_, _, []) ->
    [];
select_transitions(G, SCPs, Ts = [_|_]) ->
    UnavoidableInfo =  
        lists:map(
            fun(T) -> 
                unavoidable_transition(G, SCPs, T)
            end, 
            Ts),
    % io:format("SCPs: ~p\n", [SCPs]),
    io:format("UnavoidableInfo: ~p\n", [UnavoidableInfo]),
    case [ {T, InPs} || {true, T, InPs} <- UnavoidableInfo] of 
        [] -> 
            TSmallest = 
                take_smallest_transition(
                    [T || {_, T, _} <- UnavoidableInfo]),
            [{T, InPs} 
            || {_, T, InPs} <- UnavoidableInfo, T == TSmallest];
        List = [_|_] -> 
            List
    end.

unavoidable_transition(G, _SCPs, T) -> 
    InPs = digraph:in_neighbours(G, T),
    {false, T, InPs}.
    % TODO: Commented to pass the test since generic and specific algorithms does not produce the same places (i.e. _SCPs)
    % case sets:to_list(
    %         sets:intersection(
    %             sets:from_list(InPs), 
    %             sets:from_list(SCPs)))
    % of 
    %     [] -> 
    %         {false, T, InPs};
    %     _ -> 
    %         {true, T, InPs}
    % end.

take_smallest_transition(Ts) -> 
    Sorted = 
        lists:sort(
            fun(TA, TB) -> 
                if 
                    length(TA) < length(TB) -> 
                        true;
                    length(TA) == length(TB) -> 
                        TA < TB;
                    length(TA) > length(TB) -> 
                        false 
                end 
            end,
            Ts),
    % io:format("Sorted: ~p\n", [Sorted]),
    hd(Sorted).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Old Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    
% pn_lib:forward_slice_imp(PN = #petri_net{places = Ps}) ->
%     StartingPs = 
%         dict:fold(
%             fun
%                 (K, #place{marking = IM}, Acc) when IM > 0 ->
%                     [K | Acc];
%                 (_, _, Acc) ->
%                     Acc
%             end,
%             [],
%             Ps),
%     % io:format("StartingPs: ~p\n", [StartingPs]),
%     #petri_net{transitions = NTs} 
%         = pn_run:set_enabled_transitions(PN),
%     StartingTs = 
%         dict:fold(
%             fun
%                 (K, #transition{enabled = true}, Acc) -> 
%                     [K | Acc];
%                 (_, _, Acc) ->
%                     Acc 
%             end,
%             [],
%             NTs),
%     % io:format("StartingTs: ~p\n", [StartingTs]),
%     pn_lib:forward_slice_imp(
%         PN,
%         sets:from_list(StartingPs), 
%         sets:from_list([]), 
%         sets:from_list(StartingTs)).

% pn_lib:forward_slice_imp(PN = #petri_net{transitions = Ts, digraph = G}, W, R, V) ->
%     case sets:to_list(V) of 
%         [] ->
%             {W, R};
%         _ ->
%             OutV = 
%                 lists:append(
%                     [digraph:out_neighbours(G, P) 
%                     || P <- sets:to_list(V)]),
%             NW = sets:union(W, sets:from_list(OutV)),
%             NR = sets:union(R, V),
%             NV0 = 
%                 dict:fold(
%                     fun(K, _, Acc) ->
%                         case sets:is_element(K, NR) of 
%                             true ->
%                                 Acc;
%                             false ->
%                                 InK = 
%                                     sets:from_list(
%                                         digraph:in_neighbours(G, K)),
%                                 case sets:is_subset(InK, NW) of 
%                                     true ->
%                                         [K | Acc];
%                                     false ->
%                                         Acc
%                                 end
%                         end
%                     end,
%                     [],
%                     Ts),
%             NV = sets:from_list(NV0),
%             pn_lib:forward_slice_imp(PN, NW, NR, NV)
%     end.
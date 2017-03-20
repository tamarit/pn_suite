-module( pn_rakow ).
 
-export( [slice_ctl/2, slice_safety/2] ).

-include("pn.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CTL Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_ctl(PN0, SC) ->
    PN = pn_lib:new_pn_fresh_digraph(PN0),
    {Ps, Ts} = 
        pn_lib:slice_rec(
            PN, 
            sets:from_list(SC), 
            sets:new(), 
            sets:new(), 
            fun(I, O) -> 
                sets:union(
                    sets:from_list(I), 
                    sets:from_list(O)) 
            end,
            % Discard loops as in the algorithm
            fun(I, O) -> not(I) or not(O) end),
    pn_lib:filter_pn(PN, {Ps, Ts}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safety Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_safety(PN0, SC) ->
    PN = #petri_net{transitions = T, digraph = G} = pn_lib:new_pn_fresh_digraph(PN0),
    PDone = 
        sets:from_list(SC),
    T_ = 
        sets:from_list(
            [K 
            ||  K <- dict:fetch_keys(T), 
                check_loops_with_sc(K, SC, G)]),
    % io:format("T_: ~p\n", [lists:sort(sets:to_list(T_))]),
    P_ =
        sets:fold(
            fun(CT, CP_) ->
                sets:union(
                    sets:from_list(digraph:in_neighbours(G, CT)),
                    CP_)
            end,
            PDone,
            T_),
    {Ps, Ts} = 
        pn_lib:slice_rec(
            PN, 
            P_, 
            T_, 
            PDone, 
            fun(I, _) -> 
                sets:from_list(I)
            end,
            % fun(I, O) -> not(I and O) end),
            % Discard loops as in the algorithm
            fun(I, O) -> not(I) or not(O) end),
    pn_lib:filter_pn(PN, {Ps, Ts}).
    

check_loops_with_sc(T, [P | Ps] , G) ->
    TinOut =  
        lists:member(T, digraph:in_neighbours(G, P)),
    TinIn = 
        lists:member(T, digraph:out_neighbours(G, P)),
    case  (TinOut xor TinIn) of 
        false -> 
            check_loops_with_sc(T, Ps, G);
        true ->
            true 
    end;
check_loops_with_sc(_, [] , _) ->
    false.




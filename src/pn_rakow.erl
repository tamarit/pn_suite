-module( pn_rakow ).
 
-export( [slice_ctl/2, slice_safety/2] ).

-include("pn.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CTL Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_ctl(PN, SC) ->
    {Ps, Ts} = 
        slice_rec(
            PN, 
            sets:from_list(SC), 
            sets:new(), 
            sets:new(), 
            fun(I, O) -> 
                sets:union(
                    sets:from_list(I), 
                    sets:from_list(O)) 
            end,
            fun(I, O) -> not(I) or not(O) end),
    pn_lib:filter_pn(PN, {Ps, Ts}).
    

slice_rec(PN = #petri_net{digraph = G}, P_, T_, PDone, TsFun, ValidTFun) ->
    io:format("T_: ~p\n", [lists:sort(sets:to_list(T_))]),
    Pending = sets:to_list(sets:subtract(P_, PDone)),
    case Pending of 
        [] ->
            {P_, T_};
        [P|_] ->
            InTs = 
                digraph:in_neighbours(G, P),
            OutTs = 
                digraph:out_neighbours(G, P),
            Ts = 
                sets:to_list(
                    sets:subtract(
                        TsFun(InTs, OutTs),
                        T_)),
            {NP_, NT_} = 
                lists:foldl(
                    fun(T, {CP_, CT_}) -> 
                        TinOut =  
                            lists:member(T, InTs),
                        TinIn = 
                            lists:member(T, OutTs), 
                        case ValidTFun(TinIn, TinOut) of 
                            false -> 
                                {CP_, CT_};
                            true ->
                                {
                                    sets:union(
                                        CP_,
                                        sets:from_list(
                                            digraph:in_neighbours(G, T))
                                        ),
                                    sets:add_element(T, CT_)
                                }
                        end
                    end,
                    {P_, T_},
                    Ts
                    ),
            slice_rec(PN, NP_, NT_, sets:add_element(P, PDone), TsFun, ValidTFun)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safety Slicing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_safety(PN = #petri_net{transitions = T, digraph = G}, SC) ->
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
        slice_rec(
            PN, 
            P_, 
            T_, 
            PDone, 
            fun(I, _) -> 
                sets:from_list(I)
            end,
            % fun(I, O) -> not(I and O) end),
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




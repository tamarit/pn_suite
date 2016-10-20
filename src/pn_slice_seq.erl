-module( pn_slice_seq ).
 
-export( [slice_with_sequence/3] ).

-include("pn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slicing (sequence trans.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice_with_sequence(
    [{T_i, #petri_net{places = Ps_i, digraph = G}}, 
     {T_i_1, PN_i_1 = #petri_net{places = Ps_i_1}} 
     | T], 
    W,
    Slice) ->
    ChangingPs = 
        lists:foldl(
            fun(P, Acc) ->
                M_i = 
                    (dict:fetch(P, Ps_i))#place.marking,
                M_i_1 = 
                    (dict:fetch(P, Ps_i_1))#place.marking,
                % ORIGINAL
                % case (M_i_1 < M_i) of 
                % MODIFIED. Is what we want?
                case (M_i_1 =< M_i) of 
                    true ->
                        [P | Acc];
                    false ->
                        Acc
                end
            end,
            [],
            W),
    case ChangingPs of 
        [] ->
            slice_with_sequence([{T_i_1, PN_i_1} | T], W, Slice);
        _ ->
            InT_i = digraph:in_neighbours(G, T_i),
            slice_with_sequence(
                [{T_i_1, PN_i_1} | T], 
                lists:usort(W ++ InT_i), 
                [T_i | Slice])
    end;
slice_with_sequence(
    [{none, PN}], Ps, Ts) ->
    pn_lib:filter_pn(PN, {sets:from_list(Ps), sets:from_list(Ts)}).
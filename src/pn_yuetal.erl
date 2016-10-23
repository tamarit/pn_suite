-module( pn_yuetal ).
 
-export( [
    slice/2,
    write_sdg/2,
    sdg/2,
    bsg/3
    ] ).

-include("pn.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Slice Yu et al
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slice(PN, SC) ->
    SDG = sdg_sim(PN, SC),
    BSG = bsg_sim(PN, SDG, SC),
    % write_sdg(SDG, "sdg_sim.dot"),
    % write_sdg(BSG, "bsg_sim.dot"),
    pn_lib:filter_pn(PN, sdg_places_trans(BSG)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SDG Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_sdg(SDG, File) ->
    file:write_file(File, 
        list_to_binary(sdg_to_dot_sim(SDG))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SDG Simplification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sdg_sim(PN, SC) ->
    SDG = #sdg{},
    Nodes = [[P] || P <- SC],
    [digraph:add_vertex(SDG#sdg.digraph, N) 
     || N <- Nodes],
    sdg_process_sim(PN#petri_net.digraph, SDG#sdg.digraph, Nodes),
    SDG.

sdg_process_sim(G, SDG, [Q | Pending]) ->
    NPending = 
        lists:foldl(
            fun
                (P_i, CPending1) -> 
                    lists:foldl(
                        fun(T_i, CPending2) -> 
                            sdg_add_edges(T_i, [P_i], G, SDG, CPending2) 
                        end, 
                        CPending1, 
                        digraph:in_neighbours(G, P_i))
            end, 
            Pending, 
            Q),
    % io:format("Entra amb Q: ~p\n", [{Q, NPending}]),
    sdg_process_sim(G, SDG, NPending);
sdg_process_sim(_, _, []) ->
    ok.

sdg_add_edges(T_i, P_i, G, SDG, Pending) ->
    Q_ = lists:sort(digraph:in_neighbours(G, T_i)),
    IsIn = digraph:vertex(SDG, Q_),
    digraph:add_vertex(SDG, Q_),
    [digraph:add_vertex(SDG, [P]) || P <- Q_],
    digraph:add_edge(SDG, Q_, P_i, T_i),
    case IsIn of 
        {Q_, _} ->
            Pending;
        false ->

            [[P] || P <- Q_] ++ Pending
    end.

% Simplemente tira hacia atras desde el SC, y va metiendo los lugares y transiciones que se encuentra hasta que no puede tirar mas hacia atras
% la unica diferencia con el nuestro es que mete un nodo con todos los lugares a los cuales se llega desde uan transición especifica.

sdg_to_dot_sim(#sdg{digraph = G}) ->
    Vs = 
        digraph:vertices(G),
    Es = 
        lists:usort([begin {_, S, T, Label} = digraph:edge(G, E), {S, T, Label} end || E <- digraph:edges(G)]),
    VsDot = 
        lists:map(fun sdg_node_to_dot/1, Vs),
    EsDot = 
        lists:map(fun sdg_edge_to_dot/1, Es),

        "digraph \"SDG\"{\n"
    ++  "ordering=out; ranksep=0.5;\n"
    ++  string:join(VsDot,"\n") ++ "\n"
    ++  string:join(EsDot,"\n") ++ "\n"
    ++ "}"
    .


sdg_places_trans(#sdg{digraph = G}) ->
    PsInSDG = 
        lists:concat([[P || P <- V ] || V <- digraph:vertices(G)]),
    % io:format("~p\n", [PsInSDG]),
    Ps = 
        sets:from_list(PsInSDG),
    Ts = 
        sets:from_list(
            [element(4, digraph:edge(G, E))
            || E <- digraph:edges(G)]),
    {Ps, Ts}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % BSG Simplification
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%

bsg_sim(#petri_net{places = Ps}, #sdg{digraph = SDG}, SC) ->
    S = 
        [ V ||  V <- digraph:vertices(SDG),                         
                lists:all(
                    fun(P) -> 
                        (dict:fetch(P, Ps))#place.marking > 0 
                    end, 
                    V)],
    % io:format("S: ~p\n", [S]),
    BSG = #sdg{},
    G_BSG = BSG#sdg.digraph,
    lists:map(
        fun(N) -> 
            digraph:add_vertex(G_BSG, N) 
        end, 
        S),
    bsg_while_S_sim(SDG, G_BSG, S, []),
    Vs = digraph:vertices(G_BSG),
    InSC = [[P || P <- V, lists:member(P, SC)] || V <- Vs],
    case InSC of 
        [] ->
            #sdg{};
        _ ->  
            BSG
    end.

% Partir de nodes amb marcat

% Cuando procesa un lugar coge tanto ese lugar como todos los lugares a los que se puede llegar con las transciones del bslice que salgan de ese lugar 
% Si p_i -> t -> * coger todas las p_k tal que p_k -> t -> *, es decir todos los lugares que hacen falta para habilitar la transicion
% para toda transicion y para todo lugar desde esa transicion añadirla. Los lugares que no estuviesen hay que recorrelos

bsg_while_S_sim(G_SDG, G_BSG, [S_i | S0], Done) -> 
    % io:format("Entra : ~p\n", [S_i]),
    digraph:add_vertex(G_BSG, S_i),
    R = digraph:vertices(G_SDG),
    R_ = digraph:vertices(G_BSG),
    SKs0 = [N || N <- R, sets:intersection(sets:from_list(S_i), sets:from_list(N)) /= sets:new()],
    % io:format("SKs0: ~p\n", [{S_i,R, SKs0}]),
    % io:format("R_: ~p\n", [R_]),
    SKs = [begin digraph:add_vertex(G_BSG, S_k), S_k end  || S_k <- SKs0, lists:member(S_k -- S_i, R_)],
    % io:format("SKs: ~p\n", [SKs]),
    S = SKs ++ S0,
    OE = digraph:out_edges(G_SDG, S_i),
    case OE of 
        [] -> 
            bsg_while_S_sim(G_SDG, G_BSG, S -- Done, [S_i | Done]);
        _ ->
            NS =
                lists:foldl(
                    fun(OneOE, CS1) ->
                        {OneOE, S_i, _, T_i} = digraph:edge(G_SDG, OneOE),
                        SJs = 
                            [element(3, digraph:edge(G_SDG, E)) 
                             || E <- digraph:edges(G_SDG), 
                                element(4, digraph:edge(G_SDG, E)) == T_i],
                        lists:foldl(
                            fun (S_j, CS2) ->
                                bsg_process_s_j_sim(G_BSG, S_i, S_j, T_i, CS2)
                            end,
                            CS1,
                            SJs)
                    end,
                    S,
                    % OE),
                    % [lists:last(OE)]),
                    [hd(OE)]),
            bsg_while_S_sim(G_SDG, G_BSG, NS -- Done, [S_i | Done])
    end;
bsg_while_S_sim(_, _, [], _) -> 
    ok.


bsg_process_s_j_sim(G_BSG, S_i, S_j, T_i, S) ->
    % io:format("S_j: ~p\n", [S_j]),
    IsIn = digraph:vertex(G_BSG, S_j),
    digraph:add_vertex(G_BSG, S_j),
    digraph:add_edge(G_BSG, S_i, S_j, T_i),
    case IsIn of 
        {S_j, _} ->
            S;
        false ->    
            lists:usort([S_j | S]) 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SDG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sdg(PN, SC) ->
    SDG0 = #sdg{},
    Nodes = [[P] || P <- SC],
    [digraph:add_vertex(SDG0#sdg.digraph, N) 
     || N <- Nodes],
    SDG = SDG0#sdg{structural_nodes = [Nodes]},
    FSDG = sdg_while(PN, SDG, Nodes),
    file:write_file(
        "sdg.dot", 
        list_to_binary(sdg_to_dot(FSDG))),
    FSDG.


sdg_while(PN, SDG, [Q | T]) ->
    % io:format("Entra amb ~p\n", [Q]),
    {NSDG, NPending} = 
        lists:foldl(
            fun
                (P_i, {CSDG, CPending}) -> 
                    sdg_forEveryPN([P_i], PN, CSDG, CPending)
            end, 
            {SDG, T}, 
            Q),
    sdg_while(PN, NSDG, NPending);
sdg_while(_, SDG, []) ->
    SDG.

sdg_forEveryPN([P_i], PN = #petri_net{digraph = G}, SDG, Pending) ->
    % io:format("P_i: ~p, Ts ~p\n", [P_i, digraph:in_neighbours(G, P_i)]),
    lists:foldl(
        fun(T_i, {CSDG, CPending}) -> 
            sdg_forEveryT(T_i, [P_i], PN, CSDG, CPending) 
        end, 
        {SDG, Pending}, 
        digraph:in_neighbours(G, P_i)).


sdg_forEveryT(T_i, P_i, #petri_net{digraph = G}, FullSDG = #sdg{digraph = SDG}, Pending) ->
    Q_ = lists:sort(digraph:in_neighbours(G, T_i)),
    % io:format("P_i: ~p, T_i ~p, Q_ ~p\n", [P_i, T_i, Q_]),
    Vs = digraph:vertices(SDG),
    case search_equal(Vs, Q_) of 
        {ok, N} ->
            digraph:add_edge(SDG, N, P_i, T_i),
            {FullSDG, Pending};
        none ->
            case search_intersection(Vs, Q_) of 
                {ok, N} ->
                    digraph:add_vertex(SDG, Q_),
                    [digraph:add_vertex(SDG, [P]) || P <- Q_],
                    digraph:add_edge(SDG, Q_, P_i, T_i),
                    {
                        add_to_structurals(FullSDG, lists:usort([N, Q_] ++ [[P] || P <- Q_])),
                        [[PQ_] || PQ_ <- Q_] ++ Pending
                    };
                none ->
                    digraph:add_vertex(SDG, Q_),
                    [digraph:add_vertex(SDG, [P]) || P <- Q_],
                    digraph:add_edge(SDG, Q_, P_i, T_i),
                    {
                        add_to_structurals(FullSDG, lists:usort([Q_ | [[P] || P <- Q_]])),
                        [[PQ_] || PQ_ <- Q_] ++ Pending
                    }
            end
    end.

search_equal([C | _], C) ->
    {ok, C};
search_equal([_ | T], Q) ->
    search_equal(T, Q);
search_equal([], _) ->
    none.

search_intersection([C | T], Q) ->
    case [P1 || P1 <- Q, P2 <- C, P1 == P2] of 
        [] ->
            search_intersection(T, Q);
        _ ->
            {ok, C} 
    end;
search_intersection([], _) ->
    none.

add_to_structurals(SDG = #sdg{structural_nodes = SN}, N) ->
    case [Ns|| Ns <- SN, sets:intersection(sets:from_list(Ns), sets:from_list(N)) /= sets:new()] of 
        [Ns] ->
            SDG#sdg{structural_nodes = [lists:usort(N ++ Ns) | (SN -- [Ns])]};
        _ ->
            SDG#sdg{structural_nodes = [N | SN]}
    end.



sdg_to_dot(#sdg{digraph = G, structural_nodes = SN}) ->
    % Vs = 
    %     digraph:vertices(G),
    Es = 
        lists:usort(
            [begin 
                {_, S, T, Label} = digraph:edge(G, E), 
                {S, T, Label} 
             end 
            || E <- digraph:edges(G)]),
    {SNDot, _} = 
        lists:mapfoldl(fun sdg_sn_to_dot/2, 1, SN),
    EsDot = 
        lists:map(fun sdg_edge_to_dot/1, Es),
        "digraph \"SDG\"{\n"
    ++  "ordering=out; ranksep=0.5;\n"
    ++  SNDot ++ "\n"
    ++  string:join(EsDot,"\n")
    ++ "\n}"
    .

places_to_string(Ps) ->
    string:join(Ps, "_").

places_to_string_pp(Ps) ->
    "[" ++ string:join(Ps, ", ") ++ "]".

sdg_node_to_dot(Ps) ->
    StrPs = places_to_string(Ps),
    StrPsPP = places_to_string_pp(Ps),
        StrPs 
    ++ " [shape=ellipse, label=\"" ++ StrPsPP
    ++  "\"];".

sdg_sn_to_dot(SN, N) ->
    SNDot = lists:map(fun sdg_node_to_dot/1, SN),
    DotCluster = 
            "subgraph cluster" ++ integer_to_list(N) ++ "{"
        ++  string:join(SNDot,"\n") ++ "\n"
        ++  "}",
    {DotCluster, N + 1}.

sdg_edge_to_dot({S, T, Label}) ->
    places_to_string(S) ++ " -> "  ++ places_to_string(T) 
    ++ " [label=\"" ++ Label
    ++  "\"];".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BSG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bsg(#petri_net{places = Ps}, #sdg{digraph = G_SDG, structural_nodes = SN}, SC) ->
    % S = R' = (nodos en R con tokens) U (nodos compuestos cuyos todos sus lugares tiene tokens)
    S0 = 
        lists:concat([
                    [ Node 
                    ||  Node <- Group, 
                        lists:all(
                            fun(P) -> 
                                (dict:fetch(P, Ps))#place.marking > 0 
                            end, 
                            Node)] 
                || Group <- SN]),
    % S =
    %     lists:concat([sn_of_node(SN, V) || V <- S0]), 
    S = S0,
    % io:format("S: ~p\n", [S]),
    BSG = #sdg{},
    G_BSG = BSG#sdg.digraph,
    lists:map(
        fun(N) -> 
            digraph:add_vertex(G_BSG, N) 
        end, 
        S),
    bsg_while_S(G_SDG, G_BSG, SN, S),
    Vs = digraph:vertices(G_BSG),
    % if \forall r \in R' -> r \not\in SlicingCriterion
        % BSG = \empty
    InSC = [[P || P <- V, lists:member(P, SC)] || V <- Vs],
    case InSC of 
        [] ->
            #sdg{};
        _ ->  
            NSN = 
                [[N || N <- Group, lists:member(N, Vs)] || Group <- SN],
            FBSG = BSG#sdg{structural_nodes = NSN},
            file:write_file(
                "bsg.dot", 
                list_to_binary(sdg_to_dot(FBSG))),
            % clean structural_nodes leaving only the nodes in R'
            FBSG
    end.

% while (S no est evacio y S tenga successor nodes)
    % foreach s_i \in S
        % Elegir un t_i de los arcos de salida de s_i
            % foreach s_j al que se llega desde el arco etiquetado con t_i
bsg_while_S(G_SDG, G_BSG, SN, [S_i | S0]) -> 
    % io:format("Entra : ~p\n", [S_i]),
    digraph:add_vertex(G_BSG, S_i),
    OE = digraph:out_edges(G_SDG, S_i),

    R = digraph:vertices(G_SDG),
    R_ = digraph:vertices(G_BSG),
    SKs0 = [N || N <- R, sets:intersection(sets:from_list(S_i), sets:from_list(N)) /= sets:new()] -- [S_i],
    % io:format("SKs0: ~p\n", [{S_i,R, SKs0}]),
    % io:format("R_: ~p\n", [R_]),
    SKs = [S_k || S_k <- SKs0, lists:member(S_k -- S_i, R_)],
    % io:format("SKs: ~p\n", [SKs]),
    S = 
        case SKs of 
            [] ->
                S0;
            [S_k | _] ->
                digraph:add_vertex(G_BSG, S_k),
                % FromSNk = sn_of_node(SN, S_k),
                FromSNk = [S_k],
                lists:usort(FromSNk ++ S0) 
        end,

    % S = S0,

    % OE_Info = 
    %     [digraph:edge(G_SDG, E) || E <- OE],
    case OE of 
        [] -> 
            bsg_while_S(G_SDG, G_BSG, SN, S);
        [OneOE|_] ->
            {OneOE, S_i, _, T_i} = digraph:edge(G_SDG, OneOE),
            SJs = 
                [element(3, digraph:edge(G_SDG, E)) 
                 || E <- digraph:edges(G_SDG), 
                    element(4, digraph:edge(G_SDG, E)) == T_i],
            NS = 
                lists:foldl(
                    fun (S_j, CS) ->
                        bsg_process_s_j(G_SDG, G_BSG, SN, S_i, S_j, T_i, CS)
                    end,
                    S,
                    SJs),
            bsg_while_S(G_SDG, G_BSG, SN, NS)
    end;
bsg_while_S(_, _, _, []) -> 
    ok.


bsg_process_s_j(G_SDG, G_BSG, _, S_i, S_j, T_i, S0) ->
    S = S0,

    case digraph:vertex(G_BSG, S_j) of 
        {S_j, _} ->
            case [E || E <- digraph:out_edges(G_BSG, S_i), element(3, digraph:edge(G_SDG, E)) ==  S_j] of 
                [] ->
                    digraph:add_edge(G_BSG, S_i, S_j, T_i),
                    % io:format("No estava arc\n"),
                    S;
                _ ->
                    % io:format("Res que añadir\n"),
                    S 
            end;
        false ->    
            % io:format("Tot nou\n"),
            digraph:add_vertex(G_BSG, S_j),
            digraph:add_edge(G_BSG, S_i, S_j, T_i),
            % FromSNj = sn_of_node(SN, S_j),
            FromSNj = [S_j],
            lists:usort(FromSNj ++ S) 
    end.

% sn_of_node(SNs, N) ->
%     hd([SN || SN <- SNs, lists:member(N, SN)]).


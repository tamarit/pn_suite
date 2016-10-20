-module( pn_run ).
 
-export( [run/3, ask_fired_transition/2, set_enabled_transitions/1] ).

-include("pn.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Execution functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(PN = #petri_net{transitions = Ts}, FunChoose, Executed) ->
    % Extract enabled
    Enabled = 
        dict:fold(
            fun
                (K, #transition{enabled = true, showed_name = SN}, Acc) -> 
                    [{K, SN ++ " - " ++ K} | Acc];
                (_, _, Acc) ->
                    Acc 
            end,
            [],
            Ts),
    case FunChoose(PN, Enabled) of 
        none ->
            {PN, lists:reverse(Executed)};
        error ->
            error;
        Chosen ->
            % execute (place update)
            NPN = fire_transition(Chosen, PN),
            % transitions update 
            NPN2 = set_enabled_transitions(NPN),
            % recursive call
            run(NPN2, FunChoose, [{Chosen, NPN2} | Executed])
    end.

ask_fired_transition(PN, Enabled) ->
    % print
    pn_output:print_net_run(PN),
    % ask which one (with additional options like finish, etc)
    case Enabled of 
        [] ->
            none;
        [{T, SN}] -> 
            io:format("Transition ~s is chosen.\n", [SN]),
            io:get_line(standard_io, ""),
            T;
        _ ->
            SortingFun = fun({_,V1}, {_, V2}) -> V1 < V2 end,
            {_, Lines, Ans, AnsDict} = 
                lists:foldl(
                    fun pn_lib:build_question_option/2,
                    {1, [], [], dict:new()},
                    lists:sort(SortingFun, Enabled)),
            EnhAns = ["f" | Ans],
            EnhAnsDict = dict:store(f, none,AnsDict),
            QuestionLines = 
                    ["The following transitions are enabled:" | lists:reverse(Lines)]
                ++  ["What is the next transition to be fired?" 
                     | ["[" ++ string:join(lists:reverse(EnhAns), "/") ++ "]: "]],
            Answer = 
                pn_lib:get_answer(
                    string:join(QuestionLines,"\n"), 
                    [f | lists:seq(1, length(Ans))]),
            dict:fetch(Answer, EnhAnsDict)
    end.

% random_fired_transition(PN, Enabled) ->
%     case Enabled of 
%         [] ->
%             none;
%         [{T, SN}] -> 
%             T;
%         _ ->
%             {T, SN} = lists:nth(uniform(length(Enabled)), Enabled),
%             T
%     end.

set_enabled_transitions(
    PN = #petri_net{
        places = Ps, 
        transitions = Ts,
        digraph = G}) ->
    NTs = 
        dict:map(
            fun(K, V) ->
                InputPlaces = digraph:in_neighbours(G, K),
                Enabled = 
                    case InputPlaces of 
                        [] ->
                            true;
                        _ -> 
                            lists:all(
                                fun(X) -> X end,
                                [(dict:fetch(P, Ps))#place.marking > 0
                                 || P <- InputPlaces])
                    end,
                V#transition{enabled = Enabled}
            end,
            Ts),
    PN#petri_net{transitions = NTs}.


fire_transition(
    Transition, 
    PN = #petri_net{
        places = Ps, 
        digraph = G}) ->
    PlacesToInc = 
        digraph:out_neighbours(G, Transition),
    PlacesToDec = 
        digraph:in_neighbours(G, Transition),
    NPs = 
        dict:map(
            fun(K, V) ->
                case lists:member(K, PlacesToInc) of 
                    true ->
                        V#place{
                            marking = 
                                V#place.marking + 1
                        };
                    false ->
                        V
                end
            end,
            Ps),
    NPs2 = 
        dict:map(
            fun(K, V) ->
                case lists:member(K, PlacesToDec) of 
                    true ->
                        V#place{
                            marking = 
                                V#place.marking - 1
                        };
                    false ->
                        V
                end
            end,
            NPs),
    PN#petri_net{places = NPs2}.

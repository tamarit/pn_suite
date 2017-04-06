-module(pn_tests).

-compile(export_all).

-include("pn.hrl").

-include_lib("eunit/include/eunit.hrl").

files() -> 
    [
            "examples/other/bif_backwards.xml"
        ,   
            "examples/other/bif_forwards.xml"
        ,   
            "examples/other/improve1.xml"
        ,   
            "examples/other/loop.xml"
        ,   
            "examples/other/pn_CSP2PN.xml"
        ,   
            "examples/other/pn_example.xml"
        ,   
            "examples/other/pn_paper.xml"
        ,   
            "examples/other/pn_rakowp6.xml"
        ,   
            "examples/other/pn_rakowp5.xml"
        ,   
            "examples/other/pn_unbounded.xml"
        ,   
            "examples/other/pn_yuetal1.xml"
        ,   
            "examples/other/pn_yuetal2.xml"
        ,   
            "examples/other/problem1.xml"
    ].

llorens_ori_vs_gen_test_() ->
    Res = 
        [slice_file(
            fun pn_slice:slice/2, 
            fun pn_slice:slice_gen/2, 
            File) 
        || File <- files()],
    [?_assertEqual({SC, File, Old}, {SC, File, New}) 
     || {SC, File, Old, New} <- Res].

yu_sim_vs_ori_test_() ->
    Res = 
        [slice_file(
            fun pn_yuetal:slice_no_sim/2, 
            fun pn_yuetal:slice/2, 
            File) 
        || File <- files()],
    [?_assertEqual({SC, File, Old}, {SC, File, New}) 
     || {SC, File, Old, New} <- Res].

% Yu is not clear waht is doing, so we ignore it from testing
% yu_sim_vs_llorens_prec_single_test_() ->
%     Res = 
%         [slice_file(
%             fun pn_yuetal:slice/2, 
%             fun pn_slice:slice_imp_single/2, 
%             File) 
%         || File <- files()],
%     [?_assertEqual({SC, File, Old}, {SC, File, New}) 
%      || {SC, File, Old, New} <- Res].

llorens_prec_single_ori_vs_gen_test_() ->
    Res = 
        [slice_file(
            fun pn_slice:slice_imp_single/2, 
            fun pn_slice:slice_imp_single_gen/2, 
            File) 
        || File <- files()],
    [?_assertEqual({SC, File, Old}, {SC, File, New}) 
     || {SC, File, Old, New} <- Res].

slice_file(OldFun, NewFun, File) -> 
    MaxSC0 = 
        5,
    PN = 
        pn_input:read_pn(File),
    pn_lib:build_digraph(PN),
    Ps = 
        dict:fetch_keys(PN#petri_net.places),
    MaxSC = 
        case MaxSC0 > length(Ps) of 
            true ->
                length(Ps);
            false ->
                MaxSC0
        end,
    [SC] = 
        pn_bench:build_scs(Ps, 1, MaxSC),
    PPSC= 
        lists:concat(lists:join(",", SC)),
    print_to_file("File: ~s\nSC: ~p\n", [File, PPSC]),
    Old = 
        OldFun(PN ,SC),
    New = 
        NewFun(PN ,SC),
    OPs = 
        lists:sort(dict:fetch_keys(Old#petri_net.places)),
    OTs = 
        lists:sort(dict:fetch_keys(Old#petri_net.transitions)),
    NPs = 
        lists:sort(dict:fetch_keys(New#petri_net.places)),
    NTs = 
        lists:sort(dict:fetch_keys(New#petri_net.transitions)),
    {PPSC, File, {OPs, OTs}, {NPs, NTs}}.

print_to_file(Format, Args) -> 
    file:write_file(
        "output.txt", 
        list_to_binary(
            pn_lib:format(Format, Args))). 
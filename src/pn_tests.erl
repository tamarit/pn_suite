-module(pn_tests).

-compile(export_all).

-include("pn.hrl").

-include_lib("eunit/include/eunit.hrl").

gen_slice_test_() ->
    Files = 
        [
                "examples/other/pn_rakowp6.xml"
            ,   "examples/other/pn_rakowp5.xml"
            ,   "examples/other/pn_paper.xml"
            ,   "examples/other/pn_unbounded.xml"
            ,   "examples/other/pn_yuetal1.xml"
            ,   "examples/other/pn_yuetal2.xml"
        ],
    Res = 
        [test_file(File) || File <- Files],
    [?_assertEqual({SC, Old}, {SC, New}) || {SC, Old, New} <- Res].

test_file(File) -> 
    MaxSC0 = 5,
    PN = 
        pn_input:read_pn(File),
    pn_lib:build_digraph(PN),
    Ps = dict:fetch_keys(PN#petri_net.places),
    MaxSC = 
        case MaxSC0 > length(Ps) of 
            true ->
                length(Ps);
            false ->
                MaxSC0
        end,
    [SC] = 
        pn_bench:build_scs(Ps, 1, MaxSC),
    Old = 
        pn_slice:slice(PN ,SC),
    New = 
        pn_slice:slice_gen(PN ,SC),
    OPs = 
        lists:sort(dict:fetch_keys(Old#petri_net.places)),
    OTs = 
        lists:sort(dict:fetch_keys(Old#petri_net.transitions)),
    NPs = 
        lists:sort(dict:fetch_keys(New#petri_net.places)),
    NTs = 
        lists:sort(dict:fetch_keys(New#petri_net.transitions)),
    {SC, {OPs, OTs}, {NPs, NTs}}.
-define(UNNAMED, "UNNAMED").

-record(place, 
    {
        name = ?UNNAMED, 
        showed_name = ?UNNAMED,
        marking = 0,
        position = {"0", "0"}
    }).

-record(transition, 
    {
        name = ?UNNAMED, 
        showed_name = ?UNNAMED,
        enabled = false,
        position = {"0", "0"}
    }).

-record(arc, 
    {
        name = ?UNNAMED, 
        source,
        target
    }).

-record(petri_net, 
    {
        name = ?UNNAMED,
        places = dict:new(), 
        transitions = dict:new(),
        arcs = [],
        digraph = digraph:new(),
        dir = "."
    }).

% -record(sdg_node,
%     {
%         components = [],
%         marked = false
%     }).

-record(sdg,
    {
       digraph = digraph:new(),
       structural_nodes = []
    }).
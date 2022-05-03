# import snakes.nets
# import snakes.nets
# from xml.etree import ElementTree as ET
import math
import xmltodict
# import dot_parser
import networkx as nx 
import re
import subprocess
import snakes.plugins
snakes.plugins.load('gv', 'snakes.nets', 'nets')
from nets import *
# snakes.plugins.load('gv', 'snakes.nets', 'nets')

import petri_net_parser

# FILENAME = "../examples/other/khan_thesis_4_17.pnml"
# FILENAME = "../examples/other/reviewer_fig2.xml"
FILENAME = "examples/temp_slice_pn.xml"
FILENAME_SC = "examples/temp_slice_pn_sc.txt"
# FILENAME = "examples/other/example3_1_fundamental.xml"
content_sc = open(FILENAME_SC, 'r').read()
SLICING_CRITERION = content_sc.split(",")
# print(f"SLICING_CRITERION: {SLICING_CRITERION}")
# SLICING_CRITERION = ["P3"]
# # FILENAME = "sample.pnml"

content = open(FILENAME, 'r').read()
# pn = snakes.pnml.Tree.from_pnml(content)
# # pn = snakes.pnml.loads(content)
# print(str(pn))
# print(repr(pn))
# print(pn.to_obj())
# # pn = pn.to_obj()
# # g = snakes.nets.StateGraph(pn)
# # print(g.net)
# # print("checked %s states" % len(g))
# # g.draw("prova-states.png")

result = xmltodict.parse(content)
# print(result)
n = PetriNet('N')
g_pn = nx.DiGraph()
places = []
transitions = []
arcs = []
for place in result["pnml"]["net"]["place"]:
    # if type(place) == str:
    #     if place == "@id":
    #         result["pnml"]["net"]["place"]["@id"]
    # else:
    try:
        name = place["@id"]
    except KeyError:
        name = place["name"]["value"]
    except TypeError:
        name = result["pnml"]["net"]["place"]["@id"]
    try:
        marking = int(place["initialMarking"]["value"].split(",")[1])
    except IndexError:
        marking = int(place["initialMarking"]["value"])
    except TypeError:
        # print(result["pnml"]["net"]["place"])
        marking = int(result["pnml"]["net"]["place"]["initialMarking"]["value"])
        # print(marking)
    if marking == 0:
        marking = []
    try:
        n.add_place(Place(name, marking))
        # print(f"Adding ({name}, {marking})")
        places.append((name, marking if marking != [] else 0))
    except snakes.ConstraintError:
        pass
# print(result["pnml"]["net"]["transition"])
if "transition" in  result["pnml"]["net"]:
    for transition in result["pnml"]["net"]["transition"]:
        # if type(transition) == str:
        #     if transition == "@id":
        #         name = result["pnml"]["net"]["transition"][transition]
        # else:
        try:
            name = transition["@id"]
        except KeyError:
            name = transition["name"]["value"]
        except TypeError:
            name = result["pnml"]["net"]["transition"]["@id"]
        # print(f"Adding {name}")
        try:
            n.add_transition(Transition(name))
            transitions.append(name)
        except snakes.ConstraintError:
            pass
    for arc in result["pnml"]["net"]["arc"]:
        source = arc["@source"]
        target = arc["@target"]
        # print(source)
        # print(n.has_place(source))
        if n.has_place(source):
            n.add_input(source, target, Value(1))
        else:
            n.add_output(target, source, Value(1))
        g_pn.add_edge(source, target)
        arcs.append((source, target))

# FILE_GRAPH = 'examples/prova_before_filter.dot'
# FILE_GRAPH_PNG = 'examples/prova_before_filter.png'

# nx.drawing.nx_pydot.write_dot(g_pn, FILE_GRAPH)
# subprocess.call(f'dot -Tpng {FILE_GRAPH} -o {FILE_GRAPH_PNG}', shell=True)

# n.add_transition(Transition("T4"))
# n.add_output("P0", "T4", Value(1))
# g_pn.add_edge("T4", "P0")

# Filtering backward slice
removed = True
while(removed):
    removed = False
    places_to_remove = []
    transitions_to_remove = set()
    for p in n.place():
        if p.tokens == []:
            found_previous_trans = False
            # print(p)
            # print(type(p))
            for t in n.transition():
                # print(t)
                # print(type(t))
                # print(t.output())
                if p in map(lambda tup:tup[0], t.output()):    
                    # print("FOUND")               
                    found_previous_trans = True
                    break 
            if not found_previous_trans:
                places_to_remove.append(p)
    for p in places_to_remove:
        for t in n.transition():
            if p in map(lambda tup:tup[0], t.input()):
                transitions_to_remove.add(t)
                break
    if places_to_remove == [] and transitions_to_remove == set():
        break
    else:
        pass
    # print(f"places_to_remove: {places_to_remove}")
    # print(f"transitions_to_remove: {transitions_to_remove}")
    for p in places_to_remove:
        for t in transitions_to_remove:
            try:
                g_pn.remove_edge(p.name, t.name)
            except nx.exception.NetworkXError:
                pass
            try:
                g_pn.remove_node(p.name)
            except nx.exception.NetworkXError:
                pass
            try:
                g_pn.remove_node(t.name)
            except nx.exception.NetworkXError:
                pass
            try:
                arcs.remove((p.name, t.name))
                # n.remove_input(p.name, t.name, Value(1))
            except ValueError:
                pass
            try:
                n.remove_input(p.name, t.name)
            except ValueError:
                pass
            except snakes.ConstraintError:
                pass
            except snakes.NodeError:
                pass
            try: 
                transitions.remove(t.name)
            except ValueError:
                pass
            try:
                n.remove_transition(t.name)
            except ValueError:
                pass
            except snakes.ConstraintError:
                pass
            except snakes.NodeError:
                pass
        try:
            places.remove(p.name)
        except ValueError:
            pass 
        try:
            n.remove_place(p.name)
        except ValueError:
            pass    
    removed = True   


# FILE_GRAPH = 'examples/prova_after_filter.dot'
# FILE_GRAPH_PNG = 'examples/prova_after_filter.png'

# nx.drawing.nx_pydot.write_dot(g_pn, FILE_GRAPH)
# subprocess.call(f'dot -Tpng {FILE_GRAPH} -o {FILE_GRAPH_PNG}', shell=True)

petri_net_parser.build_coverabilty_tree(places, transitions, arcs, g_pn, slicing_criterion=SLICING_CRITERION)

# # petri_net_parser.build_coverabilty_tree(places, transitions + ["T4"], arcs + [("T4", "P0")], g_pn, slicing_criterion=SLICING_CRITERION)

# FILE_GRAPH = 'examples/prova_g.dot'
# FILE_GRAPH_PNG = 'examples/prova_g.png'

# nx.drawing.nx_pydot.write_dot(g_pn, FILE_GRAPH)
# subprocess.call(f'dot -Tpng {FILE_GRAPH} -o {FILE_GRAPH_PNG}', shell=True)
# # nx.draw(g_pn)
# # root = xml.etree.ElementTree.parse(FILENAME).getroot()

# # print(e)
# # for atype in root.findall('.//place'):
# #     print(atype)

# # for place_data in root.findall('.//place'):
# #     for name_data in place_data.findall(".//name"):
# #         # value = type_tag.get('id')
# #         print(name_data.value)

# # n = PetriNet('N')
# # n.add_place(Place('p0', 1))
# # n.add_place(Place('p1', []))
# # n.add_place(Place('p2', 1))
# # n.add_transition(Transition('t0'))
# # n.add_input('p0', 't0', Value(1))
# # n.add_output('p1', 't0', Value(1))
# # n.add_transition(Transition('t1'))
# # n.add_input('p1', 't1', Value(1))
# # n.add_output('p2', 't1', Value(1))
# # n.add_transition(Transition('t2'))
# # n.add_input('p2', 't2', Value(1))
# # n.add_output('p0', 't2', Value(1))
# # n.add_transition(Transition('t_ant'))
# # n.add_output('p0', 't_ant', Value(1))
# # n.add_output('p1', 't_ant', Value(1))
# # n.add_output('p2', 't_ant', Value(1))

# # n.add_place(Place('p0', 1))
# # n.add_place(Place('p1', []))
# # n.add_transition(Transition('t0'))
# # n.add_output('p0', 't0', Value(1))
# # n.add_output('p1', 't0', Value(1))

# # for engine in ('dot') :
# engine = "dot"
# n.draw('examples/prova.png', engine=engine)

# g = StateGraph(n) 
# g.build()

# g_fst = nx.DiGraph()

# places_dicts = dict()

# for s in g :
#     # print(s)
#     m = g.net.get_marking()
#     # print(m)
#     places_dict = dict()
#     for place in m:
#         # print(str(place) + " " + str(len(m[place].items())))
#         places_dict[place] = len(m[place].items())
#         # print(type(m[place]))
#         # print(items)
#     # g.net.rename(str(g.net.name) + "_" + str(s))
#     # print(places_dict)
#     places_dicts[s] = str(places_dict)
#     # min_path = float("inf")
#     # for scp in SLICING_CRITERION:
#     #     for place in m:
#     #         sp = nx.shortest_path(g_pn, source=place, target=scp)
#     #         print(sp)
#     #         len_sp = len(sp) - 1
#     #         if len_sp < min_path:
#     #             min_path = len_sp
#     # print(min_path)
#     # print(list(g.successors()))
#     for suc in list(g.successors()):
#         target = suc[0]
#         target_transition = suc[1].name
#         # print(f"{target} {target_transition}")
#         min_path = float("inf")
#         for scp in SLICING_CRITERION:
#             try:
#                 sp = nx.shortest_path(g_pn, source=target_transition, target=scp)
#             except nx.exception.NodeNotFound:
#                 sp = []
#             except nx.exception.NetworkXNoPath:
#                 sp = [None] * 1000
#             # print(sp)
#             len_sp = math.floor((len(sp) - 1) / 2)
#             # len_sp = len(sp) - 1
#             if len_sp < min_path:
#                 min_path = len_sp
#         # print(min_path)
#         if s in g_fst.nodes():
#             path_to_root = len(nx.shortest_path(g_fst, source=0, target=s)) - 1
#         else:
#             path_to_root = 0
#         g_fst.add_edge(s, target, label=(target_transition, min_path, path_to_root, path_to_root+min_path))

# nx.set_node_attributes(g_fst, places_dicts, name="label")

# # print(g.net)

# FILE_GRAPH = 'examples/prova_fst.dot'
# FILE_GRAPH_PNG = 'examples/prova_fst.png'

# nx.drawing.nx_pydot.write_dot(g_fst, FILE_GRAPH)
# subprocess.call(f'dot -Tpng {FILE_GRAPH} -o {FILE_GRAPH_PNG}', shell=True)

# # nx.draw_networkx_edge_labels(
# #     g_fst, pos,
# #     edge_labels={('A', 'B'): 'AB', 
# #                  ('B', 'C'): 'BC', 
# #                  ('B', 'D'): 'BD'},
# #     font_color='red'
# # )

# OUTPUT = "examples/prova-states.dot"
# OUTPUT_PNG = "examples/prova-states.png"

# g.draw(OUTPUT)

# content = open(OUTPUT, 'r').read()

# new_content = re.sub(r"^(.*?node.*?,)\s*label=.*?,(.*?)$", r"\1\2", content, flags=re.DOTALL)
# new_content = re.sub(r"\\n{}", r"", new_content, flags=re.DOTALL)

# # print(new_content)

# open(OUTPUT, 'w').write(new_content)

# subprocess.call('dot -Tpng ' + OUTPUT + ' -o ' + OUTPUT_PNG, shell=True)


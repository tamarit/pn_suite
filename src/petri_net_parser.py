# PetriNetParser

# you should have pip installed by default with Python, if not, use google
# then, make sure you have the 'numpy', 'networkx' and 'pyvis' packages installed as they are required
# cmd: pip install numpy; pip install networkx; pip install pyvis


# default packages
import xml.etree.ElementTree as et
import sys
import subprocess
import math
import json
from networkx.readwrite import json_graph
# import os.path


# custom packages
try:
    import numpy as np
    import networkx as nx
    # from pyvis import network as pvnet
except ModuleNotFoundError as err:
    print("One or more packages could not be loaded.")
    print("List of all required packages: numpy, networkx, pyvis")
    input("Press ENTER to exit...")
    sys.exit(-1)


# global stuff
CONST_OMEGA_CHAR = "w"
# NODECOLOR_FIRST = "#7FFF8C"
# NODECOLOR_GENERIC = "#8CCFFF"
# NODECOLOR_LAST = "#FFC97F"


# # functions
# def getPyvisOptions():
#     opts = '''
#         var options = {
#           "physics": {
#             "enabled": false
#           },
#           "interaction": {
#             "dragNodes": true,
#             "hideEdgesOnDrag": false,
#             "hideNodesOnDrag": false
#         },
#         "edges": {
#             "smooth": {
#                 "enabled": true,
#                 "type": "continuous"
#             }
#         }
#         }
#     '''
#     return opts


# def calcGraphResolution():
#     # screen resolution (for html graph size)
#     scrw_in = input("Your screen width (leave empty for default 1920): ")
#     scrh_in = input("Your screen height (leave empty for default 1080): ")
#     SCR_W = 1920 if scrw_in == "" else int(scrw_in)
#     SCR_H = 1080 if scrh_in == "" else int(scrh_in)
#     print(f"Screen resolution: {SCR_W}x{SCR_H}\n")
#     return SCR_W * 0.9875, SCR_H * 0.82


# classes
class Node:
    def __init__(self, id, state, predcessorNode):
        self.id = id
        self.state = state
        self.predcessorNode = predcessorNode
        self.predcessors = self.setupAllPredcessorNodes()
        self.isChecked = False
        self.designationChar = "m"
        self.distance_to_root = predcessorNode.distance_to_root + 1 if predcessorNode is not None else 0

    def getPredcessorNode(self):
        return self.predcessorNode

    def setupAllPredcessorNodes(self):
        if self.getPredcessorNode() is None:
            return []
        else:
            allNodes = list(self.getPredcessorNode().getAllPredcessorNodes())
            if self.getPredcessorNode() not in allNodes:
                allNodes.append(self.getPredcessorNode())
            return sorted(allNodes, key=lambda item: item.id, reverse=True)

    def getAllPredcessorNodes(self):
        return self.predcessors

    def getName(self):
        return f"{self.designationChar}{self.id}"

    def getAllPredcessorNames(self):
        allNames = []
        for node in self.getAllPredcessorNodes():
            allNames.insert(0, node.getName())
        return allNames

    def mergePredcessorNodesFrom(self, predNode):
        own = list(self.getAllPredcessorNodes())
        for node in predNode.getAllPredcessorNodes():
            if node not in own:
                own.append(node)
        if predNode not in own:
            own.append(predNode)
        self.predcessors = sorted(own, key=lambda item: item.id, reverse=True)

    def getGraphLabel(self):
        return f"{self.getName()}\n{self.state}\n({self.getAllPredcessorNames()})"

    def getGraphLabelCustom(self, customstate, custompredcessors):
        return f"{self.getName()}\n{customstate}\n({custompredcessors})"

    def __str__(self):
        return f"{self.getName()} | {self.state} | ({self.getAllPredcessorNames()})"


class Graph:
    def __init__(self):
        self.nodeCount = 0
        self.nodes = []

    def addNode(self, state, predcessorNode=None):
        node = Node(self.nodeCount, state, predcessorNode)
        self.nodes.append(node)
        self.nodeCount += 1
        return node

    def hasNodeWithState(self, state):
        for node in self.nodes:
            if node.state == state:
                return True
        return False

    def getNodeWithState(self, state):
        for node in self.nodes:
            if node.state == state:
                return node
        return None

    def hasNodeWithName(self, name):
        for node in self.nodes:
            if node.getName() == name:
                return True
        return False

    def getNodeWithName(self, name):
        for node in self.nodes:
            if node.getName() == name:
                return node
        return None

    def buildNodeLabelDict(self):
        data = {}
        for node in self.nodes:
            data[node.getName()] = node.getGraphLabel()
        return data


class Place:
    def __init__(self, id, label, tokens=0, static=False):
        self.id = id
        self.label = label
        self.tokens = tokens
        self.static = static

    def getId(self):
        return self.id

    def getLabel(self):
        return self.label

    def getTokens(self):
        return self.tokens

    def isStatic(self):
        return self.static


class Transition:
    def __init__(self, id, label):
        self.id = id
        self.label = label

    def getId(self):
        return self.id

    def getLabel(self):
        return self.label


class Arc:
    def __init__(self, id, sourceId, destinationId, multiplicity=1):
        self.id = id
        self.sourceId = sourceId
        self.destinationId = destinationId
        self.multiplicity = multiplicity
        self.source = None
        self.destination = None

    def getId(self):
        return self.id

    def getSourceId(self):
        return self.sourceId

    def getDestinationId(self):
        return self.destinationId

    def getMultiplicity(self):
        return self.multiplicity


class Net:
    def __init__(self):
        self.places = []
        self.transitions = []
        self.arcs = []
        self.inputMatrix = None
        self.outputMatrix = None
        self.incidenceMatrix = None

    def addPlace(self, place):
        self.places.append(place)

    def addTransition(self, transition):
        self.transitions.append(transition)

    def setPlaces(self, places):
        self.places = places

    def setTransitions(self, transitions):
        self.transitions = transitions

    def addArc(self, arc):
        self.arcs.append(arc)

    def getPlaces(self):
        return self.places

    def getTransitions(self):
        return self.transitions

    def getArcs(self):
        return self.arcs

    def getPlaceById(self, id):
        for obj in self.getPlaces():
            if obj.getId() == id:
                return obj
        return None

    def getTransitionById(self, id):
        for obj in self.getTransitions():
            if obj.getId() == id:
                return obj
        return None

    def getPlaceByLabel(self, label):
        for obj in self.getPlaces():
            if obj.getLabel() == label:
                return obj
        return None

    def getTransitionByLabel(self, label):
        for obj in self.getTransitions():
            if obj.getLabel() == label:
                return obj
        return None

    def sortObjectsByLabel(self, arr):
        n = len(arr)
        for i in range(n - 1):
            for j in range(0, n - i - 1):
                if arr[j].getLabel() > arr[j + 1].getLabel():
                    arr[j], arr[j + 1] = arr[j + 1], arr[j]

    # def sortObjectsByLabel(self, arr):
    #     arr = sorted(arr, key=lambda item: item.getLabel(), reverse=True)

    def sortPlacesByPattern(self, pattern):
        newObjects = []
        places = list(self.getPlaces()) # deep copy
        for label in pattern.split(" "):
            try:
                newObjects.append(places.pop(places.index(self.getPlaceByLabel(label))))
            except ValueError:
                print("Invalid place label specified, not sorting!")
                return
        if len(self.getPlaces()) != len(newObjects):
            print("New place count does not match the old one, not sorting!")
        else:
            self.setPlaces(newObjects)

    def sortTransitionsByPattern(self, pattern):
        newObjects = []
        transitions = list(self.getTransitions()) # deep copy
        for label in pattern.split(" "):
            try:
                newObjects.append(transitions.pop(transitions.index(self.getTransitionByLabel(label))))
            except ValueError:
                print("Invalid transition label specified, not sorting!")
                return
        if len(self.getTransitions()) != len(newObjects):
            print("New transition count does not match the old one, not sorting!")
        else:
            self.setTransitions(newObjects)

    def sortPlaces(self):
        self.sortObjectsByLabel(self.places)

    def sortTransitions(self):
        self.sortObjectsByLabel(self.transitions)

    def printOrderedPlacesTransitions(self):
        print("   ", end="")
        for transition in self.getTransitions():
            print(transition.getLabel(), end=" ")
        print()
        for place in self.getPlaces():
            print(place.getLabel())

    def printCurrentPlaceOrder(self):
        print("Current place order: ", end="")
        for place in self.getPlaces():
            print(place.getLabel(), end=" ")
        print()

    def printCurrentTransitionOrder(self):
        print("Current transition order: ", end="")
        for transition in self.getTransitions():
            print(transition.getLabel(), end=" ")
        print()

    def getGraphState(self):
        graphState = []
        for p in self.getPlaces():
            graphState.append(p.getTokens())
        return graphState

    def isTransitionRunnableFromState(self, transition, graphState):
        transitionIndex = self.getTransitions().index(transition)
        inputColumn = self.inputMatrix[:, transitionIndex]
        curGraphState = graphState

        isRunnable = True
        for i in range(len(curGraphState)):
            available = int(curGraphState[i])
            required = int(inputColumn[i])
            if available < required:
                isRunnable = False
                break
        return isRunnable

    def isTransitionRunnableFromState_Omega(self, transition, graphState):
        transitionIndex = self.getTransitions().index(transition)
        inputColumn = self.inputMatrix[:, transitionIndex]
        curGraphState = graphState

        isRunnable = True
        for i in range(len(curGraphState)):
            available_char = curGraphState[i]
            required = int(inputColumn[i])

            # only check runnability if it isnt omega, if its omega it counts as always runnable
            if available_char != CONST_OMEGA_CHAR:
                available = int(curGraphState[i])
                if available < required:
                    isRunnable = False
                    break
        return isRunnable

    def isState2GreaterThan1(self, state1, state2):
        # returns True if state2 is greater, otherwise returns False
        if state1 == state2:
            return False
        for i in range(len(state1)):
            if state1[i] > state2[i]:
                return False
        return True

    def isState2GreaterThan1_Omega(self, state1, state2):
        # returns True if state2 is greater, otherwise returns False
        if state1 == state2:
            return False
        for i in range(len(state1)):
            s1 = state1[i]
            s2 = state2[i]
            # if they are not omegas, do regular check
            # if s1 is omega and s2 is not, s1 has to be greater
            # otherwise s2 has to be greater
            if s1 != CONST_OMEGA_CHAR and s2 != CONST_OMEGA_CHAR and s1 > s2:
                return False
            if s1 == CONST_OMEGA_CHAR and s2 != CONST_OMEGA_CHAR:
                return False
        return True

    def transformState2ToOmega(self, state1, state2):
        for i in range(len(state1)):
            s1 = state1[i]
            s2 = state2[i]

            if s1 != CONST_OMEGA_CHAR and s2 != CONST_OMEGA_CHAR and s2 > s1:
                state2[i] = CONST_OMEGA_CHAR
        return state2

    def hasTokensOnPlaces(self, state, places_list):
        for i in range(len(state)):
            s = state[i]
            if i in places_list:
                if s == CONST_OMEGA_CHAR or s > 0:
                    return True
        return False

    def getTokensOnPlaces(self, state, places_list):
        tokens_on_places = []
        for i in range(len(state)):
            s = state[i]
            if i in places_list:
                if s == CONST_OMEGA_CHAR or s > 0:
                    tokens_on_places.append(i)
        return tokens_on_places
        

    def runTransition(self, transition, graphState):
        transitionIndex = self.getTransitions().index(transition)
        incidenceColumn = self.incidenceMatrix[:, transitionIndex]

        newGraphState = list(graphState)
        for place in self.getPlaces():
            placeIndex = self.getPlaces().index(place)
            newGraphState[placeIndex] += int(incidenceColumn[placeIndex])
        return newGraphState

    def runTransition_Omega(self, transition, graphState):
        transitionIndex = self.getTransitions().index(transition)
        incidenceColumn = self.incidenceMatrix[:, transitionIndex]

        newGraphState = list(graphState)
        for place in self.getPlaces():
            placeIndex = self.getPlaces().index(place)
            # only change tokens if we arent omega
            if newGraphState[placeIndex] != CONST_OMEGA_CHAR:
                newGraphState[placeIndex] += int(incidenceColumn[placeIndex])
        return newGraphState

    def getWorkflowStateFromState(self, state):
        newState = []
        for place in self.getPlaces():
            placeIndex = self.getPlaces().index(place)
            if not place.isStatic():
                tokens = state[placeIndex]
                finalTokens = "" if tokens == 1 else str(tokens)
                text = f"{finalTokens}{place.getLabel()}"
                if tokens > 0:
                    newState.append(text)
        return " + ".join(newState)

    def createEdgeDictFromGraph(self, graph):
        edgeDict = dict()
        for edge in graph.edges(data=True):
            edgeLabel = edge[2]["label"]
            entryName = edge[0] + " " + edge[1]
            if entryName not in edgeDict.keys():
                edgeDict[entryName] = edgeLabel
            else:
                edgeDict[entryName] = edgeDict[entryName] + ", " + edgeLabel
        return edgeDict

    def updateMultiEdgesForPyvisgraph(self, graph):
        edgeDict = dict()
        for edge in graph.get_edges():
            edgeLabel = edge["label"]
            entryName = edge["from"] + " " + edge["to"]
            if entryName not in edgeDict.keys():
                edgeDict[entryName] = edgeLabel
            else:
                edgeDict[entryName] = edgeDict[entryName] + ", " + edgeLabel
        # delete old edges, add new ones
        graph.edges = []
        for key, value in edgeDict.items():
            nodeData = key.split(" ")
            nodeFrom = nodeData[0]
            nodeTo = nodeData[1]
            edgeLabel = value
            graph.add_edge(nodeFrom, nodeTo, label=edgeLabel, color="black", title=edgeLabel)

    def printTransitionPreset(self, transition):
        transitionIndex = self.getTransitions().index(transition)
        inputColumn = self.inputMatrix[:, transitionIndex]

        placeList = []
        for place in self.getPlaces():
            placeIndex = self.getPlaces().index(place)
            if inputColumn[placeIndex] > 0:
                placeList.append(place.getLabel())
        print("*", transition.getLabel(), end=" = {", sep="")
        print(", ".join(placeList), end="}\n")

    def printTransitionPostset(self, transition):
        transitionIndex = self.getTransitions().index(transition)
        inputColumn = self.outputMatrix[:, transitionIndex]

        placeList = []
        for place in self.getPlaces():
            placeIndex = self.getPlaces().index(place)
            if inputColumn[placeIndex] > 0:
                placeList.append(place.getLabel())
        print(transition.getLabel(), "*", end=" = {", sep="")
        print(", ".join(placeList), end="}\n")

    def printAllTransitionsPresets(self):
        print("\nTransitions presets:")
        for t in self.getTransitions():
            self.printTransitionPreset(t)

    def printAllTransitionsPostsets(self):
        print("\nTransitions postsets:")
        for t in self.getTransitions():
            self.printTransitionPostset(t)

    def getLeafNodesFromGraph(self, graph):
        nodesWithoutSuccessors = []
        for node in graph.nodes():
            successorCount = 0
            for _ in graph.successors(node):
                successorCount += 1
            if successorCount == 0:
                nodesWithoutSuccessors.append(node)
        return nodesWithoutSuccessors

    def getGraphStatesOccurenceCount(self, graph, petrigraph):
        nodeStates = dict()
        for node in graph.nodes():
            nodeData = petrigraph.getNodeWithName(node)
            nodeState = str(nodeData.state)
            if nodeState not in nodeStates:
                nodeStates[nodeState] = 1
            else:
                nodeStates[nodeState] += 1
        return nodeStates


# # program

# file = ""

# tArgs = sys.argv
# if len(tArgs) == 2:
#     file = tArgs[1]
# else:
#     input("Error: No input file provided. Press ENTER to exit...")
#     sys.exit(-1)

# if os.path.exists(file) and os.path.isfile(file):
#     print("Parsing file:", file)
#     print()
# else:
#     input("Error: Can't load file. Press ENTER to exit...")
#     sys.exit(-1)

# PYVISGRAPH_W, PYVISGRAPH_H = calcGraphResolution()

# tree = et.parse(file)
# root = tree.getroot()

# # check if file is xml or pflow (pflow has data encapsulated in extra subnet block, otherwise it's identical to xml)
# isPflowFormat = root.find("subnet") is not None

# prefix = "subnet/" if isPflowFormat else ""

# filling the net with data


# for type_tag in root.findall(prefix + "place"):
#     id = type_tag.find('id').text
#     label = type_tag.find('label').text
#     tokens = int(type_tag.find('tokens').text)
#     static = True if type_tag.find('static').text == "true" else False
#     petri_net.addPlace(Place(id, label, tokens, static))

# for type_tag in root.findall(prefix + "transition"):
#     id = type_tag.find('id').text
#     label = type_tag.find('label').text
#     petri_net.addTransition(Transition(id, label))

# for type_tag in root.findall(prefix + "arc"):
#     id = type_tag.find('id').text
#     sourceId = type_tag.find('sourceId').text
#     destinationId = type_tag.find('destinationId').text
#     multiplicity = type_tag.find('multiplicity').text
#     petri_net.addArc(Arc(id, sourceId, destinationId, multiplicity))

def build_coverabilty_tree(places, transitions, arcs, g_pn, slicing_criterion=[]):
    petri_net = Net()
    min_dist_to_sc = dict()
    for p, t in places:
        petri_net.addPlace(Place(p, p, t, False))
    for t in transitions:
        petri_net.addTransition(Transition(t, t))
        min_path = float("inf")
        for scp in slicing_criterion:
            # if True:
            # try:
            try:
                sp = nx.shortest_path(g_pn, source=t, target=scp)
            except nx.exception.NetworkXNoPath:
                sp = [None] * 1000
            except nx.exception.NodeNotFound:
                sp = [None] * 1000
            # except nx.exception.NodeNotFound:
            #     sp = []
            len_sp = math.floor((len(sp) - 1) / 2)
            # len_sp = len(sp) - 1
            if len_sp < min_path:
                min_path = len_sp
        min_dist_to_sc[t] = min_path
    for arc_id, (s, t) in enumerate(arcs):
        petri_net.addArc(Arc(arc_id, s, t, 1))


    # sort places and transitions for proper matrix format
    # petri_net.printCurrentPlaceOrder()
    # sortPatternPlaces = input("New place sort order (leave empty to sort alphabetically): ")
    # if sortPatternPlaces == "":
    if True:
        # print("Sorting places alphabetically")
        petri_net.sortPlaces()
    else:
        petri_net.sortPlacesByPattern(sortPatternPlaces)

    slicing_criterion_indices = []
    for p_ind, p in enumerate(petri_net.places):
        if p.getLabel() in slicing_criterion:
            slicing_criterion_indices.append(p_ind)

    # petri_net.printCurrentPlaceOrder()

    # print()
    # petri_net.printCurrentTransitionOrder()
    # sortPatternTransitions = input("New transition sort order (leave empty to sort alphabetically): ")
    # if sortPatternTransitions == "":
    if True:
        # print("Sorting transitions alphabetically")
        petri_net.sortTransitions()
    else:
        petri_net.sortTransitionsByPattern(sortPatternTransitions)
    # petri_net.printCurrentTransitionOrder()

    # order recap
    # print("\nCurrent places/transitions order")
    # petri_net.printCurrentPlaceOrder()
    # petri_net.printCurrentTransitionOrder()

    #  setup matrices
    nRows = len(petri_net.getPlaces())
    nColumns = len(petri_net.getTransitions())

    inputMatrix = np.array([[0 for i in range(nColumns)] for j in range(nRows)])
    # inputMatrix = [[0 for i in range(nColumns)] for j in range(nRows)]
    outputMatrix = np.array([[0 for i in range(nColumns)] for j in range(nRows)])
    # outputMatrix = [[0 for i in range(nColumns)] for j in range(nRows)]

    # fill each matrix with the proper data
    for arc in petri_net.getArcs():
        sourceId = arc.getSourceId()
        destinationId = arc.getDestinationId()

        source = None
        destination = None

        if (petri_net.getPlaceById(sourceId) is not None) and (petri_net.getTransitionById(destinationId) is not None):
            source = petri_net.getPlaceById(sourceId)
            destination = petri_net.getTransitionById(destinationId)
        elif (petri_net.getTransitionById(sourceId) is not None) and (petri_net.getPlaceById(destinationId) is not None):
            source = petri_net.getTransitionById(sourceId)
            destination = petri_net.getPlaceById(destinationId)

        sourceIdInNetList = None
        destinationIdInNetList = None

        if type(source) == Place:
            sourceIdInNetList = petri_net.getPlaces().index(source)
            destinationIdInNetList = petri_net.getTransitions().index(destination)
            inputMatrix[sourceIdInNetList, destinationIdInNetList] = arc.getMultiplicity()

        if type(source) == Transition:
            sourceIdInNetList = petri_net.getTransitions().index(source)
            destinationIdInNetList = petri_net.getPlaces().index(destination)
            outputMatrix[destinationIdInNetList, sourceIdInNetList] = arc.getMultiplicity()

    # calculate incidence matrix
    incidenceMatrix = outputMatrix - inputMatrix

    # set matrices attributes for the net itself
    petri_net.inputMatrix = inputMatrix
    petri_net.outputMatrix = outputMatrix
    petri_net.incidenceMatrix = incidenceMatrix

    # # print info
    # print("\nInput matrix I:")
    # print(petri_net.inputMatrix)

    # print("\nOutput matrix O:")
    # print(petri_net.outputMatrix)

    # print("\nIncidence matrix C = O - I:")
    # print(petri_net.incidenceMatrix)

    # # transition presets/postsets
    # petri_net.printAllTransitionsPresets()
    # petri_net.printAllTransitionsPostsets()

    # REACHABILITY GRAPH
    # build reachability graph
    # reach_nxgraph = nx.MultiDiGraph()
    # reach_petrigraph = Graph()

    # # add first node manually
    # baseNode = reach_petrigraph.addNode(petri_net.getGraphState())
    # reach_nxgraph.add_node(baseNode.getName())

    # # variable to check whether or not the reachability graph is infinite
    # isInfinite = False
    # while True:
    #     allChecked = True
    #     for curNode in reach_petrigraph.nodes:
    #         if not curNode.isChecked:
    #             allChecked = False
    #             break

    #     # if all nodes are checked or graph is infinite, stop
    #     if allChecked or isInfinite:
    #         break

    #     for curNode in reach_petrigraph.nodes:
    #         if isInfinite:
    #             break
    #         if not curNode.isChecked:
    #             for trans in petri_net.getTransitions():
    #                 if petri_net.isTransitionRunnableFromState(trans, curNode.state):
    #                     newState = petri_net.runTransition(trans, curNode.state)

    #                     curNodePredcessors = (curNode.getAllPredcessorNames() + [curNode.getName()])
    #                     for cycleNode in reach_petrigraph.nodes:
    #                         # only check predcessors
    #                         if cycleNode.getName() in curNodePredcessors:
    #                             if petri_net.isState2GreaterThan1(cycleNode.state, newState):
    #                                 isInfinite = True
    #                                 break
    #                     if isInfinite:
    #                         break

    #                     newNode = None
    #                     if reach_petrigraph.hasNodeWithState(newState):
    #                         newNode = reach_petrigraph.getNodeWithState(newState)
    #                         if newNode != curNode:
    #                             newNode.mergePredcessorNodesFrom(curNode)
    #                     else:
    #                         newNode = reach_petrigraph.addNode(newState, curNode)

    #                     reach_nxgraph.add_edge(curNode.getName(), newNode.getName(), label=trans.getLabel())
    #             curNode.isChecked = True


    # if the graph is infinite, we cant create it, otherwise we can
    # if not isInfinite:
    #     print("\nPlotting Reachability Graphs...")

    #     reach_pyvisgraph = pvnet.Network(directed=True, width=PYVISGRAPH_W, height=PYVISGRAPH_H, heading="Reachability graph")
    #     reach_pyvisgraph_workflow = pvnet.Network(directed=True, width=PYVISGRAPH_W, height=PYVISGRAPH_H, heading="Reachability graph (Workflow)")
    #     for node in reach_nxgraph.nodes():
    #         nodeData = reach_petrigraph.getNodeWithName(node)
    #         nodeName = nodeData.getName()
    #         nodeLabel = nodeData.getGraphLabel()
    #         nodeColor = NODECOLOR_FIRST if node == list(reach_nxgraph.nodes())[0] else NODECOLOR_GENERIC
    #         nodeColor = NODECOLOR_LAST if node == list(reach_nxgraph.nodes())[-1] else nodeColor
    #         reach_pyvisgraph.add_node(nodeName, label=nodeLabel, shape="box", color=nodeColor, title=nodeName)
    #         # label for workflow graph (place names (but no static places), and no predcessors to reduce visual clutter)
    #         # predcessors are still available upon mouse hover over node
    #         nodeLabelWorkflow = nodeData.getGraphLabelCustom(petri_net.getWorkflowStateFromState(nodeData.state), "")
    #         nodeTitleWorkflow = "Predcessors: " + " ".join(nodeData.getAllPredcessorNames())
    #         reach_pyvisgraph_workflow.add_node(nodeName, label=nodeLabelWorkflow, shape="box", color=nodeColor, title=nodeTitleWorkflow)

    #     for nodeData, edgeLabel in petri_net.createEdgeDictFromGraph(reach_nxgraph).items():
    #         nodes = nodeData.split(" ")
    #         reach_pyvisgraph.add_edge(nodes[0], nodes[1], label=edgeLabel, color="black", title=edgeLabel)
    #         reach_pyvisgraph_workflow.add_edge(nodes[0], nodes[1], label=edgeLabel, color="black", title=edgeLabel)

    #     filename_reachgraph = "reachability_graph.html"
    #     print(f"Saving the result to '{filename_reachgraph}'")
    #     reach_pyvisgraph.set_options(getPyvisOptions())
    #     reach_pyvisgraph.save_graph(filename_reachgraph)

    #     filename_reachgraph_workflow = "workflow_reachability_graph.html"
    #     print(f"Saving the result to '{filename_reachgraph_workflow}'")
    #     reach_pyvisgraph_workflow.set_options(getPyvisOptions())
    #     reach_pyvisgraph_workflow.save_graph(filename_reachgraph_workflow)
    # else:
    #     print("\nReachability graph is infinite, can't plot.")

    ''' --------------------------------------------------------------
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    ------------------------------------------------------------------
    -------------------------------------------------------------- '''

    # # COVERABILITY TREE (old one, needed for coverability graph) (refactor this some day :P)
    # # build coverability tree (old)
    # cover_nxtree_old = nx.MultiDiGraph()
    # cover_petritree_old = Graph()

    # # add first node manually
    # baseNode = cover_petritree_old.addNode(petri_net.getGraphState())
    # baseNode.designationChar = "v"
    # cover_nxtree_old.add_node(baseNode.getName())
    # # cover_nxtree_old.add_node(str(baseNode.state))

    # while True:
    #     allChecked = True
    #     for curNode in cover_petritree_old.nodes:
    #         if not curNode.isChecked:
    #             allChecked = False
    #             break

    #     # if all nodes are checked, stop
    #     if allChecked:
    #         break

    #     for curNode in cover_petritree_old.nodes:
    #         if not curNode.isChecked:
    #             for trans in petri_net.getTransitions():
    #                 if petri_net.isTransitionRunnableFromState_Omega(trans, curNode.state):
    #                     shouldSkip = False
    #                     newState = petri_net.runTransition_Omega(trans, curNode.state)

    #                     newNode = None
    #                     if cover_petritree_old.hasNodeWithState(newState):
    #                         newNode = cover_petritree_old.addNode(newState, curNode)
    #                         newNode.isChecked = True
    #                         shouldSkip = True
    #                     else:
    #                         newNode = cover_petritree_old.addNode(newState, curNode)

    #                     newNode.designationChar = "v"

    #                     if not shouldSkip:
    #                         curNodePredcessors = (curNode.getAllPredcessorNames() + [curNode.getName()])
    #                         for cycleNode in cover_petritree_old.nodes:
    #                             # only check predcessors
    #                             if cycleNode.getName() in curNodePredcessors:
    #                                 if petri_net.isState2GreaterThan1_Omega(cycleNode.state, newState):
    #                                     newState = petri_net.transformState2ToOmega(cycleNode.state, newState)
    #                                     # break # dont stop at first valid node, check all

    #                     # cover_nxtree_old.add_edge(curNode.getName(), newNode.getName(), label=trans.getLabel())
    #                     cover_nxtree_old.add_edge(str(curNode.state), str(newNode.state), label=trans.getLabel())
    #             curNode.isChecked = True


    # new coverability tree

    # COVERABILITY TREE (new, good)
    # build coverability tree
    cover_nxtree = nx.MultiDiGraph()
    cover_petritree = Graph()

    # add first node manually
    baseNode = cover_petritree.addNode(petri_net.getGraphState())
    baseNode.designationChar = "v"
    # cover_nxtree.add_node(baseNode.getName())
    cover_nxtree.add_node(str(baseNode.state))

    def sorting_fun(t):
        (curNode, trans) = t
        if petri_net.hasTokensOnPlaces(curNode.state, slicing_criterion_indices):
            return (0, min_dist_to_sc[trans.getLabel()],  curNode.distance_to_root)
        return (min_dist_to_sc[trans.getLabel()] + curNode.distance_to_root, min_dist_to_sc[trans.getLabel()],  curNode.distance_to_root)

    def print_json_pn(curNode):
        # g_slice = nx.DiGraph()
        # current = curNode
        # while current is not None:
        #     new_current = current.predcessorNode
        #     if new_current is not None:
        #         g_slice.g_pn.add_edge(source, target)
        #     current = current.predcessorNode
        sp = nx.shortest_path(cover_nxtree, source=str(petri_net.getGraphState()), target=str(curNode.state))
        transitions_on_path = set()
        for i in range(len(sp) - 1):
            transitions_on_path.add(cover_nxtree.get_edge_data(sp[i], sp[i + 1])[0]["label"])
        places_sc = []
        places_sc_ind = petri_net.getTokensOnPlaces(curNode.state, slicing_criterion_indices)
        for p_ind, p in enumerate(petri_net.places):
            if p_ind in places_sc_ind:
                places_sc.append(p.getLabel())
        g_slice = nx.DiGraph()
        def get_marking(p):
            for p_ini in places:
                if p_ini[0] == p:
                    return p_ini[1]
            return None
        # print(places_sc)
        for p in places_sc:
            some_edge_added = False
            try:
                for t_ in g_pn.predecessors(p):
                    if t_ in transitions_on_path:
                        g_slice.add_edge(("transition",t_), ("place", p, get_marking(p)))
                        some_edge_added = True 
            except nx.exception.NetworkXError:
                pass
            if not some_edge_added:
                g_slice.add_node(("place", p, get_marking(p)))
                    # print("ADD EDGE 1")
        # print(transitions_on_path)
        for t in transitions_on_path:
            for p in g_pn.predecessors(t):
                g_slice.add_edge(("place", p, get_marking(p)), ("transition", t))
                # print("ADD EDGE 2")
                # print(json.dumps(json_graph.node_link_data(g_slice)))
                for t_ in g_pn.predecessors(p):
                    # print(t_)
                    # print(transitions_on_path)
                    if t_ in transitions_on_path:
                        g_slice.add_edge(("transition", t_), ("place", p, get_marking(p)))
                        # print("ADD EDGE 3")
        # FILE_GRAPH = 'examples/prova_fw.dot'
        # FILE_GRAPH_PNG = 'examples/prova_fw.png'

        # nx.drawing.nx_pydot.write_dot(g_slice, FILE_GRAPH)
        # subprocess.call(f'dot -Tpng {FILE_GRAPH} -o {FILE_GRAPH_PNG}', shell=True)
        with open('examples/prova_fw.json', 'w') as outfile1:
            outfile1.write(json.dumps(json_graph.node_link_data(g_slice)))

    done = set()
    while True:
        allChecked = True
        # print("ITERATION")
        for curNode in cover_petritree.nodes:
            # print(f"C1 {curNode.state}")
            if not curNode.isChecked:
                # print("NOT CHECKED")
                allChecked = False
                break
        for curNode in cover_petritree.nodes:
            # print(f"C2 {curNode.state}")
            if petri_net.hasTokensOnPlaces(curNode.state, slicing_criterion_indices):
                # print("TOKENS ON SC")
                print_json_pn(curNode)
                allChecked = True
                break
        # if all nodes are checked, stop
        if allChecked:
            break

        # for curNode in cover_petritree.nodes:
        #     print(f"C3 {curNode.state}")
        #     if petri_net.hasTokensOnPlaces(curNode.state, slicing_criterion_indices):
        #         break
        #     if not curNode.isChecked:
        #         for trans in petri_net.getTransitions():
        # for curNode, trans in :
        sorted_nodes_trans = sorted([(curNode, trans) for curNode in cover_petritree.nodes for trans in petri_net.getTransitions() if (curNode, trans) not in done], key=sorting_fun)
        # print(f"len(sorted_nodes_trans): {len(sorted_nodes_trans)}")
        while (len(sorted_nodes_trans) > 0):
            # print(list(map(lambda t: (t[0].state, t[1].getLabel(), sorting_fun(t)), sorted([(curNode, trans) for curNode in cover_petritree.nodes for trans in petri_net.getTransitions() if not curNode.isChecked], key=sorting_fun))))
            # print(list(map(lambda t: (t[0].state, t[1].getLabel(), sorting_fun(t)), sorted_nodes_trans)))
            (curNode, trans) = sorted_nodes_trans[0]
            # print((curNode.state, trans.getLabel()))
            # print(sorting_fun((curNode, trans)))
            if True:
                if True:
                    if petri_net.hasTokensOnPlaces(curNode.state, slicing_criterion_indices):
                        print_json_pn(curNode)
                        # print("FOUND_STOP_ITERATION")
                        break
                    if petri_net.isTransitionRunnableFromState_Omega(trans, curNode.state):
                        shouldSkip = False
                        newState = petri_net.runTransition_Omega(trans, curNode.state)

                        newNode = cover_petritree.addNode(newState, curNode)
                        # print(f"NEW STATE: {newState}")
                        newNode.designationChar = "v"

                        newNodePredcessors = newNode.getAllPredcessorNames()
                        for cycleNode in cover_petritree.nodes:
                            if cycleNode.getName() in newNodePredcessors:
                                if cycleNode.state == newNode.state:
                                    newNode.isChecked = True
                                    shouldSkip = True
                                    break


                        if not shouldSkip:
                            curNodePredcessors = (curNode.getAllPredcessorNames() + [curNode.getName()])
                            for cycleNode in cover_petritree.nodes:
                                # only check predcessors
                                if cycleNode.getName() in curNodePredcessors:
                                    # print(f"Node {curNode.getName()} with state {curNode.state}, transition {trans.getLabel()}, comparing new state {newState} with cyclestate {cycleNode.state}")
                                    if petri_net.isState2GreaterThan1_Omega(cycleNode.state, newState):
                                        newState = petri_net.transformState2ToOmega(cycleNode.state, newState)
                                        # print(f"New state is greater, transforming to {newState}")
                                        # break # dont stop at first valid node, check all
                            newNode.state = newState

                        # cover_nxtree.add_edge(curNode.getName(), newNode.getName(), label=trans.getLabel())
                        if cover_nxtree.has_edge(str(curNode.state), str(newNode.state)):
                            labels = cover_nxtree.get_edge_data(str(curNode.state), str(newNode.state))
                            # print(labels)
                            # print(cover_nxtree[str(curNode.state)][str(newNode.state)])
                            found = False
                            for l in labels:
                                if labels[l]["label"] == trans.getLabel():
                                    found = True
                                    break
                            if found:
                                continue
                        cover_nxtree.add_edge(str(curNode.state), str(newNode.state), label=trans.getLabel())
                # curNode.isChecked = True
                done.add((curNode, trans))
            sorted_nodes_trans = sorted([(curNode, trans) for curNode in cover_petritree.nodes for trans in petri_net.getTransitions() if (curNode, trans) not in done], key=sorting_fun)
            # print(list(map(lambda t: (t[0].state, t[1].getLabel(), sorting_fun(t)), sorted_nodes_trans)))

    # FILE_GRAPH = "examples/prova_cover.dot"
    # FILE_GRAPH_PNG = "examples/prova_cover.png"

    # nx.drawing.nx_pydot.write_dot(cover_nxtree, FILE_GRAPH)
    # subprocess.call(f'dot -Tpng {FILE_GRAPH} -o {FILE_GRAPH_PNG}', shell=True)

if __name__ == "__main__":
    build_coverabilty_tree([("P0", 0)], ["T0"], [("T0", "P0")])
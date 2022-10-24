# PN-Suite

<!-- ![Petri Net Slicer](http://kaz.dsic.upv.es/pn_slicer_logo.png) -->

PN-Suite is a system prepared to implement, combine, compare, and evaluate Petri net slicing algorithms.
Roughly, this system can be seen as a workbench that implements the currently most important algorithms for Petri net slicing (it is prepared to easily integrate more algorithms into it). This system provides a new functionality that is particularly useful for the analysis and optimization of Petri nets: it combines all the slicing algorithms with the analysis of properties in such a way that one can reduce the size of a Petri net producing a slice that preserves some desired properties.

PN-Suite implements interfaces to communicate with other systems such as [APT](https://github.com/CvO-theory/apt). This means that PN-Suite takes advantage of [APT](https://github.com/CvO-theory/apt) analyses to report about the properties kept or lost by the slices produced.

In the rest of this document we describe the main features and functionality of PN-Suite, and its architecture.

Table of contents
=================

  * [PN-Suite](#pn-suite)
  * [Table of contents](#table-of-contents)
  * [Installation](#installation)
  * [Docker](#docker)
  * [Usage](#usage)
    * [`pn_slicer`](#pn_slicer)
    * [`pn_prop`](#pn_prop)
    * [`pn_tools`](#pn_tools)
  * [Web interface](#web-interface)


Installation
============
There are two prerequisites to use this tool. One is the (free) tool [Graphviz](http://www.graphviz.org/), and the other is the (free) [Erlang/OTP framework](http://www.erlang.org/). These are the few steps needed to have PN-Suite installed in a Unix system.

	$ git clone https://github.com/tamarit/pn_suite.git
	$ cd pn_suite/
	$ make
	$ sudo make install
	
The first step clones the GitHub's repository content to the local system. Then, `make` is used to compile source files and, finally, three executables ([`pn_slicer`](#pn_slicer), [`pn_prop`](#pn_prop), and [`pn_tools`](#pn_tools)) are generated and installed by `make install`.

Docker
======

We have prepared a docker container for those users that are unable to find an environment that fullfils the intallation's requirements. To create the docker image simply run:

	$ docker build -t pn_suite .
	
The next step  is to run the container. To do so use thhe following command:

	$ docker run --name pn -it -v $PWD/examples:/pn_suite/examples --rm pn_suite
	
Once inside the container the instruction to follow are the same as it is explanied in section [Usage](#usage). The on-host examples' folder can be used to add your own petri nets or modify the existing ones. The outputs will be generated also there as explained in section [Usage](#usage).

Usage
=====

The three available commands are described in next sections.

pn_slicer
---------

This tool allows us to extract slices using one of the algorithms, or to extract all the slices (using all algorithms) that preserve a given set of properties

    $ pn_slicer PNML_FILE SLICING_CRITERION [PROPERTY_LIST | SLICING_ALGORITHM] [-json]
    
where `SLICING_CRITERION` is a list of places separated with commas.
`PROPERTY_LIST` is optional. It accepts [APT](https://github.com/CvO-theory/apt) properties.
Valid [APT](https://github.com/CvO-theory/apt) properties are:

	backwards_persistent
	output_nonbranching
	bicf
	strongly_connected
	free_choice
	pure
	plain
	persistent
	strongly_live
	k-marking
	s_net
	nonpure_only_simple_side_conditions
	bcf
	num_tokens
	asymmetric_choice
	safe
	weakly_live
	restricted_free_choice
	homogeneous
	k-bounded
	bounded
	reversible
	conflict_free
	weakly_connected
	num_places
	num_arcs
	simply_live
	num_transitions
	t_net
	num_labels
	isolated_elements

If not specified, all algorithms will be used.
To choose a slicing algorithm we have to write `alg:ALGORITHM` (no quotes required).
The names of the algorithms are:

    rakow_ctl
    rakow_safety
    llorens_maximal
    llorens_minimal
    yu

An example of this usage is:

	$ pn_slicer examples/other/pn_example.xml "P2,P7" alg:llorens_maximal
	Petri net named example successfully read.

	Slicing criterion: [P2, P7]

	Slicing using Llorens et al. (maximal)

For instance, we can compute several slices with the following command (observe that they all preserve the property `  conflict_free`.

	$ pn_slicer examples/other/pn_example.xml "P6,P9" "conflict_free" 
	Petri net named pn_example successfully read.
	Slicing criterion: [P6, P9]
	1.- Llorens et al.'s slicer (maximal) -> Reduction: 9.09 %
	2.- Rakow's slicer CTL -> Reduction: 0.00 %
	3.- Yu et al.'s slicer -> Reduction: 13.64 %
	4.- Rakow's slicer safety -> Reduction: 4.55 %
	
Each slice is stored in a file named `output/<PNML_NAME>_<OUTPUT_NUMBER>.pnml`. For example, Yu's slice generated above can be found at `output/example_4.pnml`. A pdf file is also generated. If flag `-json` is used, a JSON output is generated with exact details about locations and other relevant data.

	$ pn_slicer examples/other/pn_example.xml "P6,P9" "conflict_free" -json
	{
	  "slicing_criterion": [
	    {
	      "place": "P6"
	    },
	    {
	      "place": "P9"
	    }
	  ],
	  "preserved_properties": [
	    {
	      "property": "conflict_free"
	    }
	  ],
	  "warnings_parsing_slicing_criterion": "",
	  "warnings_parsing_properties": "",
	  "petri_net": {
	    "name": "example",
	    "file": "examples/other/pn_example.xml"
	  },
	  "slices": [
	    {
	      "algorithm": "llorens_maximal",
	      "reduction": "9.09",
	      "output_file": "output/example_2.pnml"
	    },
	    {
	      "algorithm": "rakow_ctl",
	      "reduction": "0.00",
	      "output_file": "output/example_3.pnml"
	    },
	    {
	      "algorithm": "yu",
	      "reduction": "13.64",
	      "output_file": "output/example_4.pnml"
	    },
	    {
	      "algorithm": "rakow_safety",
	      "reduction": "4.55",
	      "output_file": "output/example_5.pnml"
	    }
	  ]
	}

pn_prop
-------

This tool allows us to study the preservation of properties of a slice.

    $ pn_prop PNML_FILE PNML_FILE [PROPERTY_LIST]
    
Given two Petri nets (often a Petri net and its slice), it shows a list of properties that hold in both Petri nets, and a list of properties that only hold in the original Petri net. It is also possible to specify some specific properties, and only them will be analyzed. For the analysis of properties, `pn_prop` conveniently communicates with [APT](https://github.com/CvO-theory/apt). With the information provided by these tools, it decides whether the required properties are preserved. 

    $ pn_prop pn_example.xml output/example_1.pnml 
	Preserved properties:
	weakly_connected, output_nonbranching, strongly_connected, free_choice, pure, 
	plain, strongly_live, k-marking, s_net, nonpure_only_simple_side_conditions,
	simply_live, t_net, weakly_live, restricted_free_choice, homogeneous,
	isolated_elements, bounded, reversible, conflict_free
	Changed properties:
	backwards_persistent, num_places, bicf, persistent, bcf, num_tokens,
	asymmetric_choice, num_arcs, num_transitions, num_labels, safe, k-bounded

For instance, code bellow shows that property `simply_live` is preserved between the two Petri nets given as arguments.


	$ pn_prop pn_example.xml output/example_1.pnml "simply_live"
	true

pn_tools
--------

 This tool is interactive and it can be used to animate a Petri net, slice it, or convert it to several formats.
 
 	$ pn_tools pn_example.xml
	Petri net example successfully read.

	These are the available options:
	1 .- Run the Petri Net
	2 .- Export the Petri Net
	3 .- Slicing Llorens et al (maximal)
	4 .- Slicing Llorens et al (minimal)
	5 .- Slicing Llorens et al (for a given transition seq.)
	6 .- Slicing Yu et al
	7 .- Slicing Rakow CTL
	8 .- Slicing Rakow Safety
	What do you want to do?
	[1/2/3/4/5/6/7/8]: 

### Animation
The Petri net can be animated either manually or randomly. If manual animation is chosen, the system iteratively shows the user the enabled transitions, and she can select the transitions that must be fired. The Petri net that is being animated can be found at `output/<PN_NAME>_run.pdf` (thus, an external PDF viewer can be used as a visual support). In this PDF the enabled transitions are highlighted in red, so the user can see them clearly. In the random animation we can specify the number `n` of random steps to be performed, and the Petri net fires `n` random transitions, or until no more transitions can be fired.

	[1/2/3/4/5/6/7/8]:  1
	Available modes:
	0.- Manually
	n.- n random steps (at most)
	How do you want to run the PN? $ 10
	Selected transition: T5
	Selected transition: T9
	Selected transition: T10
	Selected transition: T7
	Selected transition: T8
	There is not any fireable transition.
	Execution:
	T5,T9,T10,T7,T8

### Slicing
The user can select from a menu a slicing algorithm. Then, according to the chosen algorithm, the user can specify a slicing criterion. The Petri net is then automatically sliced and a new Petri net (the slice) is produced. It is important to highlight that this tool implements another slicing algorithm (option 5) besides those of `pn_slicer`. This algorithm allows us to extract a slice from a Petri net considering a specific firing sequence (instead of all possible firing sequences as all the other algorithms do).

### Output
The output of PN-Suite can be produced in many different formats, including standard [PNML](http://www.pnml.org/) (compatible with [PIPE5](http://sarahtattersall.github.io/PIPE/)), [APT](https://github.com/CvO-theory/apt), DOT and more than 50 other formats provided by [Graphviz](http://www.graphviz.org/).

	[1/2/3/4/5/6/7/8]: $ 2
	1 .- pdf
	2 .- dot
	3 .- PNML (compatible with PIPE)
	4 .- APT
	5 .- Other formats
	What format do you need?
	[1/2/3/4/5]:  1
	
Web interface
=============

Even though we strongly encourage all users to install PN-Suite, we have implemented an [online web interface](http://kaz.dsic.upv.es/pn_slicer).

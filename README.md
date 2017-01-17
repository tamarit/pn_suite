# PN-Suite

<!-- ![Petri Net Slicer](http://kaz.dsic.upv.es/pn_slicer_logo.png) -->

We present PN-Suite, a system prepared to implement, combine, compare, and evaluate Petri net slicing algorithms.
Roughly, this system can be seen as a workbench that implements the currently most important algorithms for Petri net slicing (it is prepared to easily integrate more algorithms into it). This system provides a new functionality that is particularly useful for the analysis and optimization of Petri nets: It combines all the slicing algorithms with the analysis of properties in such a way that one can reduce the size of a Petri net producing a slice that preserves some desired properties.

PN-Suite implements interfaces to communicate with other systems such as [LoLA](http://home.gna.org/service-tech/lola/) and [APT](https://github.com/CvO-theory/apt]). This means that PN-Suite takes advantage of [LoLA](http://home.gna.org/service-tech/lola/) and [APT](https://github.com/CvO-theory/apt]) analyses to report about the properties kept or lost by the slices produced.

In the rest of this document we describe the main features and functionality of PN-Suite, and its architecture.

Installation and usage
----------------------
There are two prerequisites to use this tool. One is the (free) tool [Graphviz](http://www.graphviz.org/), and the other is the (free) [Erlang/OTP framework](http://www.erlang.org/). The (free) system [LoLA](http://home.gna.org/service-tech/lola/) is optional: it enables the use of [LoLA](http://home.gna.org/service-tech/lola/) expressions as properties to preserve when preforming slicing. These are the few steps needed to have PN-Slicer installed in a Unix system.

	$ git clone https://github.com/tamarit/pn_suite.git
	$ cd pn_suite/
	$ make
	$ sudo make install
	
The first step clones the GitHub's repository content to the local system. Then, `make` is used to compile source files and, finally, three executables ([`pn_slicer`](#pn_slicer), [`pn_prop`](#pn_prop), and [`pn_tools`](#pn_tools)) are generated and installed by `make install`.

pn_slicer
---------

This tool allows us to extract slices using one of the algorithms, or to extract all the slices (using all algorithms) that preserve a given set of properties

    $ pn_slicer PNML_FILE SLICING_CRITERION [PROPERTY_LIST|ALGORITHM] [-json]
    
where `SLICING_CRITERION` is a list of places separated with commas.
`PROPERTY_LIST` is optional. It accepts both [APT](https://github.com/CvO-theory/apt]) properties and [LoLA](http://home.gna.org/service-tech/lola/) expressions.
Valid [APT](https://github.com/CvO-theory/apt]) properties are:

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

LoLA expressions are preceded by `lola`, e.g., `lola: EF DEADLOCK`.
`ALGORITHM` is also optional. If not specified, all algorithms will be used.
To choose a slicing algorithm we have to write `alg: ALGORITHM` (no quotes required).
The names of the algorithms are:

    rakow_ctl
    rakow_safety
    llorens
    llorens_prec
    yu

For instance, we can compute several slices with the following command (observe that they all preserve the property `  conflict_free}` and the same deadlock freedom, i.e. `lola:EF DEADLOCK`).

	$ pn_slicer examples/other/pn_example.xml "P6,P9" "conflict_free,lola:EF DEADLOCK" 
	Petri net named example successfully read.
	Slicing criterion: [P6, P9]
	1.- Llorens et al's slicer improved -> Reduction: 54.55 %
	2.- Llorens et al's slicer -> Reduction: 9.09 %
	3.- Rakow's slicer CTL -> Reduction: 0.00 %
	4.- Yu et al's slicer -> Reduction: 13.64 %
	5.- Rakow's slicer safety -> Reduction: 4.55 %
	
Each slice is stored in a file named `output/<PNML_NAME>_<OUTPUT_NUMBER>.pnml`. For example, Yu's slice generated above can be found at `output/example\_4.pnml`. A pdf file is also generated. If flag `-json` is used, a JSON output is generated with exact details about locations and other relevant data.

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
	      "property": "lola:EF DEADLOCK"
	    },
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
	      "algorithm": "llorens_imp",
	      "reduction": "54.55",
	      "output_file": "output/example_1.pnml"
	    },
	    {
	      "algorithm": "llorens",
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
    
Given two Petri nets (often a Petri net and its slice), it shows a list of properties that hold in both Petri nets, and a list of properties that only hold in the original Petri net. It is also possible to specify some specific properties, and only them will be analyzed. For the analysis of properties, `pn_prop` conveniently communicates with either [LoLA](http://home.gna.org/service-tech/lola/), or [APT](https://github.com/CvO-theory/apt]), or both. With the information provided by these tools, it decides whether the required properties are preserved. 

        $ pn_prop pn_example.xml output/example_1.pnml 
	Preserved properties:
	weakly_connected, output_nonbranching, strongly_connected, free_choice, pure, 
	plain, strongly_live, k-marking, s_net, nonpure_only_simple_side_conditions,
	simply_live, t_net, weakly_live, restricted_free_choice, homogeneous,
	isolated_elements, bounded, reversible, conflict_free
	Changed properties:
	backwards_persistent, num_places, bicf, persistent, bcf, num_tokens,
	asymmetric_choice, num_arcs, num_transitions, num_labels, safe, k-bounded

For instance, code bellow shows that both properties, `simply_live` and `EF DEADLOCK}`, are preserved between the two Petri nets given as arguments.


	$ pn_prop pn_example.xml output/example_1.pnml "simply_live,lola:EF DEADLOCK"
	true

pn_tools
--------

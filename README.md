# PN-Suite

<!-- ![Petri Net Slicer](http://kaz.dsic.upv.es/pn_slicer_logo.png) -->

We present PN-Suite, a system prepared to implement, combine, compare, and evaluate Petri net slicing algorithms.
Roughly, this system can be seen as a workbench that implements the currently most important algorithms for Petri net slicing (it is prepared to easily integrate more algorithms into it). This system provides a new functionality that is particularly useful for the analysis and optimization of Petri nets: It combines all the slicing algorithms with the analysis of properties in such a way that one can reduce the size of a Petri net producing a slice that preserves some desired properties.

PN-Suite implements interfaces to communicate with other systems such as [LoLA](http://home.gna.org/service-tech/lola/) and (APT)[https://github.com/CvO-theory/apt]. This means that PN-Suite takes advantage of LoLA and APT analyses to report about the properties kept or lost by the slices produced.

In the rest of this document we describe the main features and functionality of PN-Suite, and its architecture.

Installation and usage
----------------------


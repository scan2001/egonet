---
title: 'egonet: tool for egocentric measures in Social Network Analysis'
tags:
  - R package
  - Social Network Analysis
  - egocentric networks
  - structural holes measures
  - large number of adjacency matrices
authors:
 - name: Andrea Sciandra
   orcid: 0000-0001-5621-5463
   affiliation: 1
 - name: Livio Finos
   orcid: 0000-0003-3181-8078
   affiliation: 2
affiliations:
 - name: STAR.Lab - Socio Territorial Analysis and Research, University of Padova
   index: 1
 - name: Department of Developmental Psychology and Socialisation, University of Padova
   index: 2
date: 18 October 2018
bibliography: paper.bib
---

# Summary

Egonet is a small tool for Social Network Analysis, dealing with egocentric network measures, including Burt's effective size and aggregate constraint and an import code suitable for a large number of adjacency matrices.
This is the development version of the egonet R package, available at https://cran.r-project.org/package=egonet (there is also an egonet web application: an example of output can be seen at http://www.egonet.associazionerospo.org/egonetdata/EgonetOutput.htm).
Reference manual is available at https://cran.r-project.org/web/packages/egonet/egonet.pdf.
This tool allows to read text tables and creates egocentric graphs from them (@wasserman1994social), with the ego actor in the first row and column. The result is a number of square matrices with common input/output connection' names. Thus, given a list of file' names containing the ego-network in the same directory, one can import all the egonets, compute (default) indices on the first subject of each adjacency matrix and then to merge socio-demographic information with the indices.
Egonet computes Burt's indexes (effective size and aggregate constraint) and other measures from sna package. The ego.gden index calculates the density score in a matrix without "ego" connections. Egonet also allow to restrict the analysis to one social circle identified by the name of the alters.
This software was cited by D. Halgin and S. Borgatti in the Connections journal as a R package specific to the analysis of ego network data (@halgin2012introduction).

# References

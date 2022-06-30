Replication data for "When Threat Mobilizes: Immigration Enforcement and Latino Voter Turnout"
Ariel White, Spring 2016

This replication package consists of several R scripts that pull in a number of different datasets in order to reproduce the analyses and figures presented in the paper.  
This file describes each script.

When using any of these scripts, please ensure that your working directory is set to the folder containing the various datasets.

- "SC_mainanalyses.R" takes in data on counties' treatment status, various covariates, and Latino voter turnout rates (from Catalist and ACS data), 
	and produces most of the figures and tables in the paper.
- "CCES_latinomobilization.R" takes in CCES survey data as well as data on counties' treatment status and covariates, and produces tables 5 and 6 from the paper, 
	and 12 and 13 from Appendix C.
- "CPS_turnout.R" takes in CPS data on voter turnout and produces Figure 4 and Table 8 from Appendix A.
- "mappingunits.R" takes in data on counties' treatment status and then produces Figure 1 from the paper, a map of counties' treatment status.

A number of these scripts pull in datasets that were constructed by merging multiple other primary sources in Stata.  For the stata code or the versions of the original data used, please email Ariel at arielrwhite@gmail.com and she'll be happy to send you these.

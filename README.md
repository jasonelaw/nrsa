nrsa
====

R package for calculating habitat metrics from the National Rivers and Streams Assessment (NRSA) protocol.
This package started as a large group of scripts writeen by folks at the US EPA NHEERL Western Ecology 
Division for the 2009 National Rivers and Streams Survey.  The scripts were written to work for a
specific data set in a specific environment.  There were lots of hard coded ODBC connections, file paths,
etc.  I reworked the meat of the scripts into functions that could be used to build metric calculation
scripts for data collected using the NRSA wadeable and non-wadeable protocols.

#! --matrixtable --lddin --clone mask.map
############################################
# Model: prepare baseflow  calibration NBS_GEULDAL      #
# Date: 2025-08-19                         #
# Author: Meindert Commelin                #
############################################

binding

outpoint = outpoints.map;
baseqtbl = baseq.tbl;

baseflow = baseflow.map;

initial

baseflow = lookupscalar(baseqtbl, 1, outpoint);
report baseflow = cover(baseflow, 0);

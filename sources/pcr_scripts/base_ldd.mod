#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM ldd and dem for the whole Geul
# made by Meindert Commelin 21/08/2025            
###################################################

binding 

# input maps
catch = catchment.map;
dem = dem.map;
chanmask = chanmask.map; 
outpoints = outpoints.map;

 
# output maps
Ldd = ldd.map;
streams = streams.map;

initial

adj = cover(chanmask, 0) * 10;

report dem = if(boolean(catch), dem);

report Ldd = lddcreate(dem-adj, 1e20,1e20,1e20,1e20); # correct topo for local depressions #
report streams = streamorder(Ldd);
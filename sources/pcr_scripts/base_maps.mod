#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# made by Meindert Commelin 21/08/2025            
###################################################

binding 

# input maps
catch = catchment.map;
dem = dem.map;

# output maps
Ldd = ldd.map;

initial

report dem = if(boolean(catch), dem);
report Ldd = lddcreate(dem, 1e20,1e20,1e20,1e20); # correct topo for local depressions #

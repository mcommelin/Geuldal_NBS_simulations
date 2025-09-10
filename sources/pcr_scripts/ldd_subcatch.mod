#! --matrixtable --lddin --clone mask.map

# PCRASTER script to calculate ldd maps for subcatchments of the Geul
# made by Meindert Commelin 09/09/2025            
###################################################

binding 

# input maps
catchment = catchment.map;
dem = dem.map;

# output map
Ldd = ldd.map;

initial

# remove data outside catchment area
report dem = if(boolean(catchment), dem);

# make ldd for the subcatchment
report Ldd = lddcreate(dem, 1e20,1e20,1e20,1e20); # correct topo for local depressions #

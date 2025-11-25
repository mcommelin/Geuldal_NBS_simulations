#! --matrixtable --lddin --clone mask.map

# PCRASTER script to calculate ldd maps for subcatchments of the Geul
# made by Meindert Commelin 09/09/2025            
###################################################

binding 

# input maps
catchment = catchment.map;
dem = dem.map;
subp = sub_point.map;

# output map
Ldd = ldd.map;

initial

# remove data outside catchment area
report dem = if(boolean(catchment), dem);
out = cover(subp, 0);
# make the outlet point very deep to always force LDD outlet at this point.
dem1 = if(out eq 1, dem - 100, dem);
report Ldd = lddcreate(dem1, 1e20,1e20,1e20,1e20); # correct topo for local depressions #



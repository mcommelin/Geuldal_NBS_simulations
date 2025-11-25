#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM ldd and dem for the whole Geul
# made by Meindert Commelin 21/08/2025            
###################################################

binding 

# input maps
catch = catchment.map;
dem = dem.map;
chanmask = chanmask.map; 
subp = sub_point.map;
 
# output maps
Ldd = ldd.map;

initial

report dem = if(boolean(catch), dem);
out = cover(subp, 0);
# make the outlet point very deep to always force LDD outlet at this point.
dem1 = if(out eq 1, dem - 100, dem);
report Ldd = lddcreate(dem1, 1e20,1e20,1e20,1e20); # correct topo for local depressions #

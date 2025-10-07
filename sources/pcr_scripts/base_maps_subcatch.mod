#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a base input maps for subcatchments of the Geul
# made by Meindert Commelin 21/08/2025            
###################################################

binding 

# input maps
catchment = catchment.map;
dem = dem.map;
lu = landuse.map;           # field id's for landuse 
soil = soils.map;           # field id's for texture/soil map
outpoint = outpoints.map;  # location of outlets and checkpoints
buildings = buildings.map;  # fraction of buildings in cell. (optional)
id = ID.map;
profile = profile.map;	# map with ubc soil codes for swatre


Ldd = ldd.map;

initial

# remove data outside catchment area
report dem = if(boolean(catchment), dem);
report soil = if(boolean(catchment), soil);
report lu = if(boolean(catchment), lu); 
report buildings = if(boolean(catchment), buildings);
report outpoint = if(boolean(catchment), outpoint);
report id = if(boolean(catchment), id);

# combine landuse with soil map for swatre

report profile = if (profile == 100,100,profile + 100 * lu);
report profn.map=nominal(profile);


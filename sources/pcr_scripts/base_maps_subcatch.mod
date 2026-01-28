#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a base input maps for subcatchments of the Geul
# made by Meindert Commelin 21/08/2025            
###################################################

binding 

# input maps
catchment = catchment.map;
dem = dem.map;
lu = landuse.map;           # field id's for landuse 
#soil = soils.map;           # field id's for texture/soil map
outpoint = outpoints.map;  # location of outlets and checkpoints
buildings = buildings.map;  # fraction of buildings in cell. (optional)
roads = roads_fraction.map;
hard = hard_surface.map;
id = ID.map;
idh = ID_hourly.map;
Ldd = ldd.map;

initial

# remove data outside catchment area
report dem = if(boolean(catchment), dem);
#report soil = if(boolean(catchment), soil);
report lu = if(boolean(catchment), lu); 
report buildings = if(boolean(catchment), cover(buildings,0));
report roads = if(boolean(catchment), cover(roads,0));
report hard = if(boolean(catchment), cover(hard,0));
report outpoint = if(boolean(catchment), cover(outpoint,0));
report id = if(boolean(catchment), id);
report idh = if(boolean(catchment), idh);


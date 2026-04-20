#! --matrixtable --lddin --clone mask.map
############################################
# Make retentionponds in DEM               #
# Date: 20-04-2026                         #
# Author: Meindert Commelin                #
############################################


binding

# load some mape
dem = dem.map;
ponds = nbs.map;
buffers = buffermask.map;

# the volume of the designed pond
# current approach will place everything within the assigned cells on the input map.
# make sure the surface assigned in the input map is realistic compared to the design volume!
pond_vol= ${1}; # [m3]

#adjusted dem
pond_dem = sw_dem.map; # set to dem.map in final code

initial
# some aux maps
area = dem * 0 + 1;

# remove ponds where buffers are applied
buffers = cover(buffers, 0);
ponds = if(buffers ne 0 and ponds eq 2, 1, ponds);

# identify all swale features and give uniform height
pond_clump = clump(nominal(ponds * area));
pond_mean_h = areaaverage(dem, pond_clump);
pond_surface = areaarea(pond_clump); #area in m2 of each individual pond

# all cells downstream - lower than the pond will be the 'dike'
# find the downstream 'dike' cells
# make the maps if a cell is a dike
dike_north = scalar(if(shift(dem, -1, 0) > dem and ponds eq 1 and shift(ponds, -1, 0) eq 2, shift(pond_mean_h, -1, 0),0));
dike_south = scalar(if(shift(dem, 1, 0) > dem and ponds eq 1 and shift(ponds, 1, 0) eq 2, shift(pond_mean_h, 1, 0),0));
dike_west = scalar(if(shift(dem, 0, -1) > dem and ponds eq 1 and shift(ponds, 0, -1) eq 2, shift(pond_mean_h, 0, -1),0));
dike_east = scalar(if(shift(dem, 0, 1) > dem and ponds eq 1 and shift(ponds, 0, 1) eq 2, shift(pond_mean_h, 0, 1),0));

# combine to 1 
pond_dike = boolean(if(dike_north + dike_south + dike_west + dike_east > 0, 1, 0) * area);

# make pond dike height
pond_dike_h = max(dike_north, dike_south, dike_west, dike_east);

# pond depth
pond_depth = pond_vol / pond_surface;

#make adjusted dem with pond = mean pond height minus pond depth
pond_dem = if(ponds eq 2, pond_mean_h - pond_depth, dem);
# and dike = mean dike height
report dem = if(pond_dike eq 1, pond_dike_h, pond_dem);

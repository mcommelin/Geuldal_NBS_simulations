#! --matrixtable --lddin --clone mask.map
############################################
# Make flowbarriers for swales             #
# Date: 23-03-2026                         #
# Author: Meindert Commelin                #
############################################


binding

# load some mape
dem = dem.map;
swales = swales.map;
buffers = buffers.map;

# the difference between the top of the dike and deepest
# point of the ditch
swale_dep = 1.0; # [m]
# the width of the ditch
swale_width = 1.0; #[m]

#adjusted dem
sw_dem = sw_dem.map; # set to dem.map in final code

initial
# some aux maps
area = dem * 0 + 1;

# swale volume
# we assume a triangle ditch so vol = (w*d) / 2
swale_vol = (swale_dep * swale_width) / 2; 

# the ditch in the DEM will be lowered to obtain the same
# volume as the designed swale
ditch_dep = swale_vol / celllength();

# remove swales where buffers are applied
swales = if(buffers ne 0, 0, swales);

# identify all swale features and give uniform height
sw_clump = clump(nominal(swales * area));
sw_mean_h = areaaverage(dem, sw_clump);

# the swale lines get a different landuse - so these represent the 'dike'
# one cell upstream will be the 'ditch'

# find the 'ditch' cells
# make the maps if a cell is a ditch
ditch_north = scalar(if(shift(dem, -1, 0) < dem and swales eq 0 and shift(swales, -1, 0) eq 1, shift(sw_mean_h, -1, 0),0));
ditch_south = scalar(if(shift(dem, 1, 0) < dem and swales eq 0 and shift(swales, 1, 0) eq 1, shift(sw_mean_h, 1, 0),0));
ditch_west = scalar(if(shift(dem, 0, -1) < dem and swales eq 0 and shift(swales, 0, -1) eq 1, shift(sw_mean_h, 0, -1),0));
ditch_east = scalar(if(shift(dem, 0, 1) < dem and swales eq 0 and shift(swales, 0, 1) eq 1, shift(sw_mean_h, 0, 1),0));

# combine to 1 
sw_ditch = boolean(if(ditch_north + ditch_south + ditch_west + ditch_east > 0, 1, 0) * area);

# make swale ditch height
sw_ditch_h = max(ditch_north, ditch_south, ditch_west, ditch_east);

#make adjusted dem with dike = mean swale dike height
sw_dem = if(swales eq 1, sw_mean_h, dem);
# and ditch = mean dike height - swale depth
report sw_dem = if(sw_ditch eq 1, sw_ditch_h - ditch_dep, sw_dem);

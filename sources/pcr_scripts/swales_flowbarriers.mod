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

swale_dep = 1.0;

# maps out
flowbarriers = flowbarriers.map;

# we make a map for each direction, with the value being the height of 
# the wall.

wall_height = 10.0; # height of walls in meters.

wall_north = FlowBarrierN.map;
wall_east = FlowBarrierE.map;
wall_south = FlowBarrierS.map;
wall_west = FlowBarrierW.map;


initial

area = dem * 0 + 1;
# remove swales where buffers are applied
swales = if(buffers ne 0, 0, swales);

report sw_clump.map = clump(nominal(swales * area));
report sw_mean_h.map = areaaverage(dem, sw_clump.map);

# the swale lines get a different landuse - so these represent the 'dike'
# one cell upstream will be the 'ditch'

# make the maps if a cell is a ditch
ditch_north = scalar(if(shift(dem, -1, 0) < dem and swales eq 0 and shift(swales, -1, 0) eq 1, shift(sw_mean_h.map, -1, 0),0));
ditch_south = scalar(if(shift(dem, 1, 0) < dem and swales eq 0 and shift(swales, 1, 0) eq 1, shift(sw_mean_h.map, 1, 0),0));
ditch_west = scalar(if(shift(dem, 0, -1) < dem and swales eq 0 and shift(swales, 0, -1) eq 1, shift(sw_mean_h.map, 0, -1),0));
ditch_east = scalar(if(shift(dem, 0, 1) < dem and swales eq 0 and shift(swales, 0, 1) eq 1, shift(sw_mean_h.map, 0, 1),0));

# combine to 1 
report sw_ditch.map = boolean(if(ditch_north + ditch_south + ditch_west + ditch_east > 0, 1, 0) * area);

# make swale ditch height
report sw_ditch_h.map = max(ditch_north, ditch_south, ditch_west, ditch_east);

#make adjusted dem with dike = mean swale dike height
sw_dem.map = if(swales eq 1, sw_mean_h.map, dem);
# and ditch = mean dike height - swale depth
report sw_dem.map = if(sw_ditch.map eq 1, sw_ditch_h.map - swale_dep, sw_dem.map);

# make the maps which indicate if the neighbour is lower
#wall_north = scalar(if(shift(dem, -1, 0) < dem and swales eq 1 and shift(swales, -1, 0) ne 1, 1,0));
#wall_south = scalar(if(shift(dem, 1, 0) < dem and swales eq 1 and shift(swales, 1, 0) ne 1, 3,0));
#wall_west = scalar(if(shift(dem, 0, -1) < dem and swales eq 1 and shift(swales, 0, -1) ne 1, 4,0));
#wall_east = scalar(if(shift(dem, 0, 1) < dem and swales eq 1 and shift(swales, 0, 1) ne 1, 2,0));

# combine into 1 map
#en = scalar(if(wall_north eq 1 and wall_east eq 2, 5,0));
#es = scalar(if(wall_south eq 3 and wall_east eq 2, 6,0));
#sw = scalar(if(wall_south eq 3 and wall_west eq 4, 7,0));
#wn = scalar(if(wall_north eq 4 and wall_west eq 1, 8,0));

#two_walls = en + es + sw + wn;
#one_wall = wall_north + wall_south + wall_west + wall_east;

#report walls = if(two_walls eq 0, one_wall, two_walls);

# add wall height to maps
#report wall_north = if(wall_north ne 0, wall_height, 0);
#report wall_south = if(wall_south ne 0, wall_height, 0);
#report wall_west = if(wall_west ne 0, wall_height, 0);
#report wall_east = if(wall_east ne 0, wall_height, 0);



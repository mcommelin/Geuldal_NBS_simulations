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

# make the maps which indicate if the neighbour is lower
wall_north = scalar(if(shift(dem, -1, 0) < dem and swales eq 1 and shift(swales, -1, 0) ne 1, 1,0));
wall_south = scalar(if(shift(dem, 1, 0) < dem and swales eq 1 and shift(swales, 1, 0) ne 1, 3,0));
wall_west = scalar(if(shift(dem, 0, -1) < dem and swales eq 1 and shift(swales, 0, -1) ne 1, 4,0));
wall_east = scalar(if(shift(dem, 0, 1) < dem and swales eq 1 and shift(swales, 0, 1) ne 1, 2,0));

# combine into 1 map
en = scalar(if(wall_north eq 1 and wall_east eq 2, 5,0));
es = scalar(if(wall_south eq 3 and wall_east eq 2, 6,0));
sw = scalar(if(wall_south eq 3 and wall_west eq 4, 7,0));
wn = scalar(if(wall_north eq 4 and wall_west eq 1, 8,0));

two_walls = en + es + sw + wn;
one_wall = wall_north + wall_south + wall_west + wall_east;

report walls = if(two_walls eq 0, one_wall, two_walls);

# add wall height to maps
report wall_north = if(wall_north ne 0, wall_height, 0);
report wall_south = if(wall_south ne 0, wall_height, 0);
report wall_west = if(wall_west ne 0, wall_height, 0);
report wall_east = if(wall_east ne 0, wall_height, 0);


# remove swales where buffers are applied
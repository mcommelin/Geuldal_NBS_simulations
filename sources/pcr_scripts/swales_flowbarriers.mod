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

# numbers in flow barriers
# were are the walls in for the cell
# 1 = north
# 2 = east
# 3 = south
# 4 = west
# 5 = north + east
# 6 = east + south
# 7 = south + west
# 8 = west + north



initial

# make the maps which indicate if the neighbour is lower
report wall_north.map = scalar(if(shift(dem, -1, 0) < dem and swales eq 1 and shift(swales, -1, 0) ne 1, 1,0));
report wall_south.map = scalar(if(shift(dem, 1, 0) < dem and swales eq 1 and shift(swales, 1, 0) ne 1, 3,0));
report wall_west.map = scalar(if(shift(dem, 0, -1) < dem and swales eq 1 and shift(swales, 0, -1) ne 1, 4,0));
report wall_east.map = scalar(if(shift(dem, 0, 1) < dem and swales eq 1 and shift(swales, 0, 1) ne 1, 2,0));

# combine into 1 map
report ne.map = scalar(if(wall_north.map eq 1 and wall_east.map eq 2, 5,0));
report es.map = scalar(if(wall_south.map eq 3 and wall_east.map eq 2, 6,0));
report sw.map = scalar(if(wall_south.map eq 3 and wall_west.map eq 4, 7,0));
report wn.map = scalar(if(wall_north.map eq 4 and wall_west.map eq 1, 8,0));

report two_walls.map = ne.map + es.map + sw.map + wn.map;
one_wall = wall_north.map + wall_south.map + wall_west.map + wall_east.map;

report walls.map = if(two_walls.map eq 0, one_wall, two_walls.map);


# remove swales where buffers are applied
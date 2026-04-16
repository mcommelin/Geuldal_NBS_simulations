#! --matrixtable --lddin --clone mask.map
############################################
# Adjust DEM for terraces	               #
# Date: 14-04-2026                         #
# Author: Meindert Commelin                #
############################################


binding

# load some mape
dem = dem_base.map;
terrace = nbs.map; #map with:
				# 1 = field with terraces
				# 2 = location of terraces on field
				# other values - no terraces / other land use

buffers = buffermask.map; # remove - should be done earlier
roads = road_tile_mask.map;
channels = chanmask.map;
# the contour elevation spacing of the designed terraces
# this should correspond to the input map
terrace_spacing = ${1}; # [m]
# the desired slope of the 'flat' sections 
desired_slope_p = ${2}; #[%]

#adjusted dem
ter_dem = ter_dem.map; 

initial
# some aux maps
area = dem * 0 + 1;

# remove terraces in cells with, buffers, roads or channels
buffers = cover(buffers, 0);
roads = cover(roads, 0);
channels =cover(channels, 0);

terrace = if(buffers ne 0 or roads ne 0 or channels ne 0, 0, terrace);
report terrace.map = terrace;

#slope to fraction
desired_slope = desired_slope_p / 100;

# calculate slope on original dem
loc_slope = max(sin(atan(slope(dem))),0.001); 

# mean slope around terrace
av_slope = windowaverage(loc_slope, 100);

report av_slope.map = av_slope;

# estimate distance to next upstream terrace
est_dist = terrace_spacing / av_slope;

#required terrace height to reduce upstream slope
ter_height = terrace_spacing - (est_dist * desired_slope);
ter_height = if(ter_height lt 0, 0, ter_height);
report ter_height.map = ter_height;

# add terrace height to dem
ter_dem = if(terrace eq 2, dem + ter_height, dem);

# find the first row upstream cells
# find upstream cell and place dem values of terrace
ditch_north = scalar(if(shift(dem, -1, 0) < dem and terrace eq 1 and shift(terrace, -1, 0) eq 2, shift(ter_dem, -1, 0),0));
ditch_south = scalar(if(shift(dem, 1, 0) < dem and terrace eq 1 and shift(terrace, 1, 0) eq 2, shift(ter_dem, 1, 0),0));
ditch_west = scalar(if(shift(dem, 0, -1) < dem and terrace eq 1 and shift(terrace, 0, -1) eq 2, shift(ter_dem, 0, -1),0));
ditch_east = scalar(if(shift(dem, 0, 1) < dem and terrace eq 1 and shift(terrace, 0, 1) eq 2, shift(ter_dem, 0, 1),0));

# combine to 1 
ter_two = boolean(if(ditch_north + ditch_south + ditch_west + ditch_east > 0, 1, 0) * area);

# make swale ditch height
ter_two_h = max(ditch_north, ditch_south, ditch_west, ditch_east);


# add terrace height to dem
ter_dem = if(ter_two eq 1 and ter_height > 0.1, ter_two_h, ter_dem);
report ter_dem_part.map = if(terrace ne 0 and ter_height > 0.1, ter_dem);

# filling is done with SAGA gis - wang & liu 2006. this allows a filling gradient

#report ter_dem_filled.map = lddcreatedem(ter_dem, 1e10, 1e10, 1e10, 1e10);
#report ter_dem = cover(ter_dem_filled.map, dem);

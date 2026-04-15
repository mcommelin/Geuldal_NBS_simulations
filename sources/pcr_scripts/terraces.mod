#! --matrixtable --lddin --clone mask.map
############################################
# Adjust DEM for terraces	               #
# Date: 14-04-2026                         #
# Author: Meindert Commelin                #
############################################


binding

# load some mape
dem = dem.map;
loc_slope = slope.map;
terrace = terrace.map; #map with:
				# 1 = field with terraces
				# 2 = location of terraces on field
				# other values - no terraces / other land use

buffers = buffers.map; # remove - should be done earlier

# the contour elevation spacing of the designed terraces
# this should correspond to the input map
terrace_spacing = 5.0; # [m]
# the desired slope of the 'flat' sections 
desired_slope = 5.0; #[%]

#adjusted dem
ter_dem = ter_dem.map; 

initial
# some aux maps
area = dem * 0 + 1;

#slope to fraction
desired_slope = desired_slope / 100;

# mean slope around terrace
av_slope = windowaverage(loc_slope, 50);

# estimate distance to next upstream terrace
est_dist = terrace_spacing / av_slope;

#required terrace height to reduce upstream slope
ter_height = terrace_spacing - (est_dist * desired_slope);

# add terrace height to dem
report ter_dem = if(terrace eq 2, dem + ter_height, dem);

# use SAGA GIS fill_sinks (wang & liu) to smooth the terraces!
#! --matrixtable --lddin --clone mask.map
############################################
# Make infiltrationstrips in dem           #
# Date: 23-03-2026                         #
# Author: Meindert Commelin                #
############################################


binding

# load some mape
dem = dem.map;
strips = nbs.map;
buffers = buffermask.map;

#adjusted dem
strip_dem = strip_dem.map; # set to dem.map in final code

initial
# some aux maps
area = dem * 0 + 1;

# remove strips where buffers are applied
strips = if(buffers ne 0, 0, strips);

# identify all strip features and give uniform height
st_clump = clump(nominal(strips * area));
st_mean_h = areaaverage(dem, st_clump);

#make adjusted dem with strip = mean strip height
report strip_dem = if(strips eq 2, st_mean_h, dem);

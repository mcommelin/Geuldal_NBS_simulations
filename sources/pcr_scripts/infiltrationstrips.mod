#! --matrixtable --lddin --clone mask.map
############################################
# Make infiltrationstrips in dem           #
# Date: 20-04-2026                         #
# Author: Meindert Commelin                #
############################################


binding

# load some mape
dem = dem.map;
strips = nbs.map;
buffers = buffermask.map;

# single nbs (1) or scenario with multiple measures (0)? 
single = ${1};

initial
# some aux maps
area = dem * 0 + 1;

# in maps based on the single measures (by Stroming) strips have value 2.
# in input maps with the full scenarios, strips get their lu_nr which is 20.
strip_num = if(single eq 1, 2, 20); 

# remove strips where buffers are applied
buffers = cover(buffers, 0);
strips = cover(if(buffers ne 0, 0, strips), 0) * area;

# identify all strip features and give uniform height
st_clump = clump(nominal(strips * area));
st_mean_h = areaaverage(dem, st_clump);

#make adjusted dem with strip = mean strip height
report dem = if(strips eq strip_num, st_mean_h, dem);

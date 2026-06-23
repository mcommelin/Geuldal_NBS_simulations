#! --matrixtable --lddin --clone mask.map

# PCRASTER script to analyse NBS simulation results
# made by Meindert Commelin 2026-06-23      
###################################################

# this script runs in the map directory of a LISEM database
# it calculates base statistics of the catchment and is
# part of the function catchment_overview() in the R workflow


binding 

### INPUT MAPS ### 
catch = catchment.map;
lu = landuse.map;
dem = dem.map;

### OUTPUT MAPS ###
grad = grad.map;
lu_area = lu_areas.map;
av_slope = av_slope.map;
lu_nom = lu_nom.map;

initial 
# we need nominal maps for the area function
# nominal maps also give better readable output in the pcrtable

area = if(catch eq 1, 1);

report grad = max(sin(atan(slope(dem))),0.001)*area; 

report lu_nom = nominal(lu);
report lu_area = nominal(areaarea(lu_nom));
report av_slope = nominal(areaaverage(grad, nominal(area))*100);

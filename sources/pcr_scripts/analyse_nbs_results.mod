#! --matrixtable --lddin --clone mask.map

# PCRASTER script to analyse NBS simulation results
# made by Meindert Commelin 2026-05-09      
###################################################

# this script runs in the result directory of a LISEM simulation
# the maps whmax, infiltration and interception from a
# baseline simulation should be copied to the result folder of
# a scenario simulation.


binding 

### INPUT MAPS ### 
lu = landuse.map;
lu_base = landuse_base.map;
infil = infiltration.map;
infil_base = base_infiltration.map;
icep = interception.map;
icep_base = base_interception.map;
whmax = whmax.map;
whmax_base = base_whmax.map;


### OUTPUT MAPS ###
whmax_diff = wh_diff.map;
infil_diff = infil_diff.map;
icep_diff = icep_diff.map;
lu_areas = lu_areas.map;
lu_nom = lu_nom.map;

infil_md = infil_md.map;
icep_md = icep_md.map;



initial 

# nominal landuse classes
report lu_nom = nominal(lu);


#calculate total area of catchment
tot_area = areaarea(nominal(lu * 0 + 1));

# area of each landuse type
report lu_areas = nominal(areaarea(lu_nom)); # m2

# difference in infiltration, interception and whmax
report whmax_diff = whmax - whmax_base;
report infil_diff = infil - infil_base;
report icep_diff = icep - icep_base;

# mean change per landuse class
report infil_md = nominal(areaaverage(infil_diff, lu_nom)*10); # in 0.1mm
report icep_md = nominal(areaaverage(icep_diff, lu_nom)*10); # in 0.1mm


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

catchment = catchment.map;
lu = landuse.map;
lu_base = landuse_base.map;
infil = infiltration.map;
infil_base = base_infiltration.map;
icep = interception.map;
icep_base = base_interception.map;
whmax = whmax.map;
whmax_base = base_whmax.map;


### OUTPUT MAPS ###





initial 

nbs = cover(nbs, 0) * catchment;

# for landscape elements value to change = 2 otherwise 1
nbs_val = if(do_LE eq 1, 2, 1);

# update the landuse map the the correct NBS class acording to the table
report landuse = if(nbs eq nbs_val, nbs_num, landuse);
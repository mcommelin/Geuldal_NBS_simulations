#! --matrixtable --lddin --clone mask.map

# PCRASTER script adjust landuse maps for NBS
# made by Meindert Commelin 2026-02-09      
###################################################

binding 

### INPUT MAPS ### 

catchment = catchment.map;
nbs = nbs.map;
landuse = landuse.map;
nbs_num = ${1};
do_LE = ${2};


initial 

nbs = cover(nbs, 0) * catchment;

# for landscape elements value to change = 2 otherwise 1
nbs_val = if(do_LE eq 1, 2, 1);

# update the landuse map the the correct NBS class acording to the table
report landuse = if(nbs eq nbs_val, nbs_num, landuse);
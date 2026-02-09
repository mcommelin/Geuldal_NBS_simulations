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

initial 

nbs = cover(nbs, 0) * catchment;
report landuse = if(nbs eq 1, nbs_num, landuse);
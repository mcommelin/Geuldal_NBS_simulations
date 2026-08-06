#! --matrixtable --lddin --clone mask.map

# PCRASTER script to prepare scenario maps with NBS measures
# part of the R-function: load_scenario_maps()
# made by Meindert Commelin 2026-08-06     
###################################################

binding 

### INPUT ### 
catchment = catchment.map;
scen = scen.map;
landuse = landuse.map;
lu_transform = ${1};
incl_graften = ${2};


initial 

# adjust landuse classes if needed
report lu_adj.map = if(lu_transform eq 1, lookupscalar(lu_classes.tbl, 1, scen));


# load maps depending on boolean switches
graften = if(incl_graften eq 1, scen_graften.map);


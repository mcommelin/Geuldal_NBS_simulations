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

scen_prep = scen_prep.map;


initial 

# adjust landuse classes if needed
lu_adj = if(lu_transform eq 1, lookupscalar(lu_classes.tbl, 1, scen), scen);
# fill incompatible lu_classes with original landuse
lu_adj = if(lu_adj eq -99.9, landuse, lu_adj);

# load maps depending on boolean switches
graften = if(incl_graften eq 1, scen_graften.map);

# the area where graften are applied get + 1000 for landuse, 
# which we can use later in the workflow to make two maps again.
lu_adj = if(incl_graften eq 1, if(graften eq 1, lu_adj + 1000, lu_adj), lu_adj);

# some nodata value persist due to resampling / errors in delivered maps
# fill all gaps with original landuse
lu_adj = cover(lu_adj, landuse);

report scen_prep = lu_adj;

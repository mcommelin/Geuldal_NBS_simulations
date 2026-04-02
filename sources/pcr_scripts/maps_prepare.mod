#! --matrixtable --lddin --clone mask.map

# PCRASTER script prepare some base input maps
# made by Meindert Commelin 2025-12-04         
###################################################

binding 

### INPUT MAPS ### 

ponds = pondmask.map;
dhydro = dhydro.map;

pmv = p_mv.map;
profile = profile.map;
fill = fill.map;

dem = dem.map;
burn = demburns.map;

hpc_sub = hpc_sub.map;
catch = catchment.map;

bound = flowboundary.map;

initial 


# remove all ponds that fall within the dhydro domain
report ponds = if(cover(dhydro, 0) eq 0, ponds);

# fill nodata values profile.map
profile = nominal(pmv);
fill = windowmajority(nominal(profile), 120);
report profile = scalar(cover(nominal(profile), fill));

#burn corrections around (rail)roads into dem
b1 = clump(nominal(burn));
b2 = areaminimum(dem, b1);
report dem = cover(b2, dem);

# create a flow boudary map for the hpc runs.
b3 = nominal(if(catch eq 1 and cover(hpc_sub, 0) eq 0, 1, 0));
report bound = boolean(if(windowdiversity(b3, celllength()+1) eq 2, 1, 0));
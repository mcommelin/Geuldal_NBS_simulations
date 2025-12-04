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

initial 


# remove all ponds that fall within the dhydro domain
report ponds = if(cover(dhydro, 0) eq 0, ponds);

# fill nodata values profile.map
profile = nominal(pmv);
fill = windowmajority(nominal(profile), 120);
report profile = cover(nominal(profile), fill);
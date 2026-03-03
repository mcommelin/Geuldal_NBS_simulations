#! --matrixtable --lddin --clone mask.map

# PCRASTER script to delineate subcatchments of the Geul
# for HPC simulations
# made by Meindert Commelin 12/01/2026           
###################################################

binding 

# input maps
Ldd = ldd.map;
dhydro = dhydro.map;
dem = dem.map;
catch = catchment.map;

# output map
hpc_sub = hpc_sub.map;

initial

# remove dhydro domain from ldd.map
d1 = cover(nominal(dhydro), 0);
d2 = windowdiversity(d1, 11);
d3 = boolean(if(d2 eq 2, 1));
dnum = nominal(uniqueid(d3));

# calculate subcatchments
hpc_sub = subcatchment(Ldd, dnum);
report hpc_sub = if(d1 eq 0, hpc_sub);

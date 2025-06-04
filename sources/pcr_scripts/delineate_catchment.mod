#! --matrixtable --lddin

# PCRASTER script to delineate a subcatchment within the Geul.
# based on the points defined in outpoints_description.csv
# made by Meindert Commelin 04/06/2025            
###################################################

binding

# input
dem = base_dem.map;
Ldd_base = base_ldd.map;
outlet = sub_point.map;

#output
catch = sub.map;

initial

catch = catchment(Ldd_base, outlet);
report catch = boolean(if(scalar(catch) eq 1, 1));

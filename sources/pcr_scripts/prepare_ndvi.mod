#! --matrixtable --lddin --clone mask.map
# PCRASTER script to recalculate manning, per, lai and smax from NDVI
# made by Victor Jetten 06/12/2025            
#####################################################################

binding
# input
 lutbl = lu.tbl;
 cal_lu = cal_lu.tbl;
 NDVI = ndvi${1}.map;
 lu = landuse.map;
 rr = rr.map;

# output
 per = per${1}.map;
 lai = lai${1}.map;
 mann = n${1}.map;
 smax = smax${1}.map; 

initial


#make lai corrrection by taking average ndvi of forest and stretching to 0.9
#laif = areaaverage(lu, NDVI);
NDVI=min(1.0,max(0, NDVI*1.1));
per = 0.01*(4.257*NDVI*NDVI + 100.719*NDVI + -5.439);
report per = max(0,min(0.99,per));
report lai = -ln(1.0-per)/0.4;
n_res = lookupscalar(lutbl, 2, lu); # residual n
mann_cal = lookupscalar(cal_lu, 2, lu);
report mann_cal.map = mann_cal;
report mann = (n_res + 0.1*rr+0.104*per**1.2) * mann_cal; 
# the relation between basal cover of stems and canopy cover is approx. bc = factor*c**1.2, 
# where the factor depends on the nr of stems poer m2. factor is ~0.06 for maize and  about 0.2 for grass or wheat
# so the regression of lisem data of 0.104 is actually quite good!
# add residual for forest manning, ndvi does not capture dead wood and litter, causing ndvi forrest to be smaller than for grass

smax_eq = lookupscalar(lutbl, 5, lu);
smax = if(smax_eq eq 1, 1.036+0.438*lai, 1);
smax = if(smax_eq eq 2, 0.233*lai, smax);
smax = if(smax_eq eq 3, 0.317*lai, smax);
smax = if(smax_eq eq 6, 0.286*lai, smax); 
smax = if(smax_eq eq 7, 0.317*lai, smax); 
smax = if(smax_eq eq 0, 0, smax); 
report smax = if(smax_eq eq 8, 0.59*(lai**0.88), smax);



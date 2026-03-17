#! --matrixtable --lddin --clone mask.map

#make G&A maps 
binding
soilt = soil.tbl;
profile = profile.map;
landuse = landuse.map;

ksat = ksat1.map;
depth = soildep1.map;
psi = psi1.map;
thetas = thetas1.map;
thetai = thetai1.map;


initial
profile = if(profile > 100, profile - landuse * 100, profile);

report ksat = lookupscalar(soilt, 6, profile);
report depth = lookupscalar(soilt, 1, profile);
report psi = scalar(15);
report thetas = lookupscalar(soilt, 5, profile);
report thetai = lookupscalar(soilt, 2, profile);
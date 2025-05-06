#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# made by Meindert Commelin 09/01/2025            
###################################################

binding 

### INPUT MAPS ### 

dem = dem.map;              # digital elevation model, area must be <= clone
lu = landuse.map;           # field id's for landuse 
#roads = roads.map;         # location of roads value = 1
# grass = grasswid.map;     # only if buffers are included
soil = soils.map;         # field id's for texture/soil map
# chanmask = chanmask.map;  # location of channels value = 1 (optional)
#outpoint = outpoints.map; # location of outlets and checkpoints
catchment = catchment.map; #
buildings = buildings.map; # fraction of buildings in cell.
### INPUT TABLES ### 

soiltbl = soil.tbl; 
# table with soil parameters for each field id 
lutbl = lu.tbl;
# table with crop and landuse parameters for each field id  
#
# lu
# 01 rr (cm) = random roughness
# 02 n = Manning's n
# 03 thetai (cm3/cm3) = initial moisture content
# 04 per (fraction) = surface cover by vegetation
# 05 lai (m2/m2) = leaf area index
# 06 ch (m) = crop height
# 07 cohadd (kPa) = additional cohesion by roots

# soil
# 01 stonefraction (ratio)
# 02 coh (kPa) = cohesion of soil
# 03 aggregate stability (number)
# 04 D50 (micro m)
# 05 ksat (mm/h)
# 06 thetas (cm3/cm3) = porosity   
# 07 soildepth (cm)


####################### 
### INPUT CONSTANTS ###
####################### 
# channel properties: 
# NOTE: if channels with different parameters, use a table as with landuse.
# Chanwidth = 2; # width of channels in meters 
# Chanside = 0; # tan of angle between side and surface (0 = rectangular)
# Chanman = 0.2; # Manning's n in channel
# Chancoh = 10; # high cohesion, kPa 
# ChanKsat = 20; # channel ksat for infiltration
# roads:
#widthroads = 6; # width of roads in meters
###################
### PROCES MAPS ###
###################
area = area.map; # value = 1
###################
### OUTPUT MAPS ### 
###################
## helper maps ###
one = one.map; #with scalar value 1
zero = zero.map; #with scalar value 0

### rainfall map ###
rain_id = id.map; # 
### basic topography related maps ###
grad = grad.map; # slope gradient
Ldd = ldd.map; # Local Drain Direction  
outlet = outlet.map; # location outlets and checkpoints 
### landuse maps ###
per = per.map; # surface cover by vegetation
lai= lai.map; # leaf area index
ch = ch.map;  # crop height
#roadwidth = roadwidth.map;
# grass = grasswid.map; # width of grass strips (optional)
# smax = smax.map; # max canopy storage (optional)
### surface maps ###
rr = rr.map; # random roughness
mann = n.map; # Manning's n
stone = stonefrc.map; # stone fraction 
# crust= crustfrc.map; # crusted fraction of surface (optional)
# comp = compfrc.map; # compacted fraction of surface (optional)
# hard = hardsurf.map; # impermeable surface (optional)
### erosion maps ### 
coh = coh.map; # cohesion of the soil
cohadd = cohadd.map; # additional cohesion by roots
aggrstab = aggrstab.map; # aggregate stability
D50 = d50.map; # median texture size
### infiltration maps ###
# for G&A 1st layer:
ksat= ksat1.map; 
psi= psi1.map; 
pore= thetas1.map; 
thetai= thetai1.map; 
thetas= thetas1.map;
soildep= soildep1.map; 
# for G&A 2nd layer: (optional)
# ksat2= ksat2.map; 
# psi2= psi2.map; 
# pore2= thetas2.map;
# thetai2= thetai2.map; 
# soildep2= soildep2.map;
### channel maps ### (optional)
# lddchan = lddchan.map; 
# chanwidth = chanwidt.map; 
# chanside = chanside.map;
# changrad = changrad.map; 
# chanman = chanman.map; 
# chancoh = chancoh.map;
### channel infiltration ### (optional)
# chanksat = chanksat.map;

initial 
####################
### PROCESS MAPS ###
####################

# remove data outside catchment area
report dem = if(boolean(catchment), dem);
report soil = if(boolean(catchment), soil);
report lu = if(boolean(catchment), lu); 
report buildings = if(boolean(catchment), buildings);
area = dem * 0 + 1;
report one = dem * 0 + 1; # map with value 1
report zero = dem * 0; # map with value 0
###########################
### MAPS WITH RAINFALL  ### 
########################### 
report rain_id = area; # only 1 rainfall zone
# for >1 rainfall zones based on points use ArcGIS or:
# report id = spreadzone(points, 0, friction);
# with; points = boolean map with locations of rainfall stations
# and friction = friction map (see page 70 of LISEMdocumentation6)
#################
### BASE MAPS ### 
#################
report grad = max(sin(atan(slope(dem))),0.001); 
report Ldd = lddcreate(dem, 1e20,1e20,1e20,1e20); # correct topo for local depressions #
report outlet = pit(Ldd);
##################### 
### LAND USE MAPS ### 
##################### 
report per = lookupscalar(lutbl, 4, lu); # fraction soil cover 
report ch = lookupscalar(lutbl, 6, lu); # crop height (m)
# choose method for lai:
report lai = lookupscalar(lutbl, 5, lu); # leaf area index
# or: (explained on page 71-72 from LISEMdocumentation6_170215)
# per = min(per, 0.95); 
# lai = ln(1-coverc)/-0.4;
# report lai = if(per gt 0, lai/per, 0); # leaf area index
####################
### SURFACE MAPS ### 
####################
report rr = lookupscalar(lutbl, 1, lu); # random roughness (=std dev in cm) 
report mann = lookupscalar(lutbl, 2, lu); # Manning's n
# report mann = 0.051*rr+0.104*per; # or use simple regression from Limburg data: CAREFULL this is not published 
report stone = lookupscalar(soiltbl, 1, soil); # stone fraction 
#report roadwidth = roads * scalar(widthroads);
####################
### EROSION MAPS ### 
####################
report coh = lookupscalar(soiltbl, 2, soil); 
report cohadd = lookupscalar(lutbl, 7, lu); 
report aggrstab = lookupscalar(soiltbl, 3, soil); 
report D50 = lookupscalar(soiltbl, 4, soil);
########################################## 
### INFILTRATION MAPS for GREEN & AMPT ### 
##########################################
report ksat = lookupscalar(soiltbl, 5, soil); 
#report psi = lookupscalar();
report thetas = lookupscalar(soiltbl, 6, soil); 
report thetai = lookupscalar(lutbl, 3, lu); 
report soildep = lookupscalar(soiltbl, 7, soil); 
# report ksat2 = lookupscalar(unittbl[name], [col.nr], [map.name]); 
# report psi2 = lookupscalar(unittbl[name], [col.nr], [map.name]);
# report thetas2 = lookupscalar(unittbl[name], [col.nr], [map.name]); 
# report thetai2 = lookupscalar(unittbl[name], [col.nr], [map.name]); 
# report soildep2 = lookupscalar(unittbl[name], [col.nr], [map.name]);
#################### 
### CHANNEL MAPS ###
####################
# report lddchan= lddcreate(dem*chanmask,1e20,1e20,1e20,1e20); 
# report chanwidth=chanmask*scalar(Chanwidth); 
# report chanside=chanmask*scalar(Chanside);
# report changrad=max(0.001,sin(atan(slope(chanmask*dem)))); 
# report chanman=chanmask*scalar(Chanman); 
# report chancoh=chanmask*scalar(Chancoh);
############################
### CHANNEL INFILTRATION ###
############################
# report chanksat = chanmask*scalar(ChanKsat);
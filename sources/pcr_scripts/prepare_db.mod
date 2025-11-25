#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# made by Meindert Commelin 03/06/2025            
###################################################

binding 

### INPUT MAPS ### 

dem = dem.map;              # digital elevation model, area must be <= clone
lu = landuse.map;           # field id's for landuse 
catchment = catchment.map;  #
#soil = soils.map;           # field id's for texture/soil map
roads = roads_fraction.map; # fraction road coverage (optional)
chanmask = chanmask.map;    # location of channels value = 1 (optional)
culvert = culvertmask.map;  # location of culverts
chanwidth = chanwidth.map;  # width of channels
chandepth = chandepth.map;  # depth of channels
chanside=chanside.map;
chantype = chantype.map;    # either stream (1) or ditch (2)
outpoint = outpoints.map;  # location of outlets and checkpoints
buildings = buildings.map;  # fraction of buildings in cell. (optional)
#grass = grasswid.map;      # only if buffers are included
id = ID.map;                # rainfall id grid
bua = bua.map; 		     # map with build up area.
buffers = buffermask.map;      # map with boolean location of retention buffers
#per = per.map; 		     # input map with cover based on NDVI
lai = lai.map;		     # map with lai based on NDVI (202306)
profile = profile.map;	# map with ubc soil codes for swatre
buf_outlet = buffer_outlet.map; # location and diameter of culvert outlets from buffers

### INPUT TABLES ### 

lutbl = lu.tbl;
chantbl = chan.tbl;	# table with param values for different channel types

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

### basic topography related maps ###
grad = grad.map; # slope gradient
Ldd = ldd.map; # Local Drain Direction  
outlet = outlet.map; # location outlets and checkpoints 
### landuse maps ###
roadwidth = roadwidth.map;
smax = smax.map; 

### surface maps ###
rr = rr.map; # random roughness
mann = n.map; # Manning's n
stone = stonefrc.map; # stone fraction 
# crust= crustfrc.map; # crusted fraction of surface (optional)
# comp = compfrc.map; # compacted fraction of surface (optional)
# hard = hardsurf.map; # impermeable surface (optional)

### infiltration maps ###
# swatre theta maps?

### channel maps ### (optional)
lddchan = lddchan.map; 
chandiam = chandiameter.map;
changrad = changrad.map; 
chanman = chanman.map; 
chanculvert = chanculvert.map;


initial 

####################
### PROCESS MAPS ###
####################
area = dem * 0 + 1;
report one = dem * 0 + 1; # map with value 1
report zero = dem * 0; # map with value 0
lu = if(lu eq 0, 5, lu); # adjust 0 values to urban area
lu = if(lu eq 5 and cover(bua,0) eq 1, 3,lu); # all builtup that is not bua is assumed to be roads and become grass (3)
report lu *= area; # apply ctachment mask

################
### PROFILE  ### 
################ 
profile = if(profile eq 100, 100,profile+100*lu)*area;
report profile = if(profile le 1000,100,profile)*area;
report profn.map = nominal(profile);


###########################
### MAPS WITH RAINFALL  ### 
########################### 
report id = if(boolean(catchment), id); 

#################
### BASE MAPS ### 
#################
report grad = max(sin(atan(slope(dem))),0.001); 
#report Ldd = lddcreate(dem, 1e20,1e20,1e20,1e20); # correct topo for local depressions # already made earlier
report outlet = pit(Ldd);
out1 = scalar(outlet) * 100; # used to force channel outlet to correct location
####################
### SURFACE MAPS ### 
####################
calbrRR = scalar(10.0); ## the field data were not very conclusive, at least multiply by 10 or more!
report rr = calbrRR*lookupscalar(lutbl, 1, lu); # random roughness (=std dev in cm) 

report mann = lookupscalar(lutbl, 2, lu); # Manning's n

# calculate interception
smax_eq = lookupscalar(lutbl, 5, lu);
smax = if(smax_eq eq 1, 1.036+0.438*lai, 1);
smax = if(smax_eq eq 2, 0.233*lai, smax);
smax = if(smax_eq eq 6, 0.286*lai, smax);
smax = if(smax_eq eq 7, 0.171*lai, smax);
report smax = if(smax_eq eq 8, 0.59*lai**0.88, smax);

# report mann = 0.051*rr+0.104*per; # or use simple regression from Limburg data: CAREFULL this is not published 
roadwidth = roads * celllength();
report roadwidth = if(boolean(catchment), roadwidth);

#################### 
### CHANNEL MAPS ###
####################
# when channel feature are too close to each other on the map, small side channels of length = 1 cell
# are created, we remove these first 
lddchan= lddcreate(dem*chanmask-out1,1e20,1e20,1e20,1e20); 
chanclean = accuflux(lddchan, 5); # 5 choosen by trial and error to get good channels on 5 and 20 m resolution.
chanclean = if(chanclean > celllength(), 1);
chanclean = if(boolean(catchment), chanclean);
report lddchan= lddcreate(dem*chanclean-out1,1e20,1e20,1e20,1e20); 
report chanmask=chanclean;
changrad=max(0.005,sin(atan(slope(chanclean*dem)))); 
report changrad=windowaverage(changrad,60)*chanclean; # smooth the slope over 60m to avoid instabilities in kin wave (Gulp)

# calculate mannings for channel
bua = cover(bua, 0);
chanclass = if(bua eq 1,chantype, chantype + 2);
chanman = lookupscalar(chantbl, 1, chanclass);
chandiam = if(culvert eq 1, chanwidth);

# all general culverts have type 5, all buffer outlets have type 2, only culverts in buffer wall, not on buffer floor.
bufculvert = scalar(if(cover(buf_outlet, 0) > 0, 2, 0));

chanculvert = scalar(if(cover(culvert, 0) eq 1, 5)); 
report chanculvert = if(bufculvert eq 2, bufculvert, chanculvert)*chanclean;
report chandiam = scalar(if(bufculvert eq 2, buf_outlet, chandiam))*chanclean;

#report chanculvert = if (bua eq 1 and cover(culvert, 0) gt 0,5,chanculvert) * chanclean;
report chanman = if(cover(chanculvert, 0) eq 2, 0.013, chanman)*chanclean; 




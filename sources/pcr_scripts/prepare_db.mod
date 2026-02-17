#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# called to make the fginal dbase in LISEM_run
#
# made by Meindert Commelin 03/06/2025    
# edits by Victor Jetten 14/12/2025
###################################################

binding 

### INPUT MAPS ### 

dem = dem.map;              # digital elevation model, area must be <= clone
lu = landuse.map;           # field id's for landuse 
catchment = catchment.map;  #
roads_bool = road_tile_mask.map;
roads = roads_fraction.map; # fraction road coverage (optional)
chanmask = chanmask.map;    # location of channels value = 1 (optional)
culvert = culvertmask.map;  # location of culverts
chanwidth = chanwidth.map;  # width of channels
chandepth = chandepth.map;  # depth of channels
chanside=chanside.map;
chantype = chantype.map;    # either stream (1) or ditch (2)
outpoint = outpoints.map;   # location of outlets and checkpoints
buildings = buildings.map;  # fraction of buildings in cell. (optional)
#grass = grasswid.map;      # only if buffers are included
id = ID.map;                # rainfall id grid
bua = bua.map; 		          # map with build up area.
buffers = buffermask.map;   # map with boolean location of retention buffers
profile0 = profile.map;	    # map with ubc soil codes for swatre
buf_outlet = buffer_outlet.map; # location and diameter of culvert outlets from buffers

### INPUT TABLES ### 
# calibration for standard maps moved to R code,
# still active in date specific do_ndvi = TRUE
lutbl = lu.tbl;     # 
chantbl = chan.tbl;	# table with param values for different channel types

# for info lu types: 
# 1 = akker, 2 = loofbos, 3 = productie gras, 4 = natuur gras, 5 = verhard, 6 = water, 7 = naaldbos

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
rr = rr.map;           # random roughness
mann = n.map;          # Manning's n
stone = stonefrc.map;  # stone fraction 
# crust= crustfrc.map; # crusted fraction of surface (optional)
# comp = compfrc.map;  # compacted fraction of surface (optional)
# hard = hardsurf.map; # impermeable surface (optional)
lai = lai.map;		     # map with lai 
per = per.map; 		     # input map with cover based on lu.tbl

### channel maps ### 
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
lu = if(lu eq 5 and cover(bua,0) eq 0, 3,lu); # all builtup that is not bua is assumed to be roads and become grass (3)
report lu *= area; # apply catchment mask
forest = boolean(lu == 2 or lu == 7);

################
### PROFILE  ### 
################ 
lu_num = if(lu < 10, lu * 100, lu); # original landuse to digit 4 nbs on digit 5 and 6
profile = scalar(profile0); # map is not always provided as scalar, force here
profile = if(profile eq 100, 100,profile + lu_num)*area;
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
report rr = lookupscalar(lutbl, 1, lu); # random roughness (=std dev in cm) 

# added per as last col
per = lookupscalar(lutbl, 7, lu);
report per = max(0,min(0.99,per));
report lai = -ln(1-min(0.95,per))/0.4;

# mannings N based on philips 1989: n = RR/100 + n_residue + n_vegetation * per
report mann = lookupscalar(lutbl, 8, lu);
# report mann = 0.051*rr+0.104*per; # or use simple regression from Limburg data: CAREFULL this is not published 

# calculate interception
smax_eq = lookupscalar(lutbl, 5, lu);
smax = if(smax_eq eq 1, 1.036+0.438*lai, 1);
smax = if(smax_eq eq 2, 0.233*lai, smax);
smax = if(smax_eq eq 3, 0.317*lai, smax);
smax = if(smax_eq eq 6, 0.286*lai, smax); 
smax = if(smax_eq eq 7, 0.317*lai, smax); 
smax = if(smax_eq eq 0, 0, smax); 
report smax = if(smax_eq eq 8, 0.59*(lai**0.88), smax);

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

# channel gradient
# reduce the slope of channels when they are close to a road
# often roads cause errors in the change grad due to DEM elevation
changrad=max(0.005,sin(atan(slope(chanclean*dem)))); 
rbuf = windowmaximum(cover(roads_bool, 0), 60);
changrad = if(rbuf eq 1, 0.02, changrad)*chanclean;
report changrad=windowaverage(changrad,60)*chanclean; # smooth the slope over 60m to avoid instabilities in kin wave (Gulp)

# avoid abrupt changes in channel width and depth
# gives better stability and lower MB error 
cw = windowmaximum(chanwidth,30); # make the first pixel of a side branch the size of the main branch
report chanwidth = min(0.95*celllength(), windowaverage(cw,30)) * chanclean; # then average the result which smoothes the connections
cd = windowmaximum(chandepth,30);
report chandepth = windowaverage(cd,30) * chanclean;

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
chanman = if(cover(chanculvert, 0) eq 2, 0.013, chanman)*chanclean; 
report chanman = windowaverage(if(forest,2*chanman, chanman),50)*chanclean;
#chosen channel manning is too low for LISEM kin wave, more in forest because of branches etc, and multiplied by 2




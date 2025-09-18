#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# made by Meindert Commelin 03/06/2025            
###################################################

binding 

### INPUT MAPS ### 

dem = dem.map;              # digital elevation model, area must be <= clone
lu = landuse.map;           # field id's for landuse 
catchment = catchment.map;  #
soil = soils.map;           # field id's for texture/soil map
roads = roads_fraction.map; # fraction road coverage (optional)
chanmask = chanmask.map;    # location of channels value = 1 (optional)
#culvert = culvertmask.map;  # location of culverts
chanwidth = chanwidth.map;  # width of channels
chandepth = chandepth.map;  # depth of channels
chantype = chantype.map;    # either stream (1) or ditch (2)
outpoint = outpoints.map;  # location of outlets and checkpoints
buildings = buildings.map;  # fraction of buildings in cell. (optional)
#grass = grasswid.map;      # only if buffers are included
id = ID.map;                # rainfall id grid
bua = bua.map; 		     # map with build up area.
buffers = buffers.map;      # map with boolean location of retention buffers
#per = per.map; 		     # input map with cover based on NDVI
lai = lai.map;		     # map with lai based on NDVI (202306)
profile = profile.map;	# map with ubc soil codes for swatre


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

####################
### SURFACE MAPS ### 
####################
report rr = lookupscalar(lutbl, 1, lu); # random roughness (=std dev in cm) 
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

# combine landuse with soil map for swatre
profile = profile + 100 * lu;
report profile = if(profile lt 1000, 100, profile);

#################### 
### CHANNEL MAPS ###
####################
# when channel feature are too close to each other on the map, small side channels of length = 1 cell
# are created, we remove these first 
lddchan= lddcreate(dem*chanmask,1e20,1e20,1e20,1e20); 
chanclean = accuflux(lddchan, 5); # 5 choosen by trial and error to get good channels on 5 and 20 m resolution.
chanclean = if(chanclean > celllength(), 1);
chanclean = if(boolean(catchment), chanclean);
report lddchan= lddcreate(dem*chanclean,1e20,1e20,1e20,1e20); 
report changrad=max(0.001,sin(atan(slope(chanmask*dem)))); 

# calculate mannings for channel
bua = cover(bua, 0);
chanclass = if(bua eq 1,chantype, chantype + 2);
report chanman = lookupscalar(chantbl, 1, chanclass);

# adjust channel in buffers
buffers = cover(buffers, 0);
report chanwidth = if(buffers eq 1, 3, chanwidth) * chanmask;
report chandepth = if(buffers eq 1, 0.2, chandepth) * chanmask;

# place culvert in buffer
report chanculvert = scalar(if(downstream(lddchan, buffers) eq 0 and buffers eq 1, 2));
report chandiam = scalar(if(chanculvert eq 2, 0.6));
#chanman = if(buffers eq 1, 0.2, chanman);
#report chanman = if(cover(chanculvert, 0) eq 2, 0.01, chanman);
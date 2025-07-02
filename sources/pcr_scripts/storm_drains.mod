#! --matrixtable --lddin --clone mask.map

# PCRASTER script to prepare storm drain maps
# made by Meindert Commelin 25/06/2025      
# based on the approach used by Victor Jetten in the OpenLISEM DB scripts
# see https://github.com/vjetten/openLISEMdata      
###################################################

binding

# input maps
bua = bua.map; # build up area map
roads = roads_fraction.map;
dem = dem.map;
catchment = catchment.map;

# input vars
ntile = 0.012; # the mannings n of storm drains.
diamtile = 0.7; # initial estimate storm drains.

#output maps
tiledepth = tiledepth.map;      # for stormdrains = 0, for soil drains > 0.
lddtile = lddtile.map;          # ldd of tile drains
tilediam = tilediameter.map;
tilegrad = tilegrad.map;        # gradient of the tile map
tileman = tileman.map;          # mannings n of the tile drains.
tileaccu = tileaccu.map;        # upstream accumulation to find drainage length
tilemask = tilemask.map;
vol_sd = vol_sd.map;
mm_sd = mm_sd.map;

initial

bua = if(boolean(catchment), bua);

# simplify the network by removing very small branches.
tilemask = if(bua == 1 and roads > 0, 1);
tiledem = lddcreatedem(dem * tilemask, 1e20, 1e20, 1e20, 1e20);
lddtile = lddcreate(tiledem, 1e20, 1e20, 1e20, 1e20);
tilemask = if(accuflux(lddtile, 1) > 2, 1);
#lddtile = lddcreate(tilemask * tiledem, 1e20, 1e20, 1e20, 1e20);
#tilemask = if(accuflux(lddtile, 1) > 1, 1);
#lddtile = lddcreate(tilemask * tiledem, 1e20, 1e20, 1e20, 1e20);
#tilemask = if(accuflux(lddtile, 1) > 1, 1);
report lddtile = lddcreate(tilemask * tiledem, 1e20, 1e20, 1e20, 1e20);

report tiledepth = tilemask * 0;
report tilegrad = sin(atan(slope(tilemask * dem)));
report tileman = tilemask * ntile;
report tileaccu = accuflux(lddtile, 1);
report tilediam = tilemask * diamtile;

# TODO: calculate storage capacity of the storm drian network
# should be around 7 mm for the BUA.
#vol_stormdrain / surface_area_BUA = 7mm
# vol_stormdrain = length(storm_drain) * (pi * r^2) 
report vol_sd=celllength()*3.1415*(tilediam/2) ** 2;
report mm_sd = maptotal(vol_sd) / areaarea(nominal(bua));
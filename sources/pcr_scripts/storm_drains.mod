#! --matrixtable --lddin --clone mask.map

# PCRASTER script to prepare storm drain maps
# made by Meindert Commelin 25/06/2025      
# based on the approach used by Victor Jetten in the OpenLISEM DB scripts
# see https://github.com/vjetten/openLISEMdata      
###################################################

binding

# input maps
#bua = bua.map; # map of build up area
tilestore = tilestorage.map; # mm of runoff to fit in storm drains.
#roads = roads_fraction.map;
roads = road_tile_mask.map;
dem = dem.map;
catchment = catchment.map;

# input vars
ntile = 0.012; # the mannings n of storm drains.
pi = 3.141592;

#output maps
tiledepth = tiledepth.map;      # for stormdrains = 0, for soil drains > 0.
lddtile = lddtile.map;          # ldd of tile drains
tilediam = tilediameter.map;
tilegrad = tilegrad.map;        # gradient of the tile map
tileman = tileman.map;          # mannings n of the tile drains.
tileaccu = tileaccu.map;        # upstream accumulation to find drainage length
tilemask = tilemask.map;
vol_sd = vol_sd.map;

initial

# simplify the network by removing very small branches.
tilemask = if(tilestore > 0 and roads > 0, 1);
tma = areaarea(clump(nominal(tilemask))) gt 2*cellarea();
tilenmask = if(tma,tilemask);

tiledem = lddcreatedem(dem * tilemask, 1e20, 1e20, 1e20, 1e20);
report lddtile = lddcreate(tiledem, 1e20, 1e20, 1e20, 1e20);

#tilemask = if(accuflux(lddtile, 1) > 2, 1);
#lddtile = lddcreate(tilemask * tiledem, 1e20, 1e20, 1e20, 1e20);
#tilemask = if(accuflux(lddtile, 1) > 1, 1);
#lddtile = lddcreate(tilemask * tiledem, 1e20, 1e20, 1e20, 1e20);
#tilemask = if(accuflux(lddtile, 1) > 1, 1);
#report lddtile = lddcreate(tilemask * tiledem, 1e20, 1e20, 1e20, 1e20);

# calculate diameter of drains based on required storage
tilestore = if(boolean(catchment), tilestore);
area_tile = areaarea(nominal(tilestore)); #m2
tile_num = areatotal(tilemask, nominal(tilestore)); # number of cells with storm drain
vol_sd = (area_tile div tile_num) * tilestore; # liter storage per cell

diamtile = 2 * sqrt((vol_sd / 1000) / (pi * celllength())); #
#diamtile = 0.8; # a diameter of ~800 mm results in a storage of about 7mm

report tiledepth = tilemask * 0;
report tilegrad = sin(atan(slope(tilemask * dem)));
report tileman = tilemask * ntile;
report tileaccu = accuflux(lddtile, 1);
report tilediam = tilemask * diamtile;

#! --matrixtable --lddin --clone mask.map

# PCRASTER script to prepare storm drain maps
# made by Meindert Commelin 25/06/2025      
# based on the approach used by Victor Jetten in the OpenLISEM DB scripts
# see https://github.com/vjetten/openLISEMdata      
###################################################

binding

# input maps
bua = bua.map; # build up area map
dem = dem.map;

# input vars
ntile = 0.012 # the mannings n of storm drains.

#output maps
tiledepth = tiledepth.map;      # for stormdrains = 0, for soil drains > 0.
lddtile = lddtile.map;          # ldd of tile drains
tilediam = tilediameter.map;
tilegrad = tilegrad.map;        # gradient of the tile map
tileman = tileman.map;          # mannings n of the tile drains.


initial

# simplify the network by removing very small branches 3 times.
tilemask = bua;
lddtile = ldd(tilemask, 1e20, 1e20, 1e20, 1e20);
tilemask = if(accuflux(lddtile, 1) > 1, 1, 0);
lddtile = ldd(tilemask * dem, 1e20, 1e20, 1e20, 1e20);
tilemask = if(accuflux(lddtile, 1) > 1, 1, 0);
lddtile = ldd(tilemask * dem, 1e20, 1e20, 1e20, 1e20);
tilemask = if(accuflux(lddtile, 1) > 1, 1, 0);
report lddtile = ldd(tilemask * dem, 1e20, 1e20, 1e20, 1e20);

report tiledepth = tilemask * 0;
report tilegrad = sin(atan(slope(tilemask * dem)))
report tilemann = tilemask * ntile;


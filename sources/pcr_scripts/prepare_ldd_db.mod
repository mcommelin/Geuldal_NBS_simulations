#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# called to make the fginal dbase in LISEM_run
#
# made by Meindert Commelin 03/06/2025    
# edits by Victor Jetten 14/12/2025
###################################################

binding 
# input maps
Ldd = ldd.map; # Local Drain Direction  
chanmask = chanmask.map;    # location of channels value = 1 (optional)
dem = dem.map;
catch = catchment.map;

# output maps
lddchan = lddchan.map; 
outlet = outlet.map; # location outlets and checkpoints 




initial

report outlet = pit(Ldd);
out1 = scalar(outlet) * 100; # used to force channel outlet to correct location

# when channel feature are too close to each other on the map, small side channels of length = 1 cell
# are created, we remove these first 
lddchan= lddcreate(dem*chanmask-out1,1e20,1e20,1e20,1e20); 
chanclean = accuflux(lddchan, 5); # 5 choosen by trial and error to get good channels on 5 and 20 m resolution.
chanclean = if(chanclean > celllength(), 1);
chanclean = if(boolean(catch), chanclean);
report lddchan= lddcreate(dem*chanclean-out1,1e20,1e20,1e20,1e20); 
report chanmask=chanclean;
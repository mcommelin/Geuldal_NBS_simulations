#! --matrixtable --lddin --clone mask.map
############################################
# Model: prepare baseflow NBS_GEULDAL      #
# Date: 2025-08-19                         #
# Author: Meindert Commelin                #
############################################


binding
# Input
baseflow = baseflow.map;
outlet = outlet.map;
catchment = catchment.map;  #
dem = dem.map;
chanmask = chanmask.map;    # location of channels value = 1 (optional)
culvert = culvertmask.map;  # location of culverts
chanwidth = chanwidth.map;  # width of channels and culverts
Chanman = 0.1; # Manning's n in channel
baseq = 2.0; # Discharge in m3/sec at outlet

# Output


lddchan = lddchan.map;
chandiam = chandiameter.map;
changrad = changrad.map; 
chanman = chanman.map; 
chanculvert = chanculvert.map;
basedischarge = baseq.map;

initial

# create baseflow maps
report basedischarge = scalar(if(outlet eq 1, baseq));

# adjust channel mask to only baseflow channels
chanmask = if(baseflow.map eq 1, chanmask);

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

report changrad = min(0.1, max(0.001,sin(atan(slope(chanmask*dem))))); 
report chanman = chanmask*scalar(Chanman); 
report chandiam = if(culvert eq 1, chanwidth * 1000);
report chanculvert = scalar(if(culvert eq 1, 2, 0)); # for now we assumme all culverts in channels are circular.
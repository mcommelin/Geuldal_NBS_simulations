#! --matrixtable --lddin --clone mask.map

# PCRASTER script calculate buffer volume and adjust to overlap with WL data
# made by Meindert Commelin 2026-02-18        
###################################################

binding 

### INPUT MAPS ### 

# input maps
bufvol = buffermask.map;      # map with volume (by WL) of retention buffers
bufculvert = buffer_outlet.map; # outlet locations + diameter of buffers
dem = dem.map;
catchment = catchment.map;
ponds = pondmask.map;
Ldd = ldd.map;
chanmask = chanmask.map;


# output maps
bufwall = bufwall.map; # # 0 is depression, wall is 2, rest is 1
buffers1 = buffers.map; # map with height change to add to dem if corrections are needed
demf = demfilled.map; # filled dem to calculate difference with original dem
bufvolest = bufvolest.map; # estmate of buffer volumes based on dem
bufvol_comb = bufvolcomb.map; 

bufmean_wl = bufmean_wl.map; # mean buffer volume WL data
bufmean_dem = bufmean_dem.map; # mean buffer volume DEM
bufmean_comb = bufmean_comb.map; 

buffer_out = outlet_buf.map;
maxq = maxq.map;

# values emperically found to improve the buffer simulation for the Geul catchment.
# the required change in the buffers depends on the resolution!!
wall_add = 0.0; # value to increase wall height
floor_down = -0.0; #value to decrease buffer floor 

# the max Q from buffer outlets is based on the volume and the time they empty:
empty_hours = 24; # 

initial 

# make a boolean buffermask
buffers = if(bufvol > 0, 1, 0);

# identify buffers, find the wall and the buffer floor.
buf = nominal(cover(buffers*0,catchment));
report bufwall = if(spread(nominal(buf),0,1) eq min(10, celllength()),2,buf); 
# 0 is depression, wall is 2, rest is 1

# edge buffers krijgt waarde 'wall_add' en de rest van de buffer 'floor_down'
buffers1=if(bufwall eq 2, wall_add,if(bufwall eq 0, floor_down,0))*catchment; # adding a wall can stops overland flow at the back!?
report buffers1 = if (cover(ponds,0) eq 1, floor_down, buffers1); # also add the ponds to the buffer map.


# make nominal classes for all buffers
a = clump(nominal(bufwall eq 0));

# fill the original dem
#report demf = lddcreatedem(dem, 10, 1e20, 1e20, 1e20);
# based on the difference between filled and original dem, we can estimate volume for each buffer.
dif = demf - dem;

report bufvolest = areatotal(if(bufwall eq 0, demf - dem), a) * cellarea();

# make a combination, take the WL volume if known, otherwise dem volume
bufvol_comb = if(bufwall eq 0, if(bufvol > 1, bufvol, bufvolest));

# calculate the average volume for the dem based buffers as well as the WL based buffers
report bufmean_wl = areaaverage(if(bufvol > 1, bufvol) * catchment, nominal(catchment));
report bufmean_dem = areaaverage(if(bufvolest > 0, bufvolest), nominal(catchment));
report bufmean_comb = areaaverage(if(bufvol_comb > 0, bufvol_comb), nominal(catchment));

# this check shows that on 10 m resolution the difference between dem buffers and wl buffer volumes is very small!
# so we propose to not change anything.

# all buffers with a very small volume are adapted to have at least the capacity to storm 1 m water over the
# floor area.
report bufvol_comb = if(bufvol_comb / areaarea(a) < 1, areaarea(a), bufvol_comb);

# find channel cells that leave a buffer
report buffer_out = scalar(if(bufwall eq 2 and downstream(Ldd, bufwall) eq 1, 1, 0)) * chanmask;

# calculate maxQ for buffer outlets
report maxq = if(buffer_out eq 1, upstream(Ldd, cover(bufvol_comb, 0)) / empty_hours, 0) / 3.6; # l /sec
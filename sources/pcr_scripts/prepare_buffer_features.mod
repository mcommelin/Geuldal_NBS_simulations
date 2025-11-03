#! --matrixtable --lddin --clone mask.map --lddfill 

# PCRASTER script to improve features of water retention buffers
# made by Victor Jetten 29/09/2025           
###################################################

### NOT active yet in workflow !!!
### Activate in create_lisem_run.R ~line 155 - 2025-09-29


binding

# input maps
buffers = buffermask.map;      # map with boolean location of retention buffers
bufculvert = buffers_outlet.map;
dem = dem.map;
catchment = catchment.map;
grad = grad.map;
chanwidth = chanwidth.map;  # width of channels
chandepth = chandepth.map;  # depth of channels
chanmask = chanmask.map;    # location of channels value = 1 (optional)


dem_orig = dem_orig.map;
buffers1 = buffers.map;
bufvolest = bufvolest.map;


initial

#report buffers = cover(buffers, 0) * catchment;
# edge buffers krijgt waarde 1 en de rest van de buffer -1
buf = nominal(cover(buffers*0,catchment));
 s = if(spread(nominal(buf),0,1) eq 5,2,buf); #this should be celllength() instead of 5 but with 20m this does not work

report buffers1=if(s eq 2,1,if(s eq 0,-1,0))*catchment; # with 1 meter wall but that stops overland flow
#report buffers1=if(s eq 2,0,if(s eq 0,-1,0))*catchment;


# bodem van de buffer krijgt de laagste waarde van de dem in de buffer
# talud van de buffer krijgt hoogste waarde van dem op de rand.
# save the original dem first, then update
report dem_orig = dem;

a = clump(nominal(buffers eq -1));
buffloor = areaminimum(dem, a); #/(areaarea(a.map)/cellarea())
buffwall = areamaximum(dem, a);
buffers2 = if(buffers1 eq -1, buffloor, dem);
report dem = if(buffers1 eq 1, buffwall, buffers2);
#report dem1 = if(buffers1 eq -1, buffloor, dem);
report grad = if(buffers1 eq -1, 0.005, grad);

# adjust channel in buffers
# do this in buffers! not here?
report chanwidth = if(buffers1 eq -1, 3, chanwidth);
report chandepth = if(buffers1 eq -1, 0.2, chandepth);
 


#geschat volume als je in lisem buffers aanzet met kaart buffers1.map
#demf = lddcreatedem(dem1 + buffers1, 10, 1e20, 1e20, 1e20);
#report bufvolest = areatotal((demf - dem1) * scalar(buffers1 eq -1),a)*cellarea();




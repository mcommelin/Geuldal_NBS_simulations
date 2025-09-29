#! --matrixtable --lddin --clone mask.map --lddfill 

# PCRASTER script to improve features of water retention buffers
# made by Victor Jetten 29/09/2025           
###################################################

### NOT active yet in workflow !!!
### Activate in create_lisem_run.R ~line 155 - 2025-09-29


binding

# input maps
buffers = buffers.map;      # map with boolean location of retention buffers
dem = dem.map;
catchment = catchment.map;
grad = grad.map;


buffers1 = buffers1.map;
bufvolest = bufvolest.map;

initial

# edge buffers krijgt waarde 1 en de rest van de buffer -1

buf = nominal(cover(buffers*0,catchment));
s = if(spread(nominal(buf),0,1) eq 5,2,buf);

report buffers1=if(s eq 2,1,if(s eq 0,-1,0))*catchment;

 

# bodem van de buffer krijgt de laagste waarde van de dem in de buffer

a = clump(nominal(buffers1 eq -1));
buffloor = areaminimum(dem, a); #/(areaarea(a.map)/cellarea())
dem1 = if(buffers1 eq -1, buffloor, dem);
grad1 = if(buffers1 eq -1, 0.01, grad);

 
#geschat volume als je in lisem buffers aanzet met kaart buffers1.map
#demf = lddcreatedem(dem1 + buffers1, 10, 1e20, 1e20, 1e20);
#report bufvolest = areatotal((demf - dem1) * scalar(buffers1 eq -1),a)*cellarea();

# to do: manage the channels in and outflow from the buffers properly

# current active code in prepare_db.mod
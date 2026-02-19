#! --matrixtable --lddin --clone mask.map --lddfill 

# PCRASTER script to improve features of water retention buffers
# made by Victor Jetten 29/09/2025           
###################################################


binding

# input maps
bufvol = buffermask.map;      # map with volume of retention buffers
bufculvert = buffer_outlet.map;
dem = dem.map;
catchment = catchment.map;
grad = grad.map;
chanwidth = chanwidth.map;  # width of channels
chandepth = chandepth.map;  # depth of channels
changrad = changrad.map;  # gradient of channels
chanman = chanman.map;     # Manning n of channels
chanmask = chanmask.map;    # location of channels value = 1 (optional)
ponds = pondmask.map;

initial

# make a boolean buffermask
buffers = if(bufvol > 0, 1, 0);

# identify buffers, find the wall and the buffer floor.
buf = nominal(cover(buffers*0,catchment));
bufwall = if(spread(nominal(buf),0,1) eq min(10, celllength()),2,buf); 
# 0 is depression, wall is 2, rest is 1

#adjust channel in buffers
report chandepth = if(bufwall eq 0, 0.1, chandepth)*chanmask;
chanman = if(bufwall eq 0, 0.1, chanman)*chanmask;
report chanside = if(bufwall eq 0, 1, 0)*chanmask; #trapezium shaped

# buffer outlet culverts
# this assumes that the buffer outlet has a design slope angle of 5%, and manning of concrete ...
report chanman = if(cover(bufculvert,0) eq 1, 0.013, chanman)*chanmask;
report changrad = if(cover(bufculvert,0) eq 1, 0.05, changrad)*chanmask;

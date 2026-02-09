#! --matrixtable --lddin --clone mask.map --lddfill 

# PCRASTER script to improve features of water retention buffers
# made by Victor Jetten 29/09/2025           
###################################################


binding

# input maps
buffers = buffermask.map;      # map with boolean location of retention buffers
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

# save as differetn named maps to avoid confusion and be able to really run with and without buffers
chanwidthbuf = chanwidthbuf.map;  # width of channels with buffers
chandepthbuf = chandepthbuf.map;  # depth of channels with buffers
chanmanbuf = chanmanbuf.map;  # man at outflow poins buffers
changradbuf = changradbuf.map;  # gra at outflow points buffers
chansidebuf = chansidebuf.map;

dem_orig = dem_orig.map;
buffers1 = buffers.map;
bufvolest = bufvolest.map;

initial

# edge buffers krijgt waarde 1 en de rest van de buffer -1
buf = nominal(cover(buffers*0,catchment));
s = if(spread(nominal(buf),0,1) eq min(10, celllength()),2,buf); #this should be celllength() instead of 5 but with 20m this does not work
report bufwall.map=s;
# 0 is depression, wall is 2, rest is 1
buffers1=if(s eq 2,0.5,if(s eq 0,-1,0))*catchment; # with 1 meter wall but that stops overland flow at the back?
report buffers1 = if (cover(ponds,0) eq 1, -1, buffers1);

# adjust channel in buffers
report chanwidthbuf = chanwidth;
report chandepthbuf = if(buffers1 eq -1, 0.1, chandepth)*chanmask;
chanmanbuf = if(buffers1 eq -1, 0.1, chanman)*chanmask;
report chansidebuf = if(buffers1 eq -1, 1, 0)*chanmask; #trapezium shaped

# buffer outlet culverts
# this assumes that the buffer outlet has a design slope angle of 2%, and manning of concrete ...
report chanmanbuf = if(cover(bufculvert,0) eq 1, 0.013, chanman)*chanmask;
report changradbuf = if(cover(bufculvert,0) eq 1, 0.05, changrad)*chanmask;




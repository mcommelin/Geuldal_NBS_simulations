#! --matrixtable --lddin --clone mask.map

# PCRASTER script to build a LISEM input database 
# made by Meindert Commelin 03/06/2025            
###################################################

binding 

### INPUT MAPS ### 

chantype = chantype.map;    # either stream (1) or ditch (2)
bua = bua.map; 		     # map with build up area.

### INPUT TABLES ### 



initial 


# calculate mannings for channel
bua = cover(bua, 0);
chanclass = if(bua eq 1,chantype, chantype + 2);
chantype = lookupscalar(chanclass, 1, chantbl);

############################################
# Model: Adjust DEM for flooding areas     #
# Date: 28-10-2026                         #
# Author: Tijs de Pagter                   #
############################################

binding

# load maps
dem = dem.map;
landuse = landuse.map;

areamap 
dem;

initial

# Identify cells with NBS_ID for flood area dams (NBS_ID = 29).
Dams = landuse == 29;

# Assign separate IDs to each line of cells to separate the different dams.
LineID = clump(Dams); 

# Count for each cell the amount of neighbour cells with NBS_ID = 29. 
Neighbour_Count = window4total(if(Dams, scalar(1), scalar(0)));

# Identify the end cells of each line of cells by the amount of neighbours,
# end cells will only have one neighbouring cell with NBS_ID = 29.
Endpoints = Dams and (Neighbour_Count eq 1);

# Find the lower and higher endpoint elevations for each line.
# find the lowest point on the line, replace all other values with very large value (=NA)
Min_Height = areaminimum(if(Endpoints, dem, scalar(1e30)),LineID);

# find the highest point on the line, replace all other values with a very large value (=NA)
Max_Height = areamaximum(if(Endpoints, dem, scalar(1e30)),LineID);

# Set a threshold for the two different options for dam height.
Threshold = Min_Height + 1.5;

# Choose the correct height option for each line.
# If max_height is lower than min_height + 1,5 meter then the dam height equals max height.
# Otherwise, the dam height equals min_height + 1,5 meters
Dam_Height = if( Max_Height < Threshold, Max_Height, Threshold);

# The cells will be raised to the correct dam height, dependent on which option is chosen.
# note: cells will only be raised, never lowered.
Updated = if(Dams,max(dem, Dam_Height),dem);

report updated_dem.map = Updated;

# TODO
# add some code to prevent absurdly high dams?

# for testing if the procedure works: calculate the difference between the original
# dem and updated dem.

Difference = Updated - dem;
report test_difference.map = Difference;
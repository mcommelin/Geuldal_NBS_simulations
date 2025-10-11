import numpy as np
from osgeo import gdal

# Input raster paths
classified_path = 'soilcode.tif'
value_map_paths = ['countrynr.tif','clay15m.tif', 'silt15m.tif', 'sand15m.tif','bd15m.tif']  # Add as needed
output_txt = 'class_values_combined.csv'

# Read classified raster
ds_class = gdal.Open(classified_path)
class_array = ds_class.GetRasterBand(1).ReadAsArray().flatten()

# Read value rasters and flatten them
value_arrays = []
for path in value_map_paths:
    ds_val = gdal.Open(path)
    val_array = ds_val.GetRasterBand(1).ReadAsArray().flatten()
    value_arrays.append(val_array)

# Stack arrays: shape (num_pixels, num_maps)
stacked_values = np.stack(value_arrays, axis=1)

# Combine class with stacked values: shape (num_pixels, num_maps + 1)
combined = np.column_stack((class_array, stacked_values))

# Remove rows with NaNs (optional)
combined = combined[~np.isnan(combined).any(axis=1)]

# Get unique rows by class
# Create dictionary: class → first encountered value tuple
class_value_map = {}
for row in combined:
    cls = int(row[0])
    if cls not in class_value_map:
        class_value_map[cls] = row[1:]

# Sort by class
sorted_items = sorted(class_value_map.items())

# Write to file
with open(output_txt, 'w') as f:
    # Header
    header = ['class'] + [f'value{i+1}' for i in range(len(value_map_paths))]
    f.write(','.join(header) + '\n')
    
    for cls, values in sorted_items:
        line = [str(cls)] + [f"{v:.4f}" for v in values]
        f.write(','.join(line) + '\n')

from osgeo import gdal
import numpy as np
import sys
from scipy import ndimage

def fill_nodata_nearest(input_raster, output_raster):
    in_ds = gdal.Open(input_raster, gdal.GA_ReadOnly)
    if in_ds is None:
        print(f"Error: Could not open input raster '{input_raster}'")
        sys.exit(1)

    driver = gdal.GetDriverByName("GTiff")
    out_ds = driver.CreateCopy(output_raster, in_ds, 0)
    if out_ds is None:
        print(f"Error: Could not create output raster '{output_raster}'")
        sys.exit(1)

    band = out_ds.GetRasterBand(1)
    nodata = band.GetNoDataValue()
    data = band.ReadAsArray()

    if nodata is None:
        print("Error: NoData value is not defined for the raster.")
        sys.exit(1)

    # Mask of nodata cells
    mask = (data == nodata)

    # Get indices of nearest non-nodata cell for each pixel
    # distance_transform_edt returns indices of nearest valid pixels
    nearest_indices = ndimage.distance_transform_edt(
        mask,
        return_distances=False,
        return_indices=True
    )

    # Fill nodata with nearest neighbor values
    filled = data[tuple(nearest_indices)]

    band.WriteArray(filled)
    band.SetNoDataValue(nodata)  # keep nodata definition
    band.FlushCache()

    band = None
    in_ds = None
    out_ds = None
    print(f"Filled raster written to {output_raster}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <input_raster> <output_raster>")
        sys.exit(1)
    fill_nodata_nearest(sys.argv[1], sys.argv[2])


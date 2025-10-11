from osgeo import gdal
import sys

def fill_nodata(input_raster, output_raster, search_dist=100.0, smoothing_iterations=0):
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
    gdal.FillNodata(targetBand=band,
                    maskBand=None,
                    maxSearchDist=search_dist,
                    smoothingIterations=smoothing_iterations)

    in_ds = None
    out_ds = None

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <input_raster> <output_raster>")
        sys.exit(1)
    fill_nodata(sys.argv[1], sys.argv[2])

from osgeo import gdal, osr
import sys

def assign_epsg(input_raster, output_raster, epsg_code):
    in_ds = gdal.Open(input_raster, gdal.GA_ReadOnly)
    if in_ds is None:
        print(f"Error: Could not open input raster '{input_raster}'")
        sys.exit(1)

    driver = gdal.GetDriverByName("GTiff")
    out_ds = driver.CreateCopy(output_raster, in_ds, 0)
    if out_ds is None:
        print(f"Error: Could not create output raster '{output_raster}'")
        sys.exit(1)

    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg_code)

    out_ds.SetProjection(srs.ExportToWkt())
    out_ds = None
    in_ds = None

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <input_raster> <output_raster> <epsg_code>")
        sys.exit(1)
    assign_epsg(sys.argv[1], sys.argv[2], int(sys.argv[3]))

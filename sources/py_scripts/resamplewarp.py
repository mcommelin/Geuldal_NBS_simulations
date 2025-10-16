import subprocess
import sys
from osgeo import gdal

def resample_to_reference(input_tif, reference_tif, output_tif, epsg, rmethod):
    # Open reference dataset
    ref_ds = gdal.Open(reference_tif)
    if ref_ds is None:
        raise RuntimeError(f"Cannot open reference file: {reference_tif}")

    # Extract geotransform and size
    gt = ref_ds.GetGeoTransform()
    x_size = ref_ds.RasterXSize
    y_size = ref_ds.RasterYSize
    proj = "EPSG:{E}".format(E=epsg)

    xmin = gt[0]
    ymax = gt[3]
    pixel_width = gt[1]
    pixel_height = gt[5]
    xmax = xmin + x_size * pixel_width
    ymin = ymax + y_size * pixel_height

    # Build gdalwarp command
    cmd = [
        "gdalwarp",
        "-t_srs", proj,
        "-te", str(xmin), str(ymin), str(xmax), str(ymax),
        "-ts", str(x_size), str(y_size),
        "-r", rmethod, 
	"-overwrite",
        input_tif, "gdw.tif"
    ]
    #print("gdalwarp","-t_srs ", proj,"-te", str(xmin), str(ymin), str(xmax), str(ymax),"-ts", str(x_size), str(y_size),"-r", "near", input_tif, output_tif)
    subprocess.check_call(cmd)
    print("convert")

    cmd = [
      "gdal_translate",
      "-ot", "Float32",
      "-of", "PCRaster",
      "-mo", "PCRASTER_VALUESCALE=VS_SCALAR",
      "gdw.tif",
      output_tif
    ]
    subprocess.check_call(cmd)


if __name__ == "__main__":
    if len(sys.argv) != 6:
        print("Usage: python resample.py <input.map> <clone.map> <output.map> <epsg number> <rmethod>")
        sys.exit(1)

    input_tif = sys.argv[1]
    reference_tif = sys.argv[2]
    output_tif = sys.argv[3]
    epsg = sys.argv[4]
    rmethod = sys.argv[5]

    resample_to_reference(input_tif, reference_tif, output_tif, epsg, rmethod)

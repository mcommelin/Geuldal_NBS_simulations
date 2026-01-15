import os
import sys
import subprocess
from osgeo import gdal

def resample_to_reference(input_asc, reference_map, output_map, epsg, rmethod):
    ref_ds = gdal.Open(reference_map)
    if ref_ds is None:
        raise RuntimeError(f"Cannot open reference file: {reference_map}")
    SRC_WKT = (
	'PROJCS["Stereographic_North_Pole",'
	'GEOGCS["GCS_unnamed ellipse",DATUM["D_unknown",SPHEROID["Unknown",6370040,0]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],'
	'PROJECTION["Stereographic_North_Pole"],'
	'PARAMETER["standard_parallel_1",60],'
	'PARAMETER["central_meridian",10],'
	'PARAMETER["false_easting",0],'
	'PARAMETER["false_northing",0],'
	'UNIT["Meter",1]]')

    gt = ref_ds.GetGeoTransform()
    x_size = ref_ds.RasterXSize
    y_size = ref_ds.RasterYSize
    #tgt_wkt = "EPSG:28992" #ref_ds.GetProjection()
    tgt_wkt = f"EPSG:{epsg}"
    xmin = gt[0]
    ymax = gt[3]
    xmax = xmin + x_size * gt[1]
    ymin = ymax + y_size * gt[5]

    cmd = [
        "gdalwarp",
        "-overwrite",
        "-s_srs", SRC_WKT,
        "-t_srs", tgt_wkt,
        "-te", str(xmin), str(ymin), str(xmax), str(ymax),
        "-ts", str(x_size), str(y_size),
        "-r", rmethod,
        input_asc,
        "temp.tif"
    ]
    subprocess.check_call(cmd)

    cmd = [
        "gdal_translate",
        "-ot", "Float32",
        "-of", "PCRaster",
        "-mo", "PCRASTER_VALUESCALE=VS_SCALAR",
        "temp.tif",
        output_map
    ]
    subprocess.check_call(cmd)

    os.remove("temp.tif")	
    name = f"{output_map}.aux.xml"
    os.remove(name)	


if __name__ == "__main__":
    if len(sys.argv) != 6:
        print(
            "Usage:\n"
            "python resample_folder.py <asc_folder> <mask.map> <output_folder> <epsg> <rmethod>"
        )
        sys.exit(1)

    asc_folder = sys.argv[1]
    mask_map = sys.argv[2]
    output_folder = sys.argv[3]
    epsg = sys.argv[4]
    rmethod = sys.argv[5]

    os.makedirs(output_folder, exist_ok=True)

    asc_files = sorted(
        f for f in os.listdir(asc_folder) if f.lower().endswith(".asc")
    )

    timestamps = []
    start_day = 173          # 22 June 2023
    step_minutes = 5

    for idx, fname in enumerate(asc_files):
        input_asc = os.path.join(asc_folder, fname)
	
        # consecutive numeric naming: .001, .002, ...
        base = os.path.splitext(fname)[0]
        #output_name = f"230622a0.{idx:03d}"
        #output_name = f"{base}a.{idx:03d}"
        output_name = f"{base}.map"
        total_minutes = idx * step_minutes
        day = start_day + total_minutes // 1440
        minutes = total_minutes % 1440
        timestamp = f"{day:03d}:{minutes:04d}"	
        timestamps.append((output_name, timestamp))
        
        output_map = os.path.join(output_folder, output_name)
        print(output_map)
        resample_to_reference(
            input_asc,
            mask_map,
            output_map,
            epsg,
            rmethod
        )

    txt_path = os.path.join(output_folder, "timestamps.txt")

    with open(txt_path, "w") as f:
        for name, ts in timestamps:
            f.write(f"{ts} {name}\n")
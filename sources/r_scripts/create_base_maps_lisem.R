



# 1. create subcatchment maps -------------------------------------------------

# load the outpoints csv file
points <- read_csv("LISEM_data/tables/outpoints_description.csv")

cell_size <- unique(points$cell_size)

# loop over resolutions
for (j in seq_along(cell_size)) {
  subdir <- paste0("LISEM_data/Geul_", cell_size[j], "m/maps/")
  
  # filter the correct resolution
  points_res <- points %>%
    filter(cell_size == cell_size[j]) %>%
    select(x, y, point)
  # write csv table
  write_csv(points_res, file = paste0(subdir, "outpoints.txt"),
            col_names = FALSE)
  # run col2map
  col2map(col_in = "outpoints.txt", map_out = "outpoints.map",
          sub_dir = subdir, options = "-N")
  # make the subcatchment map
  pcrcalc(
    work_dir = subdir,
    options = paste0("'subcatch.map=subcatchment(ldd.map, outpoints.map)'")
  )
}
# function to compare temporal resolutions of precipitation

# id.map to catchment size - is now included in standard db script
#wdir <- "LISEM_runs/Watervalderbeek_5m/maps/"
#ev_date <- "2023-06-22"


subcatch_observed <- function(wdir = NULL,
                              ev_date = NULL,
                              tres = "hour") {
  
  # pcrcalc(options = "ID.map=ID.map*catchment.map", work_dir = wdir)
  
  # map2asc
  # map2asc(map_in = "ID.map",
  #         map_out = "rain_ID.asc",
  #         sub_dir = wdir)
  # find unique grid id's
  rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
  id <- as.vector(rainIDs)
  freq <- as_tibble(table(id)) %>%
    mutate(id_nm = paste0("gauge_", id))
  
  # load rain data
  date_str <- str_remove_all(ev_date, "-")
  if (tres == "min") {
    pfile <- paste0("LISEM_data/rain/rain_5min_", date_str, ".txt")
  } else {
    pfile <- paste0("LISEM_data/rain/rain_", date_str, ".txt")
  }
  skipval <- as.numeric(readLines(pfile)[2]) + 2
  rain_txt <- readLines(pfile)[-(1:skipval)]
  nms <- readLines(pfile)[3:(skipval)] %>%
    str_replace_all(., " ", "_")
  # make a tibble with numbers and row names
  a <- str_split(rain_txt, " ")
  b <- as_tibble(do.call(rbind, a)) %>%
    rename_with( ~ nms) %>%
    mutate(across(-time, as.numeric)) %>%
    mutate(time_min = str_remove(time, "001:"),
           time_min = as.numeric(time_min)) %>%
    select(time_min, all_of(freq$id_nm))
  
  c <- b %>%
    pivot_longer(cols = -time_min,
                 values_to = "P",
                 names_to = "id_nm") %>%
    left_join(freq, by = "id_nm") %>%
    mutate(Ptmp = P * n) %>%
    group_by(time_min) %>%
    summarize(P = round(sum(Ptmp) / sum(n), digits = 2))
  
  #TODO include discharge and add to figure
  
  #TODO redefine begin and end times for subcatch events based on P and Q observed
  
  
  # make figure
  if (tres == "min") {
    total = round(sum(c$P) / 12, digits = 2)
  } else {
    total = round(sum(c$P), digits = 2)
  }
  plot <- ggplot(c) +
    geom_bar(aes(x = time_min, y = P), stat = "identity") +
    theme_classic() +
    labs(x = "Minutes", y = "P mm/h", 
         title = paste0("Event total = ", total, " in ", subcatch_name, " on ", ev_date))
  
  rain_info <- vector("list", length = 2)
  rain_info[[1]] <- plot
  rain_info[[2]] <- total
  
  return(rain_info)
} # end function subcatchment observations

# 4. function to compare discharge ---------------------------------------------


# function to compare temporal resolutions of precipitation

# id.map to catchment size - is now included in standard db script
#wdir <- "LISEM_runs/Gulp_10m/maps/"
#ev_date <- "2024-08-17"


subcatch_rain_compare <- function(wdir = NULL,
                              ev_date = NULL,
                              tres = "hour") {
  
  # pcrcalc(options = "ID.map=ID.map*catchment.map", work_dir = wdir)
  
  # map2asc
  map2asc(map_in = "ID.map",
          map_out = "rain_ID.asc",
          sub_dir = wdir)
  # find unique grid id's
  rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
  id <- as.vector(rainIDs)
  freq <- as_tibble(table(id)) %>%
    mutate(id_nm = paste0("gauge_", id))
  
  # load rain data
  date_str <- str_remove_all(ev_date, "-")
  if (tres == "min") {
    pfile <- paste0("LISEM_runs/rain/rain_5min_", date_str, ".txt")
  } else {
    pfile <- paste0("LISEM_runs/rain/rain_", date_str, ".txt")
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
    mutate(doy = str_remove(time, ":\\d\\d\\d\\d"),
           mod = str_remove(time, "\\d\\d\\d:"),
           hours = str_pad(floor(as.numeric(mod)/60), width = 2, side = "left", pad = "0"),
           mins = str_pad(floor(as.numeric(mod) %% 60), width = 2, side = "left", pad = "0"),
           date = as.Date(as.numeric(doy), origin = paste0(year(events$date[i]), "-01-01")),
           datestring = paste0(date, " ", hours, ":", mins),
           timestamp = ymd_hm(datestring) - days(1)) %>%
    select(timestamp, all_of(freq$id_nm)) %>%
    filter(date(timestamp) >= ymd(ev_date))
  
  c <- b %>%
    pivot_longer(cols = -timestamp,
                 values_to = "P",
                 names_to = "id_nm") %>%
    left_join(freq, by = "id_nm") %>%
    mutate(Ptmp = P * n) %>%
    group_by(timestamp) %>%
    summarize(P = round(sum(Ptmp) / sum(n), digits = 2))
  
  # make figure
  if (tres == "min") {
    total = round(sum(c$P) / 12, digits = 2)
  } else {
    total = round(sum(c$P), digits = 2)
  }
  plot <- ggplot(c) +
    geom_bar(aes(x = timestamp, y = P), stat = "identity") +
    theme_classic() +
    labs(x = "Minutes", y = "P mm/h", 
         title = paste0("Event total = ", total, " in ", subcatch_name, " on ", ev_date))
  
  rain_info <- vector("list", length = 2)
  rain_info[[1]] <- plot
  rain_info[[2]] <- total
  
  return(rain_info)
} # end function subcatchment rain comparison



# show observed discharge and rain per subcatch 
# the function
graph_subcatch_qp <- function(points_id = NULL,
                              event_dates = NULL) {
  
  # events
  events <- read_csv("sources/selected_events.csv") %>%
    mutate(ts_start = ymd_hms(event_start),
           ts_end = ymd_hms(event_end)) %>%
    filter(date(ts_start) %in% event_dates)
  
  points <- read_csv("LISEM_data/setup/outpoints_description.csv")
  
  ## precipitation 5 minute resolution (GMT +1)
  rain_5min <- read_csv("data/processed_data/neerslag/KNMI_rain_5min.csv")
  
  # make loop over subcatch
  rain_sub <- vector("list", length = length(points_id))
  for (i in seq_along(points_id)) {
    point_id <- points_id[i]
    # select subcatchment
    subcatch <- points %>%
      filter(point == point_id) %>%
      filter(cell_size == 5)
    subcatch_name <- subcatch$subcatch_name
    wdir <- paste0("LISEM_runs/", subcatch_name, "_10m/maps/")
    
    # map2asc
    map2asc(map_in = "ID.map",
            map_out = "rain_ID.asc",
            sub_dir = wdir)
    
    # load rain id's from discharge
    rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
    id <- as.vector(rainIDs)
    freq <- as_tibble(table(id)) %>%
      mutate(id_nm = paste0("gauge_", id))
    
    rain_sub[[i]] <- rain_5min %>%
      select(timestamp, all_of(freq$id))  %>%
      pivot_longer(cols = -timestamp,
                   values_to = "P",
                   names_to = "id") %>%
      left_join(freq, by = "id") %>%
      mutate(Ptmp = P * n) %>%
      group_by(timestamp) %>%
      summarize(P = round(sum(Ptmp) / sum(n), digits = 2)) %>%
      mutate(point = point_id)
    
  }
  rain_sub <- bind_rows(rain_sub)
  
  rain <- map2_dfr(events$ts_start, events$ts_end,
                   ~filter(rain_sub, timestamp >= .x & timestamp <= .y)) %>%
    left_join(events, join_by(
      closest(timestamp <= ts_end)
    )) %>%
    mutate(ev_name = as.character(date(ts_start)))
  
  # add discharge
  q_obs <- read_csv("data/processed_data/obs_discharge/observed_discharge_high_res.csv") %>%
    left_join(events, b = c("ev_num", "use")) %>%
    mutate(ev_name = as.character(date(ts_start)))
  
  # make figures for both subcatch
  
  for (i in seq_along(points_id)) {
    p <- filter(rain, point == points_id[i]) %>%
      filter(ev_num %in% events$ev_num)
    q <- filter(q_obs, point == points_id[i])%>%
      filter(ev_num %in% events$ev_num)
    
    # plot
    # axis constants
    q_max_round <- ceiling(max(c(q$Q), na.rm = TRUE) / 10) * 10
    p_max       <- max(p$P, na.rm = TRUE)
    k           <- q_max_round / (p_max * 2)
    y_top       <- q_max_round
    
    # plot regualr and inverted y-axis
    ggplot() +
      geom_linerange(data = p, aes(x = timestamp, ymin = y_top,
                                   ymax = y_top - P * k)) +
      #geom_ribbon(aes(ymin = qmin, ymax = qmax), fill = "grey60", alpha = 0.3) +
      geom_line(data = q, aes(x = timestamp, y = Q, color = code), linewidth = 0.3) +
      # geom_line(aes(y = sel_run),           colour = "red", linetype = "dashed", linewidth = 0.8) +
      # axis
      scale_y_continuous(
        name     = "Discharge (m³ s⁻¹)",
        limits   = c(0, y_top),
        sec.axis = sec_axis(
          ~ (y_top - .) / k,
          name = "Precipitation (mm)"
        ),
        expand = c(0,0)) +
      scale_x_datetime(date_breaks = "3 hours", date_labels = "%H %M",
                       name = "Time of day", expand = c(0,0)) +
      labs(color = "Meetpunt") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    ggsave(paste0("images/subcatch_observations/q_and_p_", points_id[i], ".png"))
  }
} # end graph_subcatch_qp function


# function to show simulated and observed results
graph_lisem_simulation <- function(
    point_id = NULL, resolution = NULL,
    clean_up = FALSE, run_date = NULL,
    res_dir = NULL) {
  
  # load the package to calculate GOFs
  #library(hydroGOF)
  # based on the point find the subcatch folder
  points <- read_csv("sources/setup/outpoints_description.csv")
  subcatch <- points %>%
    filter(point == point_id) %>%
    filter(cell_size == resolution)
  subcatch_name <- subcatch$subcatch_name
  if (is.null(res_dir)) {
    resdir <- paste0("LISEM_runs/", subcatch_name, "_", resolution, "m/res")
  } else {
    resdir <- paste0("LISEM_runs/", subcatch_name, "_", resolution, "m/", res_dir)
  }
  
  
  # based on the runfile in /res find the date
  resrun_file <- dir(resdir, recursive = TRUE, pattern = ".run$")
  if (is.null(run_date)) {
  ev_date <- ymd(str_remove(resrun_file, ".run"))
  } else {
    ev_date <- ymd(run_date)
  }
  # events
  events <- read_csv("sources/selected_events.csv") %>%
    mutate(ts_start = ymd_hms(event_start),
           ts_end = ymd_hms(event_end)) %>%
    filter(date(ts_start) == ev_date)
  
  point_code <- points %>%
    distinct(point, code) %>%
    filter(!is.na(code))
  
  # load the hydrographs
  hydrograph_files <- dir(resdir, recursive = TRUE, pattern = "^hydrographs_")
  hydr_points <- unlist(str_extract_all(hydrograph_files, "\\d{1,2}"))
  hydrograph_files <- dir(resdir, recursive = TRUE, pattern = "^hydrographs_",
                          full.names = T)
  hydr_list <- vector("list", length = length(hydr_points))
  for (i in seq_along(hydr_points)) {
    hy_names <- readLines(hydrograph_files[i])[2] %>%
      str_split(",", simplify = TRUE) %>%
      str_remove_all(" |#")
    hydr_list[[i]] <- read_csv(hydrograph_files[i], skip = 2) %>%
      rename_with(~hy_names) %>%
      mutate(mins = round(Time * 24 * 60, digits = 5)) %>%
      distinct() #
    if (i != 1) {
      n <- length(hy_names)
      hydr_list[[i]] <- hydr_list[[i]] %>%
        select(hy_names[n-1:n])
    }
  }
  all_hy <- bind_cols(hydr_list) %>%
    mutate(secs = round(mins * 60),
           timestamp = events$ts_start + seconds(secs))
  
  # pivot_longer and assign code
  
  a <- all_hy %>%
    select(timestamp, mins, contains("Qchan")) %>%
    pivot_longer(cols = contains("Qchan"),
                 names_to = "hy_point",
                 values_to = 'Qsim') %>%
    mutate(point = as.numeric(str_remove(hy_point, "Qchan")),
           Qsim = Qsim / 1000) %>%
    left_join(point_code, by = "point")
  
# load rainfall
  ## precipitation 5 minute resolution (GMT+1)
  rain_5min <- read_csv("data/processed_data/neerslag/KNMI_rain_5min.csv")
  
  wdir <- paste0("LISEM_runs/", subcatch_name, "_5m/maps/")
    
  # map2asc
  map2asc(map_in = "ID.map",
         map_out = "rain_ID.asc",
         sub_dir = wdir)
  
    # load rain id's from discharge
    rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
    id <- as.vector(rainIDs)
    freq <- as_tibble(table(id)) %>%
      mutate(id_nm = paste0("gauge_", id))
    
    rain_sub <- rain_5min %>%
      select(timestamp, all_of(freq$id))  %>%
      pivot_longer(cols = -timestamp,
                   values_to = "P",
                   names_to = "id") %>%
      left_join(freq, by = "id") %>%
      mutate(Ptmp = P * n) %>%
      group_by(timestamp) %>%
      summarize(P = round(sum(Ptmp) / sum(n), digits = 2)) %>%
      mutate(point = point_id) 
    
  rain <- map2_dfr(events$ts_start, events$ts_end,
                   ~filter(rain_sub, timestamp >= .x & timestamp <= .y)) %>%
    left_join(events, join_by(
      closest(timestamp <= ts_end)
    )) %>%
    mutate(ev_name = as.character(date(ts_start)))
  
  # load q obs for points and date
  # add discharge
  q_obs <- read_csv("data/processed_data/obs_discharge/observed_discharge_high_res.csv") %>%
    left_join(events, b = c("ev_num", "use")) %>%
    mutate(ev_name = as.character(date(ts_start))) %>%
    filter(!is.na(event_start))

  # make figures for both subcatch
    p <- filter(rain, point == point_id) %>%
      filter(ev_num %in% events$ev_num) %>%
      mutate(mins = as.numeric((timestamp - timestamp[1]) / 60))
    q <- filter(q_obs, point == point_id)%>%
      filter(ev_num %in% events$ev_num) %>%
      mutate(mins = as.numeric((timestamp - timestamp[1]) / 60))
    
    #gofs
    # we calculate the gofs for each observation point and then take the average.
    # alternative approach is to combine the all points to one 'series' and calculate the GOFS over that.
    gof_dat <- a %>%
      select(timestamp, code, Qsim) %>%
      left_join(q, by = c("timestamp", "code"))
    
   cs <- unique(gof_dat$code)
   
   nses <- vector("numeric", length = length(cs))
   kges <- vector("numeric", length = length(cs))
   for (i in seq_along(cs)) {
     dat <- gof_dat %>%
       filter(code == cs[i])
     nses[i] <- round(NSE(dat$Qsim, dat$Q), digits = 2)
     kges[i] <- round(KGE(dat$Qsim, dat$Q), digits = 2)
   }
   
   nse <- round(mean(nses, na.rm = TRUE), digits = 2)
   kge <- round(mean(kges, na.rm = TRUE), digits = 2)
   
   
    # plot
    # axis constants
    q_max_round <- ceiling(max(c(q$Q, a$Qsim), na.rm = TRUE))
    p_max       <- max(p$P, na.rm = TRUE)
    k           <- q_max_round / (p_max * 1.2)
    y_top       <- q_max_round * 2
    
    # plot regular and inverted y-axis
    ggplot() +
      geom_linerange(data = p, aes(x = timestamp, ymin = y_top,
                                   ymax = y_top - P * k)) +
      #geom_ribbon(aes(ymin = qmin, ymax = qmax), fill = "grey60", alpha = 0.3) +
      geom_line(data = q, aes(x = timestamp, y = Q, color = code, linetype = "Qobs"), linewidth = 0.3) +
      geom_line(data = a, aes(x = timestamp, y = Qsim, color = code, linetype = "Qsim"), linewidth = 0.3) +
      # geom_line(aes(y = sel_run),           colour = "red", linetype = "dashed", linewidth = 0.8) +
      # axis
      scale_y_continuous(
        name     = "Discharge (m³ s⁻¹)",
        limits   = c(0, y_top),
        sec.axis = sec_axis(
          ~ (y_top - .) / k,
          name = "Precipitation (mm)"
        ),
        expand = c(0,0)) +
      scale_x_datetime(date_breaks = "3 hours", date_labels = "%H %M",
                       name = "Time of day", expand = c(0,0),
                       # sec.axis = sec_axis(
                       #   ~ . -events$ts_start,
                       #   name = "minutes in event",
                       #   labels = scales::label_timespan(unit = "mins"))
                       ) +
      scale_linetype_manual(values = c("Qobs" = "solid", "Qsim" = "dashed")) +
      labs(color = "Meetpunt", 
           linetype = "",
           title = paste0(subcatch_name, " - ", ev_date, " - ", resolution , "m : NSE = ", nse, ", KGE = ", kge)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.1),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) 
    
    
    ggsave(paste0("images/simulations/sim_", subcatch_name, "_", resolution, "m_", resrun_file, ".png"))
  
 
if (clean_up == TRUE) {
  # remove res folder
  unlink(resdir, recursive = TRUE)
  # make new res folder  
  dir.create(resdir)
}

} # end function graph_lisem_simulation

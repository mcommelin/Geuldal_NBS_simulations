# function to compare temporal resolutions of precipitation

# id.map to catchment size - is now included in standard db script
#wdir <- "LISEM_runs/Watervalderbeek_5m/maps/"
#ev_date <- "2023-06-22"


subcatch_rain_compare <- function(wdir = NULL,
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
  
  ## precipitation 5 minute resolution
  rain_5min <- read_csv("data/raw_data/neerslag/KNMI_rain_5min.csv")
  
  # make loop over subcatch
  rain_sub <- vector("list", length = length(points_id))
  for (i in seq_along(points_id)) {
    point_id <- points_id[i]
    # select subcatchment
    subcatch <- points %>%
      filter(point == point_id) %>%
      filter(cell_size == 5)
    subcatch_name <- subcatch$subcatch_name
    wdir <- paste0("LISEM_runs/", subcatch_name, "_5m/maps/")
    
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
      mutate(point = point_id,
             timestamp = timestamp - minutes(5)) # correct KNMI timestamp now data is for the coming 5 minutes
    
  }
  rain_sub <- bind_rows(rain_sub)
  
  rain <- map2_dfr(events$ts_start, events$ts_end,
                   ~filter(rain_sub, timestamp >= .x & timestamp <= .y)) %>%
    left_join(events, join_by(
      closest(timestamp <= ts_end)
    )) %>%
    mutate(ev_name = as.character(date(ts_start)))
  
  # add discharge
  # first make 'dat' in Q_measurements_Geuldal
  q_obs <- read_csv("data/processed_data/obs_discharge/observed_discharge_high_res.csv") %>%
    left_join(events, b = c("ev_num", "use")) %>%
    mutate(timestamp = timestamp - minutes(60), # correct to GMT from GMT+1
           ev_name = as.character(date(ts_start)))
  
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
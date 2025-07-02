#' Calculations of discharge based on cross-sections etc.

# 1. Initialization ---------------------------------------------------------------


# 2. Load data --------------------------------------------------------------------

# load selected events  
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use != "none")

# load own cross-section data
cs <- read_csv("data/cross_sections_streams.csv") %>%
  mutate(cs_elev = cs_depth * -1 + ceiling(max(cs_depth))) %>%
  arrange(cs_length)

# load point info
Q_pars <- read_csv("sources/height_to_Q_mannings.csv")

# load data and filter for only for the selected events and measurement points
# load wh data
h_data <- q_all %>% # load data from file 'dicharge_rain_selection.R
  filter(code %in% Q_pars$code)
h_data <- map2_dfr(events$ts_start, events$ts_end,
                   ~filter(h_data, timestamp >= .x & timestamp <= .y))
# measured q's y waterboard
# load data from file 'dicharge_rain_selection.R
qwb <- map2_dfr(events$ts_start, events$ts_end,
                ~filter(qall, timestamp >= .x & timestamp <= .y))

# 3. All required functions -------------------------------------------------------

# these functions are made by Claude AI and adjusted where needed.

# area and wetted perimeter circular culvert
calc_culvert_area <- function(wh, D, return_par = "A") {

  # Handle to high water levels
  if (wh >= D) {
    wh <-  D
  }
  
  
  # Calculate central angle theta
  cos_half_theta <- 1 - 2*wh/D
  
  theta <- 2 * acos(cos_half_theta)
  
  # Calculate area
  A <- (D^2/8) * (theta - sin(theta))
  P <- D * theta / 2
  
  # Store results
  if (return_par == "A") {
    return(A)
  } else {
    return(P)
  }
  
}

# area and wetted perimeter irregular cross section
calc_channel_area <- function(stations, elevations, water_level, return_par = "A") {
  # Input validation
  if (length(stations) != length(elevations)) {
    stop("Stations and elevations must have same length")
  }
  
  if (length(stations) < 2) {
    stop("Need at least 2 stations")
  }
  
  # Sort stations and elevations by station order (CRITICAL!)
  sorted_order <- order(stations)
  stations <- stations[sorted_order]
  elevations <- elevations[sorted_order]
  

    # Calculate water depths at each station
    depths <- pmax(0, water_level - elevations)
    
    # Find wetted stations
    wetted <- depths > 0
    
    # Calculate area using trapezoidal rule
    area <- 0
    wetted_perimeter <- 0
    
    for (i in 1:(length(stations) - 1)) {
      station_width <- stations[i + 1] - stations[i]
      
      if (depths[i] > 0 || depths[i + 1] > 0) {
        avg_depth <- (depths[i] + depths[i + 1]) / 2
        segment_area <- station_width * avg_depth
        area <- area + segment_area
        
        # Wetted perimeter calculation
        elev_diff <- elevations[i + 1] - elevations[i]
        segment_length <- sqrt(station_width^2 + elev_diff^2)
        
        if (avg_depth > 0) {
          wetted_perimeter <- wetted_perimeter + segment_length
        }
      }
    }
    
    # Store results
  if (return_par == "A") {
  return(area)
  } else {
    return(wetted_perimeter)
  }
}


# 4. Apply functions to own data --------------------------------------------------

#select qwb data for points that are relevant.
qwb <- qwb %>%
  filter(code %in% c("11.Q.32", "13.Q.34")) %>%
  mutate(point = if_else(code == "11.Q.32", 14, 4)) %>%
  left_join(events, join_by(closest(timestamp >= ts_start))) %>%
  dplyr::select(timestamp, ev_num, code, Q, point, use)

# select watervalderbeek and eyserbeek for now.
Q_pars <- Q_pars %>%
  filter(river %in% c("Watervalderbeek", "Eyserbeek", "Gulp")) %>%
  filter(code != "WATE_WATHTE_001") # remove the buffer data because we dont have a good calculation for now

#adjust the cross-section depth to elevation
cs <- cs %>%
  left_join(Q_pars, by = "code") %>%
  group_by(code) %>%
  mutate(elev = max(cs_depth) + bed_lvl - cs_depth)

h_data2 <- h_data %>%
  left_join(Q_pars, by = "code") %>%
  left_join(events, join_by(closest(timestamp >= ts_start)))

# loop over locations to calculate q
locs <- Q_pars$code
hdat <- vector("list", length = length(locs))

for (i in seq_along(locs)) {
  # cross section info
  cs_loc <- cs %>%
    filter(code == locs[i])
  stats <- cs_loc$cs_length
  elev <- cs_loc$elev
  
  pars <- Q_pars %>% filter(code == locs[i])
  eq <- pars$equation
  
  # calculate discharge for cross-section
  hdat[[i]] <- h_data2 %>%
    filter(code == locs[i]) %>%
    rowwise() %>%
    mutate(A = calc_channel_area(stations = stats, elev = elev,
                              water_level = wh, return_par = "A"),
           P = calc_channel_area(stations = stats, elev = elev,
                                 water_level = wh, return_par = "P"),
           R = A/P,
           Q = 1/pars$n * A * R^(2/3) * sqrt(pars$S))
    
  
  if (eq == "tube") {
  # calculate discharge for culvert at instroom Pletsmolen
  nt <- 0.013 # typical Mannings n for concrete culvert
  St <- (0.07 + 0.0047) / 2 # slope estimate at inlet culvert, bit of trial and error.
  hdat[[i]] <- hdat[[i]] %>%
    filter(code == locs[i]) %>%
    rowwise() %>%
    mutate(At = calc_culvert_area(wh = wh-bed_lvl, D = pars$b, return_par = "A"),
           Pt = calc_culvert_area(wh = wh-bed_lvl, D = pars$b, return_par = "P"),
           Rt = At/Pt,
           Qt = 1/nt * At * Rt^(2/3) * sqrt(St))
  # make a comparison between the culvert and channel calculations of Q
  ggplot(hdat[[i]]) +
    geom_point(aes(x = timestamp, y = Q, color = "Q-stream")) +
    geom_point(aes(x = timestamp, y = Qt, color = "Q-culvert")) +
    facet_wrap(~ ev_num, scales = "free", nrow = 3) +
    theme_classic() +
    labs(title = "comparison Q crosssection and Q culvert - Pletsmolen",
         color = "",
         y = "Q (m3/s)") +
    scale_color_manual(values = c("Q-stream" = "blue", "Q-culvert" = "grey"))
  ggsave(paste0("images/discharge_compare_watervalderbeek.png"))
  
  hdat[[i]] <- hdat[[i]] %>%
    mutate(Q = (Q + Qt) / 2)
  
  }

}

hdat <- bind_rows(hdat) %>%
  dplyr::select(timestamp, ev_num, code, Q, use, point)

dat <- bind_rows(hdat, qwb) %>%
  filter(use != "control") # remove the control event

subcatch <- unique(dat$point)

for (j in seq_along(subcatch)) {
a <- dat %>%
  filter(point == subcatch[j])
  
  
ggplot(a) +
  geom_line(aes(x = timestamp, y = Q, color = code)) +
  facet_wrap(~ ev_num, scales = "free", nrow = 3) +
  labs(color = "Meetpunt") +
  theme_classic()
ggsave(paste0("images/discharge_wh_", subcatch[j], ".png"))

}

c <- c %>%
  mutate(timestamp = timestamp - minutes(65))

test <- full_join(a, c, by = c("timestamp", "ev_num")) %>%
  filter(!is.na(Q) & P != 0.00)

ggplot(c) +
  geom_bar(aes(x = timestamp, y = P), stat = "identity") +
  #geom_line(aes(x = timestamp, y = Q, color = code)) +
  facet_wrap(~ ev_num, scales = "free", nrow = 3) +
  labs(color = "Meetpunt") +
  theme_classic()



# 5. Measurements with flow meter -------------------------------------------------
#' Based on the book 'Hydrometry' by Boiten.
#' 
#' Velocity measurement based on 1 point method (p82).
#' Assuming that the waterdepth at .6 of the depth is about 
#' the mean.
#' 
#' The following calculation of the discharge is done with the
#' mid section method (p85). Since the cross section bins are not equally spaced.
#'
#'The flow speed is calculated from the equations provided with the flow meter
#'v = velocity of water (m/s)
#'n = number of revolutions per second
#'rotor 2: 
#'n < 5.00 => v = 0.1034 * n + 0.013
#'n > 5.00 => v = 0.1022 * n + 0.019
#'rotor 3:
#'n < 0.69 => v = 0.2330 * n + 0.019
#'n > 0.69 => v = 0.2520 * n + 0.006

# Eyserbeek meetgoot snelle som

# sectie 1 brede deel voor brug
vp1 <- (96/30) * 0.1304 + 0.013
vp2 <- (117.2/30) * 0.1304 + 0.013
vp3 <- (88.2/30) * 0.1304 + 0.013
vp4 <- (91.5/30) * 0.1304 + 0.013
vp5 <- (74/30) * 0.1304 + 0.013
vp6 <- (65/30) * 0.1304 + 0.013

qp1 <- vp1 * 0.15 * 0.5 * (0.6 + 0.5)
qp2 <- vp2 * 0.17 * 0.5 * (0.5 + 0.5)
qp3 <- vp3 * 0.16 * 0.5 * (0.5 + 0.5)
qp4 <- vp4 * 0.18 * 0.5 * (0.5 + 0.5)
qp5 <- vp5 * 0.21 * 0.5 * (0.5 + 0.5)
qp6 <- vp6 * 0.12 * 0.5 * (0.5 + 0.5)

qsum <- (qp1 + qp2 + qp3 + qp4 + qp5 + qp6)

# sectie 2 smalle deel onder brug
a2 <- (1.08 - 0.2) * 0.22
v1 <- (111.3/30) * 0.2520 + 0.006
v2 <- (97/30) * 0.2520 + 0.006
v3 <- (103.6/30) * 0.2520 + 0.006

vmean <- (v1 + v2 + v3) / 3
Q2 <- a2 * vmean

# Mannings flow calculation
n = 0.1
b = 2.0
h = 0.3
S = 0.007

eq_mannings <- function(n = 0.1,
                        b = 1.0,
                        h = 0.1,
                        S = 0.001) {
  R = b + 2 * h
  A = b * h
  
  Q = (1 / n) * A * R^(2 / 3) * S^(1 / 2)
  return(Q)
}

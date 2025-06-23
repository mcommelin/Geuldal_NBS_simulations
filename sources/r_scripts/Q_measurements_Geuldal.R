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
calc_culvert_area <- function(wh, D) {

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
  
  return(data.frame(
    area = A,
    wetted_perimeter = P
    ))
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


# Vectorized discharge calculation
calc_discharge_vectorized <- function(stations, elevations, water_levels, slope, manning_n) {
  # Get hydraulic properties for all water levels
  hydraulic_props <- calc_channel_area_vectorized(stations, elevations, water_levels)
  
  # Calculate discharge using Manning's equation
  # Q = (1/n) * A * R^(2/3) * S^(1/2)
  discharges <- ifelse(hydraulic_props$area > 0 & 
                         hydraulic_props$hydraulic_radius > 0 & 
                         slope > 0,
                       (1/manning_n) * hydraulic_props$area * 
                         (hydraulic_props$hydraulic_radius^(2/3)) * (slope^0.5),
                       0)
  
  velocities <- ifelse(hydraulic_props$area > 0, 
                       discharges / hydraulic_props$area, 0)
  
  froude_numbers <- ifelse(hydraulic_props$hydraulic_radius > 0, 
                           velocities / sqrt(9.81 * hydraulic_props$hydraulic_radius), 0)
  
  return(data.frame(
    water_level = water_levels,
    discharge = discharges,
    area = hydraulic_props$area,
    hydraulic_radius = hydraulic_props$hydraulic_radius,
    velocity = velocities,
    froude_number = froude_numbers,
    wetted_perimeter = hydraulic_props$wetted_perimeter,
    top_width = hydraulic_props$top_width,
    max_depth = hydraulic_props$max_depth
  ))
}

# Simple vectorized function that just returns discharge values
calc_discharge_simple <- function(stations, elevations, water_levels, slope, manning_n) {
  result <- calc_discharge_vectorized(stations, elevations, water_levels, slope, manning_n)
  return(result$discharge)
}


## 3.3 Example cross section stream -------------------------------------------
# Example usage with realistic channel data and discharge calculations
# Your actual data
stations <- c(1.10, 1.40, 1.60, 1.80, 2.60, 3.60, 3.10, 4.10, 4.70, 6.15, 6.30)
elevations <- c(3.00, 1.05, 1.00, 0.65, 0.67, 0.75, 0.67, 0.77, 0.90, 1.14, 3.00)
water_levels <- seq(0.7, 1.5, by = 0.05)  # Start from 0.7 to ensure water in channel

# Channel parameters for discharge calculation
channel_slope <- 0.0043  # 0.2% slope (typical for natural channels)
manning_n <- 0.045      # Manning's roughness coefficient (natural channel)

# Show Manning's n reference
manning_roughness_guide()

# Calculate stage-discharge curve
stage_discharge_curve <- calc_stage_discharge_curve(stations, elevations, water_levels, 
                                                    channel_slope, manning_n)

# Display results
cat("\nStage-Discharge Relationship:\n")
print(stage_discharge_curve)

# Plot comprehensive analysis
plot_discharge_analysis(stage_discharge_curve, "Channel Hydraulic Analysis")

# Plot cross-section with some water levels
plot_cross_section(stations, elevations, c(0.8, 1.0, 1.2), 
                   "Channel Cross-section with Water Levels")

# Example calculations for specific conditions
cat("\nExample Discharge Calculations:\n")
cat("Water Level 1.0m: Discharge =", round(interpolate_discharge(1.0, stage_discharge_curve), 3), "m³/s\n")
cat("Water Level 1.2m: Discharge =", round(interpolate_discharge(1.2, stage_discharge_curve), 3), "m³/s\n")

# Find water level for specific discharge
target_discharge <- 2.0  # m³/s
corresponding_level <- find_water_level(target_discharge, stage_discharge_curve)
cat("For discharge of", target_discharge, "m³/s, water level =", round(corresponding_level, 3), "m\n")

# Detailed calculation for one water level
wl_example <- 1.1
detailed_result <- calc_discharge(stations, elevations, wl_example, channel_slope, manning_n)
cat("\nDetailed results for water level", wl_example, "m:\n")
cat("  Discharge:", round(detailed_result$discharge, 3), "m³/s\n")
cat("  Area:", round(detailed_result$area, 3), "m²\n")
cat("  Velocity:", round(detailed_result$velocity, 3), "m/s\n")
cat("  Hydraulic Radius:", round(detailed_result$hydraulic_radius, 3), "m\n")
cat("  Froude Number:", round(detailed_result$froude_number, 3), "\n")
cat("  Flow Type:", ifelse(detailed_result$froude_number < 1, "Subcritical", "Supercritical"), "\n")




## 3.4 Example culvert --------------------------------------------------------
# Example usage:
# Define your channel data
stations <- c(0, 2, 4, 6, 8, 10, 12)
elevations <- c(2.0, 1.5, 1.0, 0.8, 1.0, 1.5, 2.0)
culvert_diameter <- 0.8  # 80 cm
culvert_invert <- 0.8    # elevation of culvert bottom
slope <- 0.002
manning_n <- 0.035

# Generate comparison
water_levels <- seq(0.9, 2.0, by = 0.05)
comparison <- calc_stage_discharge_comparison(stations, elevations, water_levels,
                                              culvert_diameter, slope, manning_n,
                                              culvert_invert, "square_edge")

# Plot results
plot_discharge_comparison(comparison)

# Print some results
print("Sample calculations:")
print(comparison[1:10, ])



# 4. Apply functions to own data --------------------------------------------------

#select qwb data for points that are relevant.
qwb <- qwb %>%
  filter(code %in% c("11.Q.32", "13.Q.34")) %>%
  mutate(point = if_else(code == "11.Q.32", 14, 4)) %>%
  left_join(events, join_by(closest(timestamp >= ts_start))) %>%
  rename(q = Q) %>%
  dplyr::select(timestamp, ev_num, code, q, point)

# select watervalderbeek and eyserbeek for now.
Q_pars <- Q_pars %>%
  filter(river %in% c("Watervalderbeek", "Eyserbeek", "Gulp"))

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
  
  if (eq == "manning" | eq == "both") {
  # calculate discharge for cross-section
  hdat[[i]] <- h_data2 %>%
    filter(code == locs[i]) %>%
    rowwise() %>%
    mutate(A = calc_channel_area(stations = stats, elev = elevations,
                              water_level = wh, return_par = "A"),
           P = calc_channel_area(stations = stats, elev = elevations,
                                 water_level = wh, return_par = "P"))
  
  } else if (eq == "tube" | eq == "both") {
  # calculate discharge for culvert
  culvert_type <- "groove_end"
    
  hdat[[i]] <- h_data2 %>%
    filter(code == locs[i]) %>%
    mutate(q = calc_inlet_control_discharge_simple(headwater_depths = wh-bed_lvl, culvert_diameter = pars$b,
                                                   inlet_type = culvert_type))
  }

}

hdat <- bind_rows(hdat) %>%
  dplyr::select(timestamp, ev_num, code, q, point)

dat <- bind_rows(hdat, qwb)

subcatch <- unique(dat$point)

for (j in seq_along(subcatch)) {
a <- dat %>%
  filter(point == subcatch[j])
  
  
ggplot(b) +
  geom_line(aes(x = timestamp, y = wh-bed_lvl, color = code)) +
  facet_wrap(~ ev_num, scales = "free") +
  theme_classic()
ggsave(paste0("images/discharge_wh_", subcatch[j], ".png"))

}


# make figures for eyserbeek and watervalderbeek.
combos <- expand.grid(
  point = c(10, 14),
  event = events$ts_start,
  stringsAsFactors = FALSE
)

i = 7

dat <- qh_data %>%
  filter(point == 10) %>%
  filter(timestamp >= events$ts_start[4] & timestamp <= events$ts_end[4]) %>%
  
  
  #
  ggplot(dat) + geom_point(aes(x = timestamp, y = h, color = code))

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

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


# Station-Elevation Method for Irregular Channel Cross-sections

# Main function to calculate cross-sectional area
calc_channel_area <- function(stations, elevations, water_level) {
  # stations = horizontal distances (chainage) from left bank
  # elevations = bed elevations at each station
  # water_level = water surface elevation
  
  # Input validation
  if (length(stations) != length(elevations)) {
    stop("Stations and elevations must have same length")
  }
  
  if (length(stations) < 2) {
    stop("Need at least 2 stations")
  }
  
  # Sort stations and elevations by station order
  sorted_order <- order(stations)
  stations <- stations[sorted_order]
  elevations <- elevations[sorted_order]
  
  # Calculate water depths at each station
  depths <- pmax(0, water_level - elevations)
  
  # Find wetted stations (where depth > 0)
  wetted <- depths > 0
  
  if (sum(wetted) == 0) {
    return(list(area = 0, wetted_perimeter = 0, top_width = 0))
  }
  
  # Calculate area using trapezoidal rule
  area <- 0
  wetted_perimeter <- 0
  
  for (i in 1:(length(stations) - 1)) {
    station_width <- stations[i + 1] - stations[i]
    
    # Only calculate if at least one station is wetted
    if (depths[i] > 0 || depths[i + 1] > 0) {
      # Average depth for this segment
      avg_depth <- (depths[i] + depths[i + 1]) / 2
      
      # Add to total area
      segment_area <- station_width * avg_depth
      area <- area + segment_area
      
      # Calculate wetted perimeter for this segment
      elev_diff <- elevations[i + 1] - elevations[i]
      segment_length <- sqrt(station_width^2 + elev_diff^2)
      
      # Only add to wetted perimeter if segment is actually wetted
      if (avg_depth > 0) {
        wetted_perimeter <- wetted_perimeter + segment_length
      }
    }
  }
  
  # Ensure wetted_perimeter is not zero or negative
  wetted_perimeter <- max(wetted_perimeter, 0)
  
  # Calculate top width (water surface width)
  wetted_stations <- stations[wetted]
  top_width <- ifelse(length(wetted_stations) > 0, 
                      max(wetted_stations) - min(wetted_stations), 0)
  
  # Calculate hydraulic radius (with safety check)
  hydraulic_radius <- ifelse(wetted_perimeter > 0 && area > 0, 
                             area / wetted_perimeter, 0)
  
  return(list(
    area = area,
    wetted_perimeter = wetted_perimeter,
    top_width = top_width,
    hydraulic_radius = hydraulic_radius,
    max_depth = max(depths)
  ))
}

# Vectorized version of discharge calculation functions

# Vectorized channel area calculation
calc_channel_area_vectorized <- function(stations, elevations, water_levels) {
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
  
  # Initialize result vectors
  n_levels <- length(water_levels)
  areas <- numeric(n_levels)
  wetted_perimeters <- numeric(n_levels)
  top_widths <- numeric(n_levels)
  hydraulic_radii <- numeric(n_levels)
  max_depths <- numeric(n_levels)
  
  # Loop through each water level
  for (j in 1:n_levels) {
    water_level <- water_levels[j]
    
    # Calculate water depths at each station
    depths <- pmax(0, water_level - elevations)
    
    # Find wetted stations
    wetted <- depths > 0
    
    if (sum(wetted) == 0) {
      areas[j] <- 0
      wetted_perimeters[j] <- 0
      top_widths[j] <- 0
      hydraulic_radii[j] <- 0
      max_depths[j] <- 0
      next
    }
    
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
    
    # Safety checks
    wetted_perimeter <- max(wetted_perimeter, 0)
    
    # Calculate other properties
    wetted_stations <- stations[wetted]
    top_width <- ifelse(length(wetted_stations) > 0, 
                        max(wetted_stations) - min(wetted_stations), 0)
    
    hydraulic_radius <- ifelse(wetted_perimeter > 0 && area > 0, 
                               area / wetted_perimeter, 0)
    
    # Store results
    areas[j] <- area
    wetted_perimeters[j] <- wetted_perimeter
    top_widths[j] <- top_width
    hydraulic_radii[j] <- hydraulic_radius
    max_depths[j] <- max(depths)
  }
  
  return(data.frame(
    water_level = water_levels,
    area = areas,
    wetted_perimeter = wetted_perimeters,
    top_width = top_widths,
    hydraulic_radius = hydraulic_radii,
    max_depth = max_depths
  ))
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



# Function to calculate area for multiple water levels
calc_stage_area_curve <- function(stations, elevations, water_level_range) {
  results <- data.frame(
    water_level = water_level_range,
    area = numeric(length(water_level_range)),
    wetted_perimeter = numeric(length(water_level_range)),
    top_width = numeric(length(water_level_range)),
    hydraulic_radius = numeric(length(water_level_range)),
    max_depth = numeric(length(water_level_range))
  )
  
  for (i in 1:length(water_level_range)) {
    calc_result <- calc_channel_area(stations, elevations, water_level_range[i])
    results$area[i] <- calc_result$area
    results$wetted_perimeter[i] <- calc_result$wetted_perimeter
    results$top_width[i] <- calc_result$top_width
    results$hydraulic_radius[i] <- calc_result$hydraulic_radius
    results$max_depth[i] <- calc_result$max_depth
  }
  
  return(results)
}

# Function to interpolate area for any water level
interpolate_area <- function(target_level, stage_area_data) {
  if (target_level < min(stage_area_data$water_level) || 
      target_level > max(stage_area_data$water_level)) {
    warning("Target level outside data range - extrapolating")
  }
  
  approx(stage_area_data$water_level, stage_area_data$area, 
         target_level, rule = 2)$y
}

# Function to plot cross-section
plot_cross_section <- function(stations, elevations, water_levels = NULL, 
                               main = "Channel Cross-section") {
  
  # Plot the channel bed
  plot(stations, elevations, type = "l", lwd = 2, 
       xlab = "Station (m)", ylab = "Elevation (m)",
       main = main, ylim = c(min(elevations) - 0.5, max(elevations) + 1))
  
  # Fill the channel bed
  polygon(c(stations, rev(stations)), 
          c(elevations, rep(min(elevations) - 0.5, length(stations))), 
          col = "lightgray", border = NA)
  
  # Add the channel outline
  lines(stations, elevations, lwd = 2, col = "black")
  
  # Add water levels if provided
  if (!is.null(water_levels)) {
    colors <- rainbow(length(water_levels))
    for (i in 1:length(water_levels)) {
      wl <- water_levels[i]
      # Only show water level if it intersects the channel
      if (wl > min(elevations) && wl < max(elevations)) {
        abline(h = wl, col = colors[i], lwd = 2, lty = 2)
        text(max(stations) * 0.9, wl + 0.1, 
             paste("WL =", round(wl, 2)), col = colors[i], cex = 0.8)
      }
    }
  }
  
  grid()
}

# Function to calculate discharge using Manning's equation
calc_discharge <- function(stations, elevations, water_level, slope, manning_n) {
  # Get hydraulic properties
  hydraulic_props <- calc_channel_area(stations, elevations, water_level)
  
  # Extract needed values
  area <- hydraulic_props$area
  hydraulic_radius <- hydraulic_props$hydraulic_radius
  
  # Manning's equation: Q = (1/n) * A * R^(2/3) * S^(1/2)
  if (area > 0 && hydraulic_radius > 0 && slope > 0) {
    discharge <- (1/manning_n) * area * (hydraulic_radius^(2/3)) * (slope^0.5)
  } else {
    discharge <- 0
  }
  
  return(list(
    discharge = discharge,
    area = area,
    hydraulic_radius = hydraulic_radius,
    velocity = ifelse(area > 0, discharge / area, 0),
    froude_number = ifelse(hydraulic_radius > 0, 
                           (discharge / area) / sqrt(9.81 * hydraulic_radius), 0)
  ))
}

# Function to create stage-discharge curve
calc_stage_discharge_curve <- function(stations, elevations, water_level_range, 
                                       slope, manning_n) {
  results <- data.frame(
    water_level = water_level_range,
    discharge = numeric(length(water_level_range)),
    area = numeric(length(water_level_range)),
    velocity = numeric(length(water_level_range)),
    hydraulic_radius = numeric(length(water_level_range)),
    froude_number = numeric(length(water_level_range))
  )
  
  for (i in 1:length(water_level_range)) {
    discharge_result <- calc_discharge(stations, elevations, water_level_range[i], 
                                       slope, manning_n)
    results$discharge[i] <- discharge_result$discharge
    results$area[i] <- discharge_result$area
    results$velocity[i] <- discharge_result$velocity
    results$hydraulic_radius[i] <- discharge_result$hydraulic_radius
    results$froude_number[i] <- discharge_result$froude_number
  }
  
  return(results)
}

# Function to interpolate discharge for any water level
interpolate_discharge <- function(target_level, stage_discharge_data) {
  if (target_level < min(stage_discharge_data$water_level) || 
      target_level > max(stage_discharge_data$water_level)) {
    warning("Target level outside data range - extrapolating")
  }
  
  approx(stage_discharge_data$water_level, stage_discharge_data$discharge, 
         target_level, rule = 2)$y
}

# Function to find water level for a given discharge
find_water_level <- function(target_discharge, stage_discharge_data) {
  if (target_discharge < min(stage_discharge_data$discharge) || 
      target_discharge > max(stage_discharge_data$discharge)) {
    warning("Target discharge outside data range - extrapolating")
  }
  
  approx(stage_discharge_data$discharge, stage_discharge_data$water_level, 
         target_discharge, rule = 2)$y
}

# Enhanced plotting function for discharge analysis
plot_discharge_analysis <- function(stage_discharge_data, 
                                    main_title = "Hydraulic Analysis") {
  
  par(mfrow = c(2, 3))
  
  # Stage-Discharge curve
  plot(stage_discharge_data$water_level, stage_discharge_data$discharge, 
       type = "l", lwd = 2, col = "blue",
       xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
       main = "Stage-Discharge Curve")
  grid()
  
  # Stage-Area curve
  plot(stage_discharge_data$water_level, stage_discharge_data$area, 
       type = "l", lwd = 2, col = "red",
       xlab = "Water Level (m)", ylab = "Area (m²)",
       main = "Stage-Area Curve")
  grid()
  
  # Stage-Velocity curve
  plot(stage_discharge_data$water_level, stage_discharge_data$velocity, 
       type = "l", lwd = 2, col = "green",
       xlab = "Water Level (m)", ylab = "Velocity (m/s)",
       main = "Stage-Velocity Curve")
  grid()
  
  # Discharge vs Area
  plot(stage_discharge_data$area, stage_discharge_data$discharge, 
       type = "l", lwd = 2, col = "purple",
       xlab = "Area (m²)", ylab = "Discharge (m³/s)",
       main = "Discharge vs Area")
  grid()
  
  # Froude Number
  plot(stage_discharge_data$water_level, stage_discharge_data$froude_number, 
       type = "l", lwd = 2, col = "orange",
       xlab = "Water Level (m)", ylab = "Froude Number",
       main = "Froude Number vs Stage")
  abline(h = 1, lty = 2, col = "red")  # Critical flow line
  grid()
  
  # Velocity vs Discharge
  plot(stage_discharge_data$discharge, stage_discharge_data$velocity, 
       type = "l", lwd = 2, col = "brown",
       xlab = "Discharge (m³/s)", ylab = "Velocity (m/s)",
       main = "Velocity vs Discharge")
  grid()
  
  par(mfrow = c(1, 1))
}



# Culvert Inlet Control Discharge Calculations
# Based on FHWA HDS-5 methodology for circular concrete culverts

# Function to calculate discharge under inlet control conditions
calc_inlet_control_discharge <- function(headwater_depth, culvert_diameter, 
                                         inlet_type = "square_edge") {
  
  # Convert diameter to meters if needed
  D <- culvert_diameter  # diameter in meters
  HW <- headwater_depth  # headwater depth above culvert invert in meters
  
  # Inlet control coefficients for circular concrete culverts
  # Based on FHWA HDS-5 Table 5-7
  inlet_coefficients <- list(
    square_edge = list(c = 0.0098, Y = 2.0, constant = 0.0398),
    groove_end = list(c = 0.0078, Y = 2.0, constant = 0.0292),
    headwall = list(c = 0.0078, Y = 2.0, constant = 0.0292)
  )
  
  if (!inlet_type %in% names(inlet_coefficients)) {
    stop("Invalid inlet type. Use: square_edge, groove_end, or headwall")
  }
  
  coef <- inlet_coefficients[[inlet_type]]
  
  # Calculate HW/D ratio
  HW_D_ratio <- HW / D
  
  # Inlet control equation for circular culverts (FHWA HDS-5)
  # Form 1: HW/D = c * (Q/AD^0.5)^Y + constant
  # Rearranged to solve for Q:
  # Q = (AD^0.5) * ((HW/D - constant)/c)^(1/Y)
  
  A <- pi * (D/2)^2  # Cross-sectional area
  
  if (HW_D_ratio <= coef$constant) {
    # Very low headwater - use orifice equation
    Q <- 0.6 * A * sqrt(2 * 9.81 * HW)
  } else {
    # Standard inlet control equation
    Q <- (A * sqrt(D)) * ((HW_D_ratio - coef$constant) / coef$c)^(1/coef$Y)
  }
  
  # Calculate velocity and other parameters
  velocity <- Q / A
  velocity_head <- velocity^2 / (2 * 9.81)
  
  return(list(
    discharge = Q,
    velocity = velocity,
    velocity_head = velocity_head,
    HW_D_ratio = HW_D_ratio,
    culvert_area = A,
    inlet_type = inlet_type
  ))
}

# Vectorized inlet control discharge calculation
calc_inlet_control_discharge_vectorized <- function(headwater_depths, culvert_diameter, 
                                                    inlet_type = "square_edge") {
  
  # Convert diameter to meters if needed
  D <- culvert_diameter  # diameter in meters
  
  # Inlet control coefficients for circular concrete culverts
  # Based on FHWA HDS-5 Table 5-7
  inlet_coefficients <- list(
    square_edge = list(c = 0.0098, Y = 2.0, constant = 0.0398),
    groove_end = list(c = 0.0078, Y = 2.0, constant = 0.0292),
    headwall = list(c = 0.0078, Y = 2.0, constant = 0.0292)
  )
  
  if (!inlet_type %in% names(inlet_coefficients)) {
    stop("Invalid inlet type. Use: square_edge, groove_end, or headwall")
  }
  
  coef <- inlet_coefficients[[inlet_type]]
  
  # Initialize result vectors
  n_depths <- length(headwater_depths)
  discharges <- numeric(n_depths)
  velocities <- numeric(n_depths)
  velocity_heads <- numeric(n_depths)
  HW_D_ratios <- numeric(n_depths)
  
  # Cross-sectional area (constant)
  A <- pi * (D/2)^2
  
  # Loop through each headwater depth
  for (i in 1:n_depths) {
    HW <- headwater_depths[i]
    
    # Skip if headwater depth is zero or negative
    if (HW <= 0) {
      discharges[i] <- 0
      velocities[i] <- 0
      velocity_heads[i] <- 0
      HW_D_ratios[i] <- 0
      next
    }
    
    # Calculate HW/D ratio
    HW_D_ratio <- HW / D
    HW_D_ratios[i] <- HW_D_ratio
    
    # Calculate discharge based on headwater conditions
    if (HW_D_ratio <= coef$constant) {
      # Very low headwater - use orifice equation
      Q <- 0.6 * A * sqrt(2 * 9.81 * HW)
    } else {
      # Standard inlet control equation
      Q <- (A * sqrt(D)) * ((HW_D_ratio - coef$constant) / coef$c)^(1/coef$Y)
    }
    
    # Calculate velocity and velocity head
    velocity <- Q / A
    velocity_head <- velocity^2 / (2 * 9.81)
    
    # Store results
    discharges[i] <- Q
    velocities[i] <- velocity
    velocity_heads[i] <- velocity_head
  }
  
  return(data.frame(
    headwater_depth = headwater_depths,
    discharge = discharges,
    velocity = velocities,
    velocity_head = velocity_heads,
    HW_D_ratio = HW_D_ratios,
    culvert_area = A,
    inlet_type = inlet_type,
    stringsAsFactors = FALSE
  ))
}

# Simple vectorized function that just returns discharge values for culverts
calc_inlet_control_discharge_simple <- function(headwater_depths, culvert_diameter, 
                                                inlet_type = "square_edge") {
  result <- calc_inlet_control_discharge_vectorized(headwater_depths, culvert_diameter, inlet_type)
  return(result$discharge)
}

# Function to compare upstream channel vs inlet control discharge
compare_discharge_methods <- function(stations, elevations, water_level, 
                                      culvert_diameter, slope, manning_n,
                                      culvert_invert_elevation, inlet_type = "square_edge") {
  
  # Method 1: Upstream channel with Manning's equation
  upstream_result <- calc_discharge(stations, elevations, water_level, slope, manning_n)
  
  # Method 2: Inlet control
  headwater_depth <- water_level - culvert_invert_elevation
  
  if (headwater_depth <= 0) {
    inlet_result <- list(discharge = 0, velocity = 0, velocity_head = 0, 
                         HW_D_ratio = 0, culvert_area = 0, inlet_type = inlet_type)
  } else {
    inlet_result <- calc_inlet_control_discharge(headwater_depth, culvert_diameter, inlet_type)
  }
  
  # Calculate entrance loss for upstream method
  entrance_loss_coef <- switch(inlet_type,
                               "square_edge" = 0.5,
                               "groove_end" = 0.2,
                               "headwall" = 0.2,
                               0.5)  # default
  
  entrance_loss <- entrance_loss_coef * (upstream_result$velocity^2) / (2 * 9.81)
  corrected_upstream_discharge <- upstream_result$discharge * 
    sqrt(1 - entrance_loss_coef)
  
  return(list(
    upstream = list(
      discharge = upstream_result$discharge,
      corrected_discharge = corrected_upstream_discharge,
      velocity = upstream_result$velocity,
      area = upstream_result$area,
      entrance_loss = entrance_loss,
      entrance_loss_coef = entrance_loss_coef
    ),
    inlet_control = inlet_result,
    headwater_depth = headwater_depth,
    governing_condition = ifelse(inlet_result$discharge < upstream_result$discharge,
                                 "Inlet Control", "Channel Control")
  ))
}

# Stage-discharge comparison for both methods
calc_stage_discharge_comparison <- function(stations, elevations, water_level_range,
                                            culvert_diameter, slope, manning_n,
                                            culvert_invert_elevation, inlet_type = "square_edge") {
  
  results <- data.frame(
    water_level = water_level_range,
    headwater_depth = water_level_range - culvert_invert_elevation,
    upstream_discharge = numeric(length(water_level_range)),
    upstream_corrected = numeric(length(water_level_range)),
    inlet_control_discharge = numeric(length(water_level_range)),
    governing_discharge = numeric(length(water_level_range)),
    governing_method = character(length(water_level_range)),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(water_level_range)) {
    comparison <- compare_discharge_methods(stations, elevations, water_level_range[i],
                                            culvert_diameter, slope, manning_n,
                                            culvert_invert_elevation, inlet_type)
    
    results$upstream_discharge[i] <- comparison$upstream$discharge
    results$upstream_corrected[i] <- comparison$upstream$corrected_discharge
    results$inlet_control_discharge[i] <- comparison$inlet_control$discharge
    results$governing_discharge[i] <- min(comparison$upstream$corrected_discharge,
                                          comparison$inlet_control$discharge)
    results$governing_method[i] <- comparison$governing_condition
  }
  
  return(results)
}

# Plotting function for comparison
plot_discharge_comparison <- function(comparison_data, main = "Discharge Comparison") {
  plot(comparison_data$water_level, comparison_data$upstream_discharge, 
       type = "l", col = "blue", lwd = 2,
       xlab = "Water Level (m)", ylab = "Discharge (m³/s)",
       main = main,
       ylim = c(0, max(comparison_data$upstream_discharge, 
                       comparison_data$inlet_control_discharge, na.rm = TRUE) * 1.1))
  
  lines(comparison_data$water_level, comparison_data$upstream_corrected, 
        col = "green", lwd = 2, lty = 2)
  lines(comparison_data$water_level, comparison_data$inlet_control_discharge, 
        col = "red", lwd = 2)
  lines(comparison_data$water_level, comparison_data$governing_discharge, 
        col = "black", lwd = 3)
  
  legend("topleft", 
         legend = c("Upstream Channel", "Upstream Corrected", "Inlet Control", "Governing"),
         col = c("blue", "green", "red", "black"),
         lty = c(1, 2, 1, 1), lwd = c(2, 2, 2, 3))
  
  grid()
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
  filter(code == "11.Q.32") %>%
  mutate(point = 14) %>%
  left_join(events, join_by(closest(timestamp >= ts_start))) %>%
  rename(q = Q) %>%
  dplyr::select(timestamp, ev_num, code, q, point)

# select watervalderbeek and eyserbeek for now.
Q_pars <- Q_pars %>%
  filter(river == "Watervalderbeek" | river == "Eyserbeek")

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
  stations <- cs_loc$cs_length
  elevations <- cs_loc$elev
  
  pars <- Q_pars %>% filter(code == locs[i])
  eq <- pars$equation
  
  if (eq == "manning" | eq == "both") {
  # calculate discharge for cross-section
  hdat[[i]] <- h_data2 %>%
    filter(code == locs[i]) %>%
    mutate(q = calc_discharge_simple(stations = stations, elevations = elevations,
                              water_level = wh, slope = S, manning_n = n))
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
  
  
ggplot(a) +
  geom_line(aes(x = timestamp, y = q, color = code)) +
  facet_wrap(~ ev_num, scales = "free_x") +
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

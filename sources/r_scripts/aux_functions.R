# function to calculate Q from wh
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

# add a suffix
add_suffix <- function(strings) {
  # Use grepl to check if the string contains an underscore
  sapply(strings, function(x) {
    if (!grepl("_", x)) {
      paste0(x, "_00")
    } else {
      x
    }
  })
}
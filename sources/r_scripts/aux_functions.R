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

# functions to draw from an visualize distributions

plot_distr <- function(n, mean, sd, type = "gaussian", store = TRUE, plot = TRUE) {
  if (type == "gaussian") {
    x <- rnorm(n, mean, sd)
  }
  
  if (type == "gamma") {
    shape <- (mean^2)/(sd^2)
    scale <- (sd^2)/(mean)
    x <- rgamma(n, shape = shape, scale = scale)
  }
  
  if (type == "uniform") {
    x <- runif(n, min = mean, max = sd)
  }
  
  if (type == "logtrans") {
    x <- exp(rnorm(n, mean = mean, sd = sd))
  }
  
  if (type == "lognormal") {
    x <- rlnorm(n, log(mean), log(sd))
  }
  
  if (plot == TRUE) {
    print(min(x))
    hist(x, breaks = 200)
  }
  
  if (store == TRUE) 
  {
    return(x)}
}


# calculate actual vapour pressure based on min and max temperature and relative humidity
vap.pressure <- function(tmin, tmax, rh) {
  e_act = (rh/100) * 0.5* 0.6108 * (exp((17.27*tmin)/(tmin + 237.3))+exp((17.27*tmax)/(tmax + 237.3)))
  return(e_act)
}

#' Give all words in a character column CAPS at start of word.
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# emperical function in SWAP to calculate runoff after rainfall and ponding
# the empirical function:
swap_runoff_emp <- function(r, rexp) {
  1/r * (max(0, (0.7 - 0.2)))^rexp
}

# function to draw random values from the distributions, and correct for 
# upper and lower limits in the model
sample_parameters <- function(params, n, type = "", matrix = "A")
{
  if (type == "QRN") {
    x <- sobol_matrices(N = n, params = params$para, matrices = matrix)
    
    for(i in 1:ncol(x)) {
      dist_sobol <- params$dist[[i]]
      # transform probabilities to one of the three types of distributions
      # gaussian or normal
      if (dist_sobol == "gaussian") {
        x[, i] <- qnorm(x[, i], params$mean[[i]], params$sd[[i]])
      }
      # uniform
      if (dist_sobol == "uniform") {
        x[, i] <- qunif(x[, i], params$lower[[i]], params$upper[[i]])
      }
      if (dist_sobol == "lognormal") {
        x[, i] <- qlnorm(x[, i], log(params$mean[[i]]), log(params$sd[[i]]))
      }
      
    } # end loop transform probabilities
    p <- data.frame(run = seq(1,nrow(x),1)) %>%
      bind_cols(as.data.frame(x)) %>%
      mutate(across(everything(), ~ round(. ,digits = 4)))
  }
  
  if (type == "random") {
    p <- tibble(run = seq(1,n,1))
    for (i in 1:nrow(params)) {
      
      
      x <- plot_distr(n = n, mean = params$mean[i], sd = params$sd[i],
                      type = params$dist[i], store = T, plot = F)
      
      
      p <- p %>%
        mutate(v = x) %>%
        rename_with(~ params$para[i], .cols = v)
    }
  }
  return(p)
}

# boesten 1986 function temperature influence on decomposition
temp_fac_decom <- function(g, t) {
  x = exp(g * (t-20))
  return(x)
}

# mualem van genuchten swap equation for soil moisture
mvg_thetha_swap <- function(tres, tsat, n, a, h) {
  m = 1 - (1/n)
  x = tres + ((tsat - tres) * ((1 + abs(a * h)^n)^-m))
  return(x)
}


# function to calculate an index value for overlap with observation range
range_overlap_index <- function(sim, obs_l, obs_u) {
  x = ifelse(sim > obs_l & sim < obs_u, 1, 0)
  return(x)
}


# function to run OpenLLISEM-pesticide from the command line
# dir = the directory where the runfile is located
# run_file = runfile name (with .run extension!)
# lisem_dir = full path name to Lisem executable
# GUI = show the user interface (default = TRUE)
# wait = should R wait until Lisem is finished? (default = TRUE)

run_lisem <- function(dir, run_file, lisem_dir, GUI = TRUE, wait = TRUE) {
  projwd <- getwd()
  run <- paste0(projwd, "/", dir, run_file)
  if (!GUI) {gui <-  "-ni"} else {gui <- ""}
  command <- paste0(gui, " -r ", run)
  print("Running OpenLISEM...")
  t_s <- Sys.time()
  system2(lisem_dir, command, wait = wait)
  t_e <- Sys.time()
  t_r <- t_e - t_s
  print(paste0("Run finished!"))
  return(t_r)
}


# root mean squared error calculation
rmse_calc <- function(sim, obs) {
  x = (sim - obs)^2
  x2 = sqrt(mean(x, na.rm = T))
  return(x2)
}


# function to calculate mean soil concentration from discrete outpu of SWAP
# for input in OpenLISEM. both the zm and zs concentration are calculated.
# input:
# depths a vector with positive depth values, the bottom on the layer is assumed
#       so 5 mm is the bottom of the layer 0 - 5.
# concs a vector with the corresponding concentration per depth
# zs the thickness of the layer below the mixing layer - usefull when simulation erosion
#   default is 20 mm
# zm the thickness of the mixing layer

ini_conc_OLP <- function(depths = NULL, concs = NULL, zs = 20, zm = NULL) {
  
  # make a tibble from the input
  tab <- tibble(depth = depths,
                conc = concs) %>%
    mutate(layer_z = if_else(is.na(lag(depth)), depth, depth - lag(depth)),
           dc = layer_z * conc)
  # calculate concentration for zm and zs by combining the descrete layers
  
  if (zm < max(depths)) { #zm is smaller than the depth of pesticides in the soil
    full_layers_zm <- floor(zm / tab$layer_z[1])
    left_over_zm <- zm %% tab$layer_z[1] 
    #calculate the mean concentration in the mixing layer
    y <- 0
    if (full_layers_zm > 0) {
      for(l in 1:full_layers_zm) {
        x <- tab$layer_z[l] * tab$conc[l]
        y <- y + x
      }
    }
    y <- y + (left_over_zm * tab$conc[full_layers_zm+1])
    zm_c <- y / zm
    # calculate the mean concentration of the 20 mm soil below the mixing layer
    full_layers_zs <- floor((max(depths) - zm) / tab$layer_z[1])
    # zs_c part of the unfinished layer from zm
    zs_y <- (tab$layer_z[full_layers_zm+1] - left_over_zm) * tab$conc[full_layers_zm+1]
    if (full_layers_zs > 0) {
      for(l in 1:full_layers_zs) {
        zl <- full_layers_zm+1+l
        x <- tab$layer_z[zl] * tab$conc[zl]
        zs_y <- zs_y + x
      }
    }
    zs_c <- zs_y / zs 
    
  } else { # zm is larger than the assumed depth of pesticide conc (20 mm)
    zm_c <- sum(tab$dc) / zm
    zs_c <- 0
  }
  # return the two values in a tibble
  concs <- tibble(zm_c = zm_c, zs_c = zs_c)
  return(concs)
}

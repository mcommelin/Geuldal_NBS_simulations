# Calculate van Genuchten parameters and create SWATRE input tables.

# Initialization --------------------------------------------------------------
# we expect this to run inside the full workflow, so all libraries required are
# loaded already.
soil_landuse_to_swatre <- function(file = "",
                                   swatre_out = "",
                                   do_NBS = FALSE) 
{
  # 1. Calculate params -------------------------------------------------------------
  
  if (do_NBS == TRUE) {
    lutbl <- "lu.tbl"
  } else {
    lutbl <- "lu_nbs.tbl"
  }
  #load the UBC codes including texture, gravel
  ubc_in <- read_csv(file, show_col_types = FALSE)
  # load landuse classes with OM and O depth
  lu_in <- read.table(paste0("sources/setup/calibration/", lutbl))[-1, ] %>%
    select(1, 5, 7) %>%
    rename_with(~ c("lu", "om", "od")) %>%
    mutate(lu = if_else(lu < 10, lu * 100, lu))
  # NBS value go to digit 5 and 6, original landuse to digit 4.
  
  if (DEBUGm) message("Making all soil horizon codes")
  
  # the measured OM values from the field campaign are applied to the upper
  # x centimeters of the A horizon, below that values from the original soil
  # are used.
  
  # In the UBC dataset now A = 1.5, B = 1, C = 0.5 OM where does this come from?
  # TODO verify with literature! from fieldwork akkerbouw = 1.46, not bad!
  
  # adjust O horizons for organic matter related to landuse
  ubc_o <- ubc_in %>%
    filter(str_detect(CODE, "-A")) %>%
    select(-om)
  # combine the landuse and soil data to all possible combinations
  ubc_lu <- expand_grid(UBC = ubc_o$UBC, lu = lu_in$lu) %>%
    mutate(ubclu = UBC + lu) %>%
    left_join(lu_in, by = "lu") %>%
    left_join(ubc_o, by = "UBC") %>%
    select(-UBC, -lu, -depth) %>%
    rename("UBC" = "ubclu") %>%
    rename("depth" = "od") %>%
    mutate(CODE = str_replace(CODE, "-A", "-O")) %>%
    filter(depth != 0)
  
  ubc_all <- bind_rows(ubc_lu, ubc_in) %>%
    mutate(horizon = str_extract(CODE, "-.*$"),
           CODE = paste0(UBC, horizon),
           CODE = str_remove(CODE, "NA")) %>%
    select(-horizon)
  
  ## 1.2 Saxton & Rawls --------------------------------------------------------------
  
  # to apply the S&R calculation we make use of code provided by: rcropmod
  # https://github.com/ldemaz/rcropmod
  # Containing an Apache 2.0 license
  if (DEBUGm) message("pedotransfer.R")
  source("modules/rcropmod/pedotransfer.R")
  
  sr_params <- ubc_all %>%
    mutate(wp = wilt_point(sand, clay, om),
           fc = field_cap(sand, clay, om),
           #thetas = theta_BD(sand, clay, om, gravel), # new function calculating the effect of gravel on porosity
           thetas = theta_s(sand, clay, om), # <= works better for Kelmis!
           bd = bdens(thetas, DF = 1, gravel = gravel/100),
           tex_sum = sand + clay + silt)
  
  ## 1.3 S&R through Rosetta v3 --------------------------------------------------
  
  # with the rosetta model (Zang & Schaap 2017) further parameters for SWATRE
  # are found. To run this we install the rosetta-soil python package inside 
  # a conda environment and the rosettaPTF package in R see also:
  # https://ncss-tech.github.io/rosettaPTF/
  if (DEBUGm) message("doing rosetta")
  
  # we give the 6 available parameters to rosetta in the correct order.
  soildat <- sr_params %>%
    select(sand, silt, clay, bd, fc, wp) %>%
    mutate(sand = sand * 100,
           silt = silt * 100,
           clay = clay * 100)
  
  # these results also contain uncertainty, which we can use for calibration later.  
  rosetta_params <- run_rosetta(soildat)
  
  soil_params <- bind_cols(sr_params, rosetta_params) %>%
    mutate_at(vars(matches("^log10")), ~ 10^.) %>% # recalculate all log10 values
    rename_with(~ str_remove(., "^log10_")) # update names
    # folder LISEM_data/calibration must exist
    # write the soil_params to LISEM_data/calibration
    # here we can adjust many parameters for each variable during testing
    write_csv(soil_params, swatre_out)
  }
  
  ## 1.3b MERGE FIELD MEASUREMENTS KSAT AND PORE ----------------------------------
  

  ## 1.4 theta - h - k table ------------------------------------------------
  # here we can add code to include observed porosity and ksat before making the swatre tables
  
  make_swatre_tables <- function(cal_file = "",
                                 swatre_dir = NULL) 
  {
    # 2. SWATRE tables LISEM-----------------------------------
    if (DEBUGm) message("make_swatre_tables")    
      ## 2.1 theta - h - k table ------------------------------------------------
    #calibration
  
      soil_cal <- read_csv("sources/setup/calibration/calibration_soil.csv", 
                         show_col_types = F) %>%
      select(-description, -cal_comment) %>% 
        pivot_longer(
          cols = -soil,
          names_to = "parameter",
          values_to = "value"
        ) %>%
        separate(parameter, into = c("param", "horizon"), sep = "_") %>%
        mutate(horizon = ifelse(horizon == "BC", "C", horizon)) %>%
        mutate(soil = paste0(soil, "_", horizon),
               param = paste0(param, "_cal")) %>%
        select(-horizon) %>%
        pivot_wider(
          names_from = param,
          values_from = value
        )
      
    #load landuse related ksat calibration
      lu_cal <- read_csv("sources/setup/calibration/calibration_landuse.csv") %>%
        select(landuse, ksat_cal) %>%
        rename("ksat_lu" = "ksat_cal")
      
    soil_params <- read_csv(paste0("sources/setup/calibration/", cal_file), show_col_types = FALSE) %>%
      filter(!is.na(clay)) %>%
      mutate(CODE = str_replace(CODE, "-", "_"),
             horizon = str_extract(CODE, ".$"),
             horizon = ifelse(horizon == "E", "C", horizon),
             horizon = ifelse(horizon == "t", "C", horizon),
             horizon = ifelse(horizon == "B", "C", horizon),
             soil = floor(UBC/ 1000),
             soil = paste0(soil, "_", horizon),
             soil = ifelse(soil == "0_0", "0_O", soil),
             landuse = (UBC %% 1000) / 100) %>%
      left_join(soil_cal, by = "soil") %>%
      left_join(lu_cal, by = "landuse") %>%
      mutate(ksat_lu = if_else(is.na(ksat_lu), 1.0, ksat_lu),
             ksat_cal = ksat_cal * ksat_lu,  #multiply is easier to understand
        alpha_mean = alpha_mean * alpha_cal,
             npar_mean = npar_mean * n_cal,
             Ksat_mean = Ksat_mean * ksat_cal)
    

    tbl_dir <- paste0(swatre_dir, "tables/")
    
    # cleanup /swatre/tables.
    if (dir.exists(tbl_dir)) {
      unlink(tbl_dir, recursive = TRUE)
    }
    dir.create(tbl_dir)
    # for loop making all tables
    for (i in seq_along(soil_params$CODE)) {
      ubc_tbl_n <- soil_params$CODE[i]
    
    # get profile specific params
    alpha <- soil_params$alpha_mean[i]
    theta_r <- soil_params$theta_r_mean[i]
    theta_s <- soil_params$theta_s_mean[i]
    n <- soil_params$npar_mean[i]
    silt <- soil_params$silt[i]
    # if (silt > 0.450) {
    #   alpha = alpha*cal_alpha
    #   n = max(1.1,n*cal_n)
    # }
    #message(silt, " ",alpha,"| ", n)
    m <- 1 - (1/n)
    ks <- soil_params$Ksat_mean[i]

    # theta values between theta_r and theta_s
    ubc_tbl <- tibble(
      theta = seq(from = theta_r + 0.001, to = theta_s,
        length.out = 30)) %>%
      mutate(S = signif((theta - theta_r) / (theta_s - theta_r), digits = 6),
             h = -1/ alpha * (S^(-1/m)-1)^(1/n),
             h = formatC(h, format = "e", digits = 2),
             theta = round(theta, digits = 3),
             k = ks * sqrt(S) * (1 - (1 - S^(1/m))^m)^2,
             k = formatC(k, format = "e", digits = 2)) %>%
      select(theta, h, k)
    
    # write the profile tables
    ubc_file <- paste0(tbl_dir, ubc_tbl_n, ".tbl")
    write.table(ubc_tbl, file = ubc_file, col.names = F,
                row.names = F, sep = " ", quote = F)
    
    # copy the table for impermeable lower soils 97.tbl to the tables folder
    file.copy("sources/setup/swatre/97.tbl", tbl_dir, overwrite = T)

  }


  ## 2.2 update profile.inp ------------------------------------------------------
  
  # load template .inp
  inp <- readLines("sources/setup/swatre/profile_template.inp")
  file <- paste0(swatre_dir, "profile.inp") # input file name
  write(inp, file = file, append = F)
  
  soil_params <-soil_params %>%
    mutate(ubc_soil = str_extract(UBC, "^\\d\\d\\d"),
           horizon = str_extract(CODE, ".$"))
  
  o_soils <- soil_params %>% 
    filter(horizon %in% c("O", "0"))
  
  lower_soils <- soil_params %>%
    anti_join(o_soils, by = "UBC")
  
  ubc <- unique(o_soils$UBC)

  for (i in seq_along(ubc)) {
    # write horizon data
    ubc_n <- ubc[i]
    ubc_s <- o_soils$ubc_soil[i]
    string <- paste0("\n", ubc_n)
    write(string, file = file, append = T)  
    
    low <- lower_soils %>%
      filter(ubc_soil == ubc_s)
    
    ubc_pars <- o_soils %>%
      filter(UBC == ubc_n) %>%
      bind_rows(low)
   
    for (j in seq_along(ubc_pars$CODE)) {
      ubc_tbl_n <- ubc_pars$CODE[j]
      depth <- ubc_pars$depth[j]
      string <- paste0(
        ubc_tbl_n, ".tbl\n",
        depth
      )
      write(string, file = file, append = T) 
    }
    if (ubc_n == 100) {
      depth <- 350
      string <- paste0(
        "140000_C.tbl\n",
        "350"
      )
      write(string, file = file, append = T) 
    }
    if (depth < 350) {
      string <- paste0(
        "97.tbl\n",
        "350"
      )
      write(string, file = file, append = T) 
    }
  
  }
  message("Done.")
} # end function make_swatre_tables()

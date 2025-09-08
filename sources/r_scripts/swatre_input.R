# Calculate van Genuchten parameters and create SWATRE input tables.

# Initialization --------------------------------------------------------------
# we expect this to run inside the full workflow, so all libraries required are
# loaded already.
library(rosettaPTF)

# Calculate params -------------------------------------------------------------
soil_landuse_to_swatre <- function(file = "",
                                   swatre_out = "")
{
#load the UBC codes including texture, gravel, OM
ubc_in <- read_csv(file)

## 1.2 UBS to S&R --------------------------------------------------------------

# to apply the S&R calculation we make use of code provided by: rcropmod
# https://github.com/ldemaz/rcropmod
# Containing an Apache 2.0 license
source("modules/rcropmod/pedotransfer.R")

sr_params <- ubc_in %>%
  mutate(wp = wilt_point(sand, clay, om),
         fc = field_cap(sand, clay, om),
         thetas = theta_s(sand, clay, om),
         bd = bdens(thetas, DF = 1, gravel = gravel/100),
         tex_sum = sand + clay + silt)

## 1.3 S&R through Rosetta v3 --------------------------------------------------

# with the rosetta model (Zang & Schaap 2017) further parameters for SWATRE
# are found. To run this we install the rosetta-soil python package inside 
# a conda environment and the rosettaPTF package in R see also:
# https://ncss-tech.github.io/rosettaPTF/

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

# write the soil_params to LISEM_data/calibration
# here we can adjust many parameters for each variable during testing
write_csv(soil_params, swatre_out)
}
## 1.4 theta - h - k table ------------------------------------------------

make_swatre_tables <- function(cal_file = ""
                               ) 
{

soil_params <- read_csv(paste0("LISEM_data/calibration/", cal_file)) %>%
  filter(!is.na(clay)) %>%
  mutate(CODE = str_replace(CODE, "-", "_"))

# for loop making all tables
for (i in seq_along(soil_params$CODE)) {
ubc_tbl_n <- soil_params$CODE[i]

# get profile specific params
alpha <- soil_params$alpha_mean[i]
theta_r <- soil_params$theta_r_mean[i]
theta_s <- soil_params$theta_s_mean[i]
n <- soil_params$npar_mean[i]
m <- 1 - (1/n)
ks <- soil_params$Ksat_mean[i]


# theta values between theta_r and theta_s
ubc_tbl <- tibble(
  theta = seq(from = theta_r + 0.001, to = theta_s,
    length.out = 15)) %>%
  mutate(h = -1/ alpha * (((theta_s -theta_r) / (theta - theta_r))^(1/m)-1)^(1/n),
         h = formatC(h, format = "e", digits = 2),
         S = signif((theta - theta_r) / (theta_s - theta_r), digits = 3),
         theta = round(theta, digits = 3),
         k = formatC(ks * sqrt(S) * (1 - (1 - S^(1/m))^m)^2, format = "e", digits = 2)) %>%
  select(theta, h, k)

# write the profile tables
ubc_file <- paste0("LISEM_data/swatre/tables/", ubc_tbl_n, ".tbl")
write.table(ubc_tbl, file = ubc_file, col.names = F,
            row.names = F, sep = " ", quote = F)

}


## 1.5 update profile.inp ------------------------------------------------------

# load template .inp
inp <- readLines("LISEM_data/swatre/profile_template.inp")
file <- "LISEM_data/swatre/profile.inp" # input file name
write(inp, file = file, append = F)

ubc <- unique(soil_params$UBC)

for (i in seq_along(ubc)) {

  # write horizon data
  ubc_n <- ubc[i]
  string <- paste0("\n", ubc_n)
  write(string, file = file, append = T)  
  
  ubc_pars <- soil_params %>%
    filter(UBC == ubc_n)
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
} # end function make_swatre_tables()

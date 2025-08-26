# Initializations -------------------



# Prepare input parameters -----------------------------------------------------
input_parameters_OL <- function() {
  var_distributions <- read_csv("LISEM_data/setup/initial_parameters_runoff.csv")
  lu_table <- read_csv("LISEM_data/tables/lu_tbl.csv")
  lu_types <- lu_table$num
  
  # table for variables that are lumped for all landuses
  vars_all <- var_distributions %>%
    filter(lumped_for == "all") %>%
    rename(para = var_name) %>%
    mutate(dist = dis_type) %>%
    select(para, dist, mean, sd, upper, lower, dis_type, cal_include) %>%
    mutate(default = mean)
  # table for landuse specific variables
  a <- var_distributions %>%
    filter(lumped_for == "lu")
  
  # all specific combinations.
  vars_lu <- expand_grid(a$var_name, lu_types) %>%
    rename(var_name = `a$var_name`) %>%
    left_join(var_distributions, by = "var_name") %>%
    mutate(para = str_c(var_name, "_", lu_types)) %>%
    mutate(dist =  dis_type) %>%
    select(para, dist, mean, sd, upper, lower, dis_type, cal_include) %>%
    mutate(default = mean)
  
  # combine the tables to 1 input tabel.
  vars_runoff <- bind_rows(vars_all, vars_lu)
  write_csv(vars_runoff, "LISEM_data/setup/vars_openlisem.csv")
  
  # write runoff_parameters.csv extend later with QRN sample.
  parameters <- vars_runoff %>%
    select(para, default) %>%
    pivot_wider(names_from = "para", values_from = "default")
  # save the result
  write_csv(parameters, "LISEM_data/params/runoff_parameters.csv")
}

# sample quasi random numbers parameters

sample_QRN_sim <- function(file = "", n = 1024, matrix = "A",
                           out_file = "") {
  # sample QRN params
  vars <- read_csv(file)
  fixed <- vars %>%
    filter(cal_include == 0) %>%
    select(para, default) %>%
    pivot_wider(names_from = "para", values_from = "default")
  
  calvars <- vars %>%
    filter(cal_include == 1)
  
  run_params <- sample_parameters(params = calvars,n = n, type = "QRN", matrix = matrix)
   # bind_cols(fixed) %>%
    # add adjustements were needed.
  write_csv(run_params, out_file)
}
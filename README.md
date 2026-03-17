# Geuldal_NBS_simulations
Code to setup a database and simulate the impact of NBS measures on floods in the Geul catchment.
[![CC BY 4.0][cc-by-shield]][cc-by]

For the simulations the OpenLISEM model is used.


This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

# Preparing and running OpenLISEM
The repository only contains code and some tables with parameters and settings. The timeseries and map data are not yet public available. 

To run the code properly the ./spatial_data/ folder is required. Besides that a conda environment with PCRaster and Python installed is needed.

Two options exist: 
- manual simulation, preparing a few subcatchments and exploring simulations on a PC.
- command line / HPC simulation, with a config file and Rscript the simulations are made automatically.

For manual simulations set basic settings in the config_template.yaml and save it as config.yaml. 
The main workflow can be followed with the R script: LISEM_Geuldal_full_workflow.R  

For the command line setup, set basic settings in the hpc_template.yaml and save under a different name.
The OpenLISEM dataset and simulations can be made with the following command:  
`Rscript --vanilla ./sources/r_scripts/hpc_workflow.R [name_hpc_config].yaml`


# Required Software

## OpenLISEM
The OpenLISEM model can be downloaded from: https://github.com/vjetten/openlisem  
For installation etc follow the instruction at: https://github.com/vjetten/openlisem/wiki/Getting-started

## PCRaster
PCRaster is a GIS language which is used to prepare and manupilate input data for OpenLISEM a short guide for installation can be found here: https://github.com/vjetten/openlisem/wiki/Getting-started#1-install-miniconda

Further documentation can be found here: https://pcraster.geo.uu.nl/pcraster/4.4.2/documentation/index.html

## R and RStudio
To run the code in this repository R is required, the IDE Rstudio makes working with R easier.


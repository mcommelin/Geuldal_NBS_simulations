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

Set basic settings in the config_template.yaml and save it as config.yaml. 
The main workflow can be followed with the R script: LISEM_Geuldal_full_workflow.R  

At the moment the workflow is finished untill the setup and evaluation of sub catchments for calibration.
To run the code properly the ./LISEM_data/ and ./data/processed_data/ folders are required.

# Required Software

## OpenLISEM
The OpenLISEM model can be downloaded from: https://github.com/vjetten/openlisem
For installation etc follow the instruction at: https://github.com/vjetten/openlisem/wiki/Getting-started

## PCRaster
PCRaster is a GIS language which is used to prepare and manupilate input data for OpenLISEM a short guide for installation can be found here: https://github.com/vjetten/openlisem/wiki/Getting-started#1-install-miniconda

Further documentation can be found here: https://pcraster.geo.uu.nl/pcraster/4.4.2/documentation/index.html

## R and RStudio
To run the code in this repository R is required, the IDE Rstudio makes working with R easier.


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
Detailed instructions on using this code in this repository can be found in the manual.pdf.  
The repository only contains code and some tables with parameters and settings. The timeseries and map data are available in the spatial_data.zip folder in the releases section.

To run the code properly the ./spatial_data/ folder is required. Besides that a conda environment with PCRaster and Python installed is needed - see the required software below.

Three workflow options are available to prepare OpenLISEM simulations:

**1. Interactive guided run (recommended for new users)**  
Open `Geuldal_NBS.Rproj` in Rstudio and open the script `./LISEM_interactive_workflow.R` 
Source the script and follow the instructions.

**2. Manual (step-by-step)**  
Set basic settings in `config_template.yaml` and save it as `config.yaml`.  
Open `Geuldal_NBS.Rproj` in Rstudio and load `./sources/r_scripts/LISEM_Geuldal_full_workflow.R`. Execute sections with `Ctrl+Enter` for full control over every step.

**3. Command line / HPC**  
Set basic settings in `hpc_template.yaml` and save under a different name.  
The OpenLISEM dataset and simulations can be made with the following command:  
`Rscript --vanilla ./sources/r_scripts/hpc_workflow.R [name_hpc_config].yaml`


# Required Software

## OpenLISEM
The OpenLISEM model can be downloaded from: https://github.com/vjetten/openlisem  
For installation etc follow the instructions at: https://github.com/vjetten/openlisem/wiki/Getting-started

## PCRaster
PCRaster is a GIS language which is used to prepare and manipulate input data for OpenLISEM. A short guide for installation can be found here: https://github.com/vjetten/openlisem/wiki/Getting-started#1-install-miniconda

Further documentation can be found here: https://pcraster.geo.uu.nl/pcraster/4.4.2/documentation/index.html

## R and RStudio
To run the code in this repository R is required, the IDE Rstudio makes working with R easier.


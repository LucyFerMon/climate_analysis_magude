# climate_magude

The following projects conducts an analysis over the climate in Magude since 1980
to evaluate whether there have been any signficant trends that can affect malaria 
in the area and correlates the climate variables with malaria incidence

## Project structure

The project folders are structured in the following way:


* scripts
   * **master_vece.R**: This script calls the project scripts in order. Inside the file you can find different
   packages of scripts to be run depending on the purpose that can be a) calculating sample size,
   b) updating lists of households or c) monitor project progress. Choose the one you like and 
   run the enlisted scripts.  
  * **config_original.R**: This script contains the variables with the credentials to access 
  the differente data sets. Please before running this script, dowload the project 
  in your computer and enter your credentials in this file.
  * **00_prerequisites.R**:  This script contains the needed libraries for this project.
  * **01_reading_data.R**: Reads in data from different sources and reshapes it.
  * **02_plotting.R**: Plots overal trend and seasonality and plots yearly climatic
  conditions to visualize differences
  
* img

Contains all images produced by the R code.

* info_satellites

Contains the maps produced via IRI library from the rainfall data product CHIRPS.
It also contains information on the stations taken into account in this data product

* data

It contains the climate data provided by Arasul and Xinavane Sugar cane plantation.
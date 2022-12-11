# uep232_final_project
Author: Elizabeth Hora
email: Elizabeth.Hora@tufts.edu

## Motivation for Project:
This project is part of the UEP-232 Introduction to GIS course offerred at Tufts University. For my final project, I chose to study tornadoes in the Continental United States.

## Contents:
### final_project_Hora
This R file contains all the analysis I completed in R. I have annotated the code.

### final_project_code
This R project is how I accessed my data and ran my code.

### data
This folder contains all the files needed to reproduce my analysis. It contains the following information:
- FinalProjectHora: This is where I did my work in ArcGIS Pro. I have all my maps and layouts that I did for this project.
- 1950-2021-torn-aspath: Data associated with each tornado recorded by NOAA.
- cb_2018_us_county: County shapefiles from the United States Census Bureau.
- cb_2018_us_state: State shapefiles from the United States Census Bureau.
- current_county_pop: The population of each county in the United States recorded by the United States Census Bureau.
- decades: Each shapefile has taken the data from 1950-2021-torn-aspath and separated all tornadoes for a particular decade (e.g., 1950s, 1960s, etc.). This is a product of a function in final_project_Hora.R 
- FinalProjectHora.gdb: This database contains intermediate calculations in the FinalProjectHora ArcGIS Project File.
- GpMessages: Associated information.
- Index: Associated with FinalProjectHora.
- inflation: This table has the Consumer Price Index (CPI) for each month from January, 1950 until January 2022. 
- mean_centers: Where I stored the mean centers of tornadoes weighted by the median width of each tornado, completed in ArcGIS Pro.
- median_centers: Where I stored the median centers of tornadoes weighted by the median width of each tornado, completed in ArcGIS Pro (this did not end up being used).
- morans: the result of the global (html report screenshot) and local analysis (stored as shapefiles) for the 1950s and 2010s.
- saved_plots: All figures in the report are saved here.
- statefp: This file links the statefp to the common state name.

# Project Title: Early Warning Systems For Malaria Outbreaks in Thailand: An Anomaly Detection Approach

## Project Description: 
The aim of this research is to propose an early detection system to support the malaria
elimination program in Thailand and to improve methods for early detection of Malaria
in areas with impending epidemics. Anomaly detection algorithms are applied and compared using malaria data in Thailand. Additionally, a dashboard is built to support visualisation of anomalies. 

## Definitions:
1. Anomalies (in this context): Unusual malaria case activity based on province and time. 
	

## Structure:
1. anomaly_plotting_and_test.Rmd : Markdown file for testing and visualising anomaly detection algorithms.
2. anomaly_thailand_app (file): has the app and data files to run the app\
   2a. app.R : the main body of the app. Contains the UI and connection to server. Run this code to launch the app.\
   2b. load_data.R : load the initial data files for the app\
   2c. ano_algo_fxns.R : this file contains all the functions for anomaly detection, data handling, and comparing methods\
   2d. packages.R : this file loads all the required libraries for running the functions and the application\
   2e. outbreak_malaria_th_5.csv : this file contains the outbreak dates used for the comparison of algorithms\
   2f. pr_timeseries_monthly_cru_1901-2021_THA.csv : Thailand precipitation data from 1901 to 2021\
   2g. tas_timeseries_monthly_cru_1901-2021_THA.csv : Thailand temperature data from 1901 to 2021\
   2h. gadm36_THA_1_sp.rds : Preloaded Thailand map\ 
   2i. sector_01_11201_EN_.csv : Thailand province\


## How to Run App:
The app can be run by compiling the app.R file, however, due to data privacy, malaria case data is not uploaded for now. The app can be used using this link: https://moru.shinyapps.io/Malaria_Anomaly_Detection_App/ 

## Data Limitation: 
1. Thai malaria data is not provided. 

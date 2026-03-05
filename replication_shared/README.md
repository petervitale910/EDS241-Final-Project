Plastic bag bans and fees reduce harmful bag litter on shorelines 
By Anna Papp and Kimberly Oremus

This folder includes code and data to fully replicate Figures 1-5. In addition, the folder also includes instructions to rerun data cleaning steps. 

Last modified: March 6, 2025

For any questions, please reach out to ap3907@columbia.edu.
____

Code (replication/code):

To replicate main figures, run each file for each main figure: 
- 1_figure1.R
- 1_figure2.R
- 1_figure3.R 
- 1_figure4.R
- 1_figure5.R 

Update the home directory to match where the directory is saved ("replication" folder) in this file before running it. The code will require you to install packages (see note on versions below).

To replicate entire data cleaning pipeline:
- First download all required data (explained in Data section below). 
- Run code in code/0_setup folder (refer to separate README file).

____ 

R-Version and Package Versions

The project was developed and executed using:
- R version: 4.0.0 (2024-04-24)
- Platform: macOS 13.5 

Code was developed and main figures were created using the following versions: 
- data.table: 1.14.2
- dplyr: 1.1.4
- readr: 2.1.2
- tidyr: 1.2.0
- broom: 0.7.12
- stringr: 1.5.1
- lubridate: 1.7.9
- raster: 3.5.15
- sf: 1.0.7
- readxl: 1.4.0
- cobalt: 4.4.1.9002
- spdep: 1.2.3
- ggplot2: 3.4.4
- PNWColors: 0.1.0
- grid: 4.0.0
- gridExtra: 2.3
- ggpubr: 0.4.0
- knitr: 1.48
- zoo: 1.8.12 
- fixest: 0.11.2
- lfe: 2.8.7.1 
- did: 2.1.2
- didimputation: 0.3.0 
- DIDmultiplegt: 0.1.0
- DIDmultiplegtDYN: 1.0.15
- scales: 1.2.1 
- usmap: 0.6.1 
- tigris: 2.0.1 
- dotwhisker: 0.7.4

____

Data 

Processed data files are provided to replicate main figures. To replicate from raw data, follow the instructions below.

Policies (needs to be recreated or email for version): Compiled from bagtheban.com/in-your-state/, rila.org/retail-compliance-center/consumer-bag-legislation, baglaws.com, nicholasinstitute.duke.edu/plastics-policy-inventory, and wikipedia.org/wiki/Plastic_bag_bans_in_the_United_States; and massgreen.org/plastic-bag-legislation.html and cawrecycles.org/list-of-local-bag-bans to confirm legislation in Massachusetts and California.

TIDES (needs to be downloaded for full replication): Download cleanup data for the United States from Ocean Conservancy (coastalcleanupdata.org/reports). Download files for 2000-2009, 2010-2014, and then each separate year from 2015 until 2023. Save files in the data/tides directory, as year.csv (and 2000-2009.csv, 2010-2014.csv) Also download entanglement data for each year (2016-2023) separately in a file called data/tides/entanglement (each file should be called 'entangled-animals-united-states_YEAR.csv').

Shapefiles (needs to be downloaded for full replication): Download shapefiles for processing cleanups and policies. Download county shapefiles from the US Census Bureau; save files in the data/shapefiles directory, county shapefile should be in folder called county (files called cb_2018_us_county_500k.shp). Download TIGER Zip Code tabulation areas from the US Census Bureau (through data.gov); save files in the data/shapefiles directory, zip codes shapefile folder and files should be called tl_2019_us_zcta510.

Other: Helper files with US county and state fips codes, lists of US counties and zip codes in data/other directory, provided in the directory except as follows. Download zip code list and 2020 IRS population data from United States zip codes and save as uszipcodes.csv in data/other directory. Download demographic characteristics of zip codes from Social Explorer and save as raw_zip_characteristics.csv in data/other directory.

Refer to the .txt files in each data folder to ensure all necessary files are downloaded.


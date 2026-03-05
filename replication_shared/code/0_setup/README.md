Plastic bag bans and fees reduce harmful bag litter on shorelines 
By Anna Papp and Kimberly Oremus

This folder includes code to rerun data analysis steps. This requires having downloaded the necessary data as outlined below.

Last modified: March 6, 2025

For any questions, please reach out to ap3907@columbia.edu.
____

Code

To replicate entire pipeline: 
- First download all required data (explained in Data section below). 
- Run _00_03_clean_data.R file to clean and process data. This will run the entire pipeline and create files in data/processed folder that are required for the main figures and appendix figures/tables. 
Details on the individual folders:
-- 00_cleanup: These files process the cleanup data. 
-- 01_policy: These files process the bag policies. 
-- 02_merge: These files merge the cleanup and the policy data for various geographic and temporal resolutions. 
-- 03_entanglement: These files process the entanglement data. 

____

Data 

Processed data files are provided to replicate main figures. To replicate from raw data, follow the instructions below.

Policies (needs to be recreated or email for version): Compiled from bagtheban.com/in-your-state/, rila.org/retail-compliance-center/consumer-bag-legislation, baglaws.com, nicholasinstitute.duke.edu/plastics-policy-inventory, and wikipedia.org/wiki/Plastic_bag_bans_in_the_United_States; and massgreen.org/plastic-bag-legislation.html and cawrecycles.org/list-of-local-bag-bans to confirm legislation in Massachusetts and California.

TIDES (needs to be downloaded for full replication): Download cleanup data for the United States from Ocean Conservancy (coastalcleanupdata.org/reports). Download files for 2000-2009, 2010-2014, and then each separate year from 2015 until 2023. Save files in the data/tides directory, as year.csv (and 2000-2009.csv, 2010-2014.csv) Also download entanglement data for each year (2016-2023) separately in a file called data/tides/entanglement (each file should be called 'entangled-animals-united-states_YEAR.csv').

Shapefiles (needs to be downloaded for full replication): Download shapefiles for processing cleanups and policies. Download county shapefiles from the US Census Bureau; save files in the data/shapefiles directory, county shapefile should be in folder called county (files called cb_2018_us_county_500k.shp). Download TIGER Zip Code tabulation areas from the US Census Bureau (through data.gov); save files in the data/shapefiles directory, zip codes shapefile folder and files should be called tl_2019_us_zcta510.

Other: Helper files with US county and state fips codes, lists of US counties and zip codes in data/other directory, provided in the directory except as follows. Download zip code list and 2020 IRS population data from United States zip codes and save as uszipcodes.csv in data/other directory. Download demographic characteristics of zip codes from Social Explorer and save as raw_zip_characteristics.csv in data/other directory.

Refer to the .txt files in each data folder to ensure all necessary files are downloaded.


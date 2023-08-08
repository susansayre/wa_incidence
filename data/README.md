The analysis relies on serveral input files included in the data folder and described here.

- data/analysis_vars_baseline.rds is an author constructed file listing the variables used as explanatory and dependent variables as well as an indicator for the variable type

- data/census_regions.rdata is an author created list of census regions and codes

- data/CPIAUCSL.csv was download from https://fred.stlouisfed.org/series/CPIAUCSL using annual frequency with the aggregation method set to average

- data/decennial_data/DECENNIALSF12010.PCT16_data_with_overlays_2021-02-20T172748.csv (and accompanying metadata) contains data downloaded from the 2010 US census at data.census.gov

- data/degree_days/hdd.txt and degree_days/cdd.txt were downloaded from ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/procdate.txt. These specific files are no longer available there, but similar data can be constructed from the data available at https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/cdus/degree_days/

- data/elas_ctam.csv is an author constructed csv file containing the long-run own price elasticity estimates from the [Carbon Tax Assessment Model](https://www.commerce.wa.gov/growing-the-economy/energy/washington-state-energy-office/carbon-tax/#:~:text=The%20Carbon%20Tax%20Assessment%20Model,the%20five%20primary%20energy%20sectors.)

- data/energy_prices.csv contains energy price data that was downloaded from the EIA using the EIAdata R package that provided an interface to the now defunct version 1 of EIA's data API. Similar data can be downloaded from EIA using the price series identifiers in the file.

- data/fuelmix2016.csv was transcribed from http://www.commerce.wa.gov/wp-content/uploads/2019/12/Energy-Fuel-Mix-Disclosure-2016-final.pdf

- The three files labeled pums/geocorr2014_wa_xxx_to_yyy.csv were generated using the Missouri Data Center's 2014 crosswalk utility at https://mcdc.missouri.edu/applications/geocorr2014.htmlhttps://mcdc.missouri.edu/applications/geocorr2014.html using the xxx designator as the source and the yyy designator as the destination using 2010 population as the weighting variable.
- pums/id_psu.csv is an author constructed list of Core Based Statistical Areas in the US with their CEX designated Primary Sampling Unit (psu) identifiers for the CBSAs that are large enough to be self-identified in the CEX data.
*Note: the calls to the geocorr and id_psu files are embedded in functions calls within the carbonsms package without explicitly declaring them as targets dependencies. Changing these files will not trigger downstream files to be recreated on a new run of tar_make()

- rake_vars_alpha.rds is an author constructed file listing the raking variables used for the main analysis

- rake_vars_beta.rds is an author constructed file listing the raking variables used for supplemental analysis

data/Sales_Ult_Cust_2018.xlsx was downloaded and unzipped from https://www.eia.gov/electricity/data/eia861/archive/zip/f8612018.zip
data/Sales_Ult_Cust_2018.csv was created by modifying the header rows to facilitate reading into R

data/shapefiles/
2016 precinct data downloaded from https://www.sos.wa.gov/elections/research/precinct-shapefiles.aspx on 6/17/2020

Block group shapefiles downloaded from https://www.census.gov/cgi-bin/geo/shapefiles/index.php on 6/18/2020
Census tract shapefiles downloaded from https://www.census.gov/cgi-bin/geo/shapefiles/index.php on 7/7/2020

- data/state_price_match.csv is a manually constructed file that matches state abbreviations to the EIA price series used as the input price for the key fuels in the spending functions estimations

- data/U.S._Natural_Gas_Citygate_Price.csv was downloaded from https://www.eia.gov/dnav/ng/hist/n3050us3A.htm

data/utility_list.csv is an author-created file matching utility names to facilitate looking at utility price changes over time

data/utility_names_crosswalk.csv/xlsx is an author-created file matching utility names to facilitate computing tract level electricity price averages.

data/votes/2016Gen_Precinct_Results_GIS-Ready.xlsx was downloaded from https://www.sos.wa.gov/elections/research/Election-Results-and-Voters-Pamphlets.aspx on 1/13/2023. The file was downloaded as an xlsx file and modified to a csv file for use in code.

- data/wa2016_totals.csv is an author constructed csv file giving state total spending on various fuels. Numbers are drawn from the State Energy Data Systems (SEDS) and the Personal Consumption Expenditure data (PCE) as documented in the csv file.- analysis_vars_baseline.rds is an author constructed file listing the variables used as explanatory and dependent variables as well as an indicator for the variable type

- degree_days/hdd.txt and degree_days/cdd.txt were downloaded from ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/procdate.txt. This file is no longer available there, but similar data can be constructed from the data available at https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/cdus/degree_days/
- elas_ctam.csv is an author constructed csv file containing the long-run own price elasticity estimates from the [Carbon Tax Assessment Model](https://www.commerce.wa.gov/growing-the-economy/energy/washington-state-energy-office/carbon-tax/#:~:text=The%20Carbon%20Tax%20Assessment%20Model,the%20five%20primary%20energy%20sectors.)

- energy_prices.csv contains energy price data that was downloaded from the EIA using the EIAdata R package that provided an interface to the now defunct version 1 of EIA's data API. Similar data can be downloaded from EIA using the price series identifiers in the file.

- The three files labeled pums/geocorr2014_wa_xxx_to_yyy.csv were generated using the Missouri Data Center's 2014 crosswalk utility at https://mcdc.missouri.edu/applications/geocorr2014.htmlhttps://mcdc.missouri.edu/applications/geocorr2014.html using the xxx designator as the source and the yyy designator as the destination using 2010 population as the weighting variable.
- pums/id_psu.csv is an author constructed list of Core Based Statistical Areas in the US with their CEX designated Primary Sampling Unit (psu) identifiers for the CBSAs that are large enough to be self-identified in the CEX data.
*Note: the calls to the geocorr and id_psu files are embedded in functions calls within the carbonsms package without explicitly declaring them as targets dependencies. Changing these files will no trigger downstream files to be recreated on a new run of tar_make()

- rake_vars_alpha.rds is an author constructed file listing the raking variables used for the main analysis
- rake_vars_beta.rds is an author constructed file listing the raking variables used for supplemental analysis

* shapefiles/? bundle these with the code or provide instructions to download them?
- state_price_match.csv is a manually constructed file that matches state abbreviations to the EIA price series used as the input price for the key fuels in the spending functions estimations

- wa_totals.csv is an author constructed csv file giving state total spending on various fuels. Numbers are drawn from the State Energy Data Systems (SEDS) and the Personal Consumption Expenditure data (PCE) as documented in the csv file.
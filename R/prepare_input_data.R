## script designed to prepare cex and pums data and to create washington
## specific electricity carbon intensity input files. Needs to be manually adjusted 
## if years or states of analysis changes or if moved to a different computer
## where data is stored differently
## 
library(tidyverse)
library(sf)
library(tigris)
source("R/cex_functions.R")
source("R/main_functions.R")
source("R/sms_functions.R")
options(tigris_use_cache = T)
run_raking <- FALSE #set to true to run raking code

## check cex data and create RDS files containing necessary info ----
data_dir <- "data-raw/cex"
years <- 2012:2017
verify_cex_dir(data_dir, 2012:2017)
output_dir <- "prepared-data/cex"
if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = T)}

fmli_vars_to_keep <- c("fam_size", "state", "age_ref", "ref_race", "sex_ref",
                       "educ_ref", "occucod1", "persot64", "fam_type", 
                       "fincbtxm", "fsalarym", "frretirm", "fssixm", "pensionm",
                       "intrdvxm", "welfarem", "othregxm", "building", "roomsq",
                       "built", "vehq", "vehql", "jfs_amtm", "foodsmpm", 
                       "cutenure","fnonfrmm", "ffrmincm", "fsmpfrxm", "newid", 
                       "qintrvmo","qintrvyr", "finlwt21", "prinearn", "desflg",
                       "retsurvm", "other", "othrincm", "unistrq", "ownyi", 
                       "bls_urbn", "popsize", "region", "smsastat", "division", 
                       "psu", "totexppq", "foodpq", "sheltpq", "utilpq", 
                       "gasmopq", "healthpq", "cashcopq", "alcbevpq", "educapq")

map_dfr(years, ~ combine_fmli(.x, data_dir, fmli_vars_to_keep)) %>%
  saveRDS(file.path(output_dir, "combined_fmli.rds"))

map_dfr(years, ~ combine_mtbi(.x, data_dir)) %>%
  saveRDS(file.path(output_dir, "combined_mtbi.rds"))

read_cex("opb", years, data_dir,
         c("newid", "ownyb", "qyear", "acquiryr")) %>%
  saveRDS(file.path(output_dir, "combined_opb.rds"))

read_cex("opi", years, data_dir,
         c("newid", "ownyi", "propvalx")) %>%
  saveRDS(file.path(output_dir, "combined_opi.rds"))

read_cex("rnt", years, data_dir,
         c("newid", "RTELECT", "RTGAS", "RTWATER", "RTHEAT")) %>%
  saveRDS(file.path(output_dir, "combined_rnt.rds"))

read_quarterly_cex("memi", years, data_dir,
                   c("newid", "membno", "age", "cu_code", "arm_forc", "in_coll",
                     "earner","wkstatus", "inc_hrsq", "incnonwk", "educa",
                     "socrrx", "ssix")) %>%
  saveRDS(file.path(output_dir, "combined_memi.rds"))

## pums data for synthetic households ----
pums_dir <- "data-raw/pums/2016-5-year"
output_dir <- "prepared-data/pums"
pums_hh_file <- file.path(pums_dir, "csv_hwa/ss16hwa.csv")
pums_pp_file <- file.path(pums_dir, "csv_pwa/ss16pwa.csv")

if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = T)}

pp_vars <- c("SERIALNO","SPORDER", "ADJINC", "AGEP", "COW", "INTP", "MIL",
"RAC1P", "SEX", "ESR", "SCHG", "OCCP", "SCHL", "WKHP", "WAGP",
"SEMP", "SSP", "SSIP", "RETP", "FINTP", "PAP", "OIP", "RELP",
"MSP")

hh_vars <- c("SERIALNO", "NP", "NOC", "WIF", "FES", "FS", "VEH", "HINCP",
                       "TEN", "TYPE", "RMSP", "YBL", "HFL", "MV", "GRNTP", "SMP",
                       "INSP", "TAXP", "VALP", "ELEP", "GASP", "FULP", "WATP",
                       "ADJINC", "ADJHSG", "BLD", "PUMA", "WGTP")

pp_data <- data.table::fread(pums_pp_file, sep = ",",
                  stringsAsFactors = F, colClasses = "character", select = pp_vars) 
saveRDS(pp_data, "prepared-data/pums/pums_pp_wa2016.rds")

hh_data <- data.table::fread(pums_hh_file, sep = ",",
                  stringsAsFactors = F, colClasses = "character", select = hh_vars) 
saveRDS(pp_data, "prepared-data/pums/pums_hh_wa2016.rds")

## create tract level electricity intensity ----
boundaries <- tigris::tracts(state = "WA", cb = T, year = 2016) %>%
  st_as_sf() %>% 
  sf::st_transform("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs") 

tribal <- st_read(file.path("data-raw","shapefiles", "utilities_tribal", "BPA_CustomerTribal.shp")) %>% 
  st_transform("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs") %>% 
  select(-FID)

public <- st_read(file.path("data-raw","shapefiles", "utilities_public", "BPA_CustomerPublics.shp")) %>% 
  st_transform("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs") %>% 
  select(-FID) %>% 
  filter(State == "WA")

iou <- st_read(file.path("data-raw","shapefiles", "utilities_investor", "BPA_CustomerIOU.shp")) %>% 
  st_transform("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs") %>% 
  select(-FID) #%>% 
#st_union()

## add utility-specific prices to dataset ----

prices_by_utility <- read_csv(file.path("data","Sales_Ult_Cust_2018.csv")) %>%
  filter(State == "WA") %>%
  mutate(rev = as.numeric(gsub(",","",Revenues))) %>%
  mutate(sale = as.numeric(gsub(",","",Sales))) %>%
  mutate(price_utility = rev/sale) %>%
  select("Name2","price_utility")

summary(prices_by_utility)

# "To calculate a state or the US total, sum Parts (A,B,C & D) for Revenue, but only Parts (A,B & D) for Sales and Customers.
# To avoid double counting of customers, the aggregated customer counts for the states and US do not include the customer count for respondents with ownership code 'Behind the Meter'.
# This group consists of Third Party Owners of rooftop solar systems."																			

## Load fuelmix data by utility ----
fuelmix <- read_csv(file.path("data","fuelmix2016.csv")) %>%
  rename(unit = 1, share = 3, total_mwh = 6) %>%
  select("unit","Fuel","share","total_mwh") %>%
  mutate(share = as.numeric(gsub("%","",share))/100)

##########################################
####### Calculate Carbon content #########
##########################################

# Get carbon content by fuel using WA total CO2 for each fuel / WA total MWH for each fuel
# total CO2 is in metric tons
# output is in mwh
Fuel <- c('Biogas','Biomass','Coal','Geothermal','Hydro','Natural Gas',
          'Nuclear','Other Biogenic','Other Non-Biogenic',
          'Petroleum','Solar','Waste','Wind')
# Values from http://www.commerce.wa.gov/wp-content/uploads/2019/12/Energy-Fuel-Mix-Disclosure-2016-final.pdf
#mwh from Table 1; co2 from Table 3
mwh_2016 <- c(148177,675649,12799782,0,55697796,9937111,
              4308647,48095,73976,
              61888,3491,36723,3661267)

co2_2016 <- c(0,0,12507204,0,0,4276427,
              0,92695*48095/(48095+73976),92695*73976/(48095+73976),
              60172,0,60930,0)

fuel_emissions <- data.frame(Fuel,mwh_2016,co2_2016) %>% 
  mutate(co2_per_mwh = co2_2016/mwh_2016)
rm(Fuel,mwh_2016,co2_2016)

fuelmix <- left_join(x=fuelmix,y=fuel_emissions,by="Fuel") %>% 
  mutate(co2_per_mwh = replace_na(co2_per_mwh,0)) %>%
  group_by(unit) %>%
  summarise(co2_content = sum(share*co2_per_mwh))

##########################################
###### Graft onto utility shapefiles #####
##########################################

# Load crosswalk to match unit (fuelmix2016) to Name (BPA file) and to Name2 (Sales_Ult_Cust file)
crosswalk <- read_csv(file.path("data","utility_names_crosswalk.csv"))

#Merge fuel mix with WA state shape files:
# use hand-created crosswalk file to match Name and unit
u_public <- merge(public, crosswalk, by="Name", all.x=TRUE) %>%
  merge(fuelmix, by="unit", all.x=TRUE) %>%
  merge(prices_by_utility, by="Name2", all.x=TRUE)

u_investor <- merge(iou, crosswalk, by="Name", all.x=TRUE) %>%
  merge(fuelmix, by="unit", all.x=TRUE) %>%
  merge(prices_by_utility, by="Name2", all.x=TRUE)

u_tribal <- merge(tribal, crosswalk, by="Name", all.x=TRUE) %>% 
  merge(fuelmix, by="unit", all.x=TRUE) %>%
  merge(prices_by_utility, by="Name2", all.x=TRUE)

##########################################
###### Assign co2 content to tracts ######
##########################################

# Join tracts with utilities
joined_public <- st_join(boundaries,u_public) %>%
  select(STATEFP,COUNTYFP,TRACTCE,GEOID,co2_content,type,price_utility) %>%
  group_by(STATEFP,COUNTYFP,TRACTCE,GEOID) %>%
  #  st_drop_geometry() %>%
  drop_na(co2_content) %>%
  mutate(price_utility = replace_na(price_utility,0.0952))

joined_investor <- st_join(boundaries,u_investor) %>%
  select(STATEFP,COUNTYFP,TRACTCE,GEOID,co2_content,type,price_utility) %>%
  group_by(STATEFP,COUNTYFP,TRACTCE,GEOID) %>%
  #  st_drop_geometry() %>%
  drop_na(co2_content) %>%
  mutate(price_utility = replace_na(price_utility,0.0952))

joined_tribal <- st_join(boundaries,u_tribal) %>%
  select(STATEFP,COUNTYFP,TRACTCE,GEOID,co2_content,type,price_utility) %>%
  group_by(STATEFP,COUNTYFP,TRACTCE,GEOID) %>%
  #  st_drop_geometry() %>%
  drop_na(co2_content) %>%
  mutate(price_utility = replace_na(price_utility,0.0952))

# Summary of co2_content for each type
summary(joined_public$co2_content)
summary(joined_investor$co2_content)
summary(joined_tribal$co2_content)

## r combine co2 content into single data set ----

# All tracts (GEOIDs) compiled in single data set with joined utilities
# some tracts may appear multiple times if they use multiple utilities
joined_all <- joined_public %>%
  bind_rows(joined_investor) %>%
  bind_rows (joined_tribal) %>%
  drop_na(co2_content) 

# Frequency table of tracts with N utilities
temp <- joined_all %>% count(GEOID)
temp %>% group_by(n) %>% summarize(freq=n())
rm(temp)

# Now collapse dataset to GEOID; simple mean of co2_content average over all contributing utilities
joined_all <- joined_all %>%
  group_by(GEOID) %>%
  summarize(carbon_content = mean(co2_content),price_utility = mean(price_utility)) %>%
  #  select(GEOID, carbon_content, geometry)
  select(GEOID, carbon_content, price_utility)

rm(joined_public,joined_investor,joined_tribal,u_public,u_investor,u_tribal)

summary(joined_all)

detailed <- boundaries %>%
  select(GEOID) %>%
  st_drop_geometry %>%
  merge(joined_all, by="GEOID", all.x=TRUE) %>%
  mutate(carbon_content = replace_na(carbon_content,
                                     mean(carbon_content,na.rm=TRUE)),
         price_utility = replace_na(price_utility, 
                                    mean(price_utility, na.rm = TRUE))) %>%
  mutate(price = price_utility * 1000) %>% 
  select(GEOID, carbon_content, price)

summary(detailed)

write_csv(detailed, file.path("prepared-data", "electric_info_tract.csv"))

st_avg <- detailed %>% 
  mutate(carbon_content = 17096666/87452602, #totals from tables 1 and 3
         price = 95.2)  %>% #probably manually created from old data)
  write_csv(file.path("prepared-data", "electric_info_stavg.csv"))

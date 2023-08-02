#contains functions used by create_synthetic households
#functions to assist in processing PUMS data

#' merge household and person data for prediction purposes
#' @param household tibble of household prediction data
#' @param people tibble of individual prediction data
#' @param census_regions table of census regions
#' @param stAbbrev two-letter state abbreviation
#' @return merged household and person prediction data
#' @export
merge_pums_prediction_data <- function(hh_prediction, people_prediction, census_regions, stAbbrev){

  households <- hh_prediction %>%
    replace_na(list(NOC ~ 0,
                    WIF ~ 0)) %>%
    mutate(people = as.numeric(NP), #top-coded at 20
           children = as.numeric(NOC),
           adjinc = as.numeric(ADJINC)*.000001,
           adjhsg = as.numeric(ADJHSG)*.000001,
           tenure = as.factor(case_when(TEN %in% c("1","2") ~ "own",
                                        TEN %in% c("3","4") ~ "rent")),
           homeowner = if_else(tenure == "own", 1, 0),
           food_stamp = case_when(FS == '1' ~ 1,
                                  FS == '2' ~ 0,
                                  TRUE ~ 0), #setting missing data to no food stamps
           num_vehicles = as.numeric(VEH), #note: top-coded at 6
           tot_income = as.numeric(HINCP)*adjinc,
           house_type = as.factor(case_when(BLD=="01" ~ "mobile",
                                            BLD=="02" ~ "sf_detached",
                                            BLD=="03" ~ "sf_attached",
                                            BLD=="04" ~ "apts_2",
                                            BLD=="05" ~ "apts_3_4",
                                            BLD=="06" ~ "apts_5_9",
                                            BLD=="07" ~ "apts_10_19",
                                            BLD=="08" ~ "apts_20_49",
                                            BLD=="09" ~ "apts_50",
                                            BLD=="10" ~ "other")),
           sf_detached = if_else(house_type == "sf_detached", 1, 0),
           num_rooms = as.numeric(RMSP),
           year_built = case_when(tenure == "rent" ~ "rent",
                                  YBL=='01' ~ "pre1940", #includes all buildings older than 1939
                                  YBL=="02" ~ "1940s", #1940-1949
                                  YBL=="03" ~ "1950s", #1950-1959
                                  YBL=="04" ~ "1960s", #1960-1969
                                  YBL=="05" ~ "1970s",
                                  YBL=="06" ~ "1980s",
                                  YBL=="07" ~ "1990s",
                                  YBL=="08" ~ "early2000s", #2000-2004
                                  YBL!="bb" ~ "post2005",
                                  YBL=="bb" ~ "missing"),
           heating_fuel = as.factor(case_when(HFL=="1" ~ "natural gas",
                                              HFL=="2" ~ "bottled gas",
                                              HFL=="3" ~ "electricity",
                                              HFL=="4" ~ "fuel oil",
                                              HFL %in% c("5","6","8") ~ "coal wood other",
                                              HFL=="7" ~ "coal wood other", #none cannot be inferred from CEX data
                                              HFL=="9" ~ "coal wood other")), #solar cannot be inferred from CEX data
           electric_heat = if_else(heating_fuel == "electricity", 1, 0),
           bottled_gas_heat = if_else(heating_fuel == "bottled gas", 1, 0),
           ng_heat = if_else(heating_fuel == "natural gas", 1, 0),
           oil_heat = if_else(heating_fuel == "fuel oil", 1, 0),
           tenure_years = as.factor(case_when(tenure=="rent" ~ "rent",
                                              MV=="1" ~ "<1",
                                              MV=="2" ~ "1-2",
                                              MV=="3" ~ "2-4",
                                              MV=="4" ~ "5-9",
                                              MV=="5" ~ "10-19",
                                              MV=="6" ~ "20-29",
                                              MV=="7" ~ ">30",
                                              TRUE ~ "NA")),
           rent = as.numeric(GRNTP)*adjhsg,
           mortgage = as.numeric(SMP)*adjhsg, #monthly amount, consider whether to adjust for taxes/insurance
           home_insurance = as.numeric(INSP)*adjhsg, #fire/hazard/flood
           taxp_code = as.numeric(TAXP), #given in 68 distinct ranges
           property_tax = case_when(taxp_code<=21 ~ "<1000",
                                    taxp_code<=31 ~ "1000-2000",
                                    taxp_code<=41 ~ "2000-3000",
                                    taxp_code<=51 ~ "3000-4000",
                                    taxp_code<=61 ~ "4000-5000",
                                    taxp_code<=67 ~ "5000-10000",
                                    taxp_code==68 ~ ">10000"),
           prop_value = ifelse(tenure == "rent", 0, as.numeric(VALP)), #top-coded
           electric_in_rent = case_when(ELEP=='1' ~ 1,
                                        TRUE ~ 0), #should there be NA values?
           gas_in_rent = case_when(GASP=='1' ~ 1,
                                   TRUE ~ 0), #should there be NA values?
           heat_in_rent = case_when(FULP=='1' ~ 1,
                                    TRUE ~ 0), #should there be NA values?)
           water_in_rent = case_when(WATP=='1' ~ 1,
                                     TRUE ~ 0)
    )

  people <- people_prediction %>%
    mutate(age = as.numeric(AGEP),
           schl = as.numeric(SCHL),
           rel_code = as.numeric(RELP),
           occp_code = as.numeric(OCCP),
           educ_idx = case_when(schl==1 ~ 0,
                                schl<=11 ~ 10,
                                schl<=15 ~ 11,
                                schl<=17 ~ 12,
                                schl<=19 ~ 13,
                                schl==20 ~ 14,
                                schl==21 ~ 15,
                                schl>21 ~ 16),
           worker = case_when(as.numeric(ESR)<6 ~ 1,
                              TRUE ~ 0),
           adjinc = as.numeric(ADJINC)
    )

  household_totals <- people %>%
    mutate(adults = age >= 18,
           adults18_44 = age >= 18 & age <= 44,
           adults45_64 = age >= 45 & age <= 64,
           adults64plus = age > 64,
           children_6 = age < 6,
           children6_12 = age >= 6 & age <= 12,
           children13_17 = age >= 13 & age <= 17,
           college_students = SCHG %in% c("15","16"), #includes undergrad and grad
           num_military = MIL == "1", #doesn't include reserves, cex unclear
           num_unemployed = case_when(ESR=='3' ~ 1,
                                      TRUE ~ 0),
           wage_salary = as.numeric(WAGP)*adjinc,
           self_income = as.numeric(SEMP)*adjinc,
           soc_sec_inc = (as.numeric(SSIP) + as.numeric(SSP))*adjinc,
           ret_inc = as.numeric(RETP)*adjinc, #top-coded at 1m
           int_inc = as.numeric(INTP)*adjinc, #top and bottom coded
           oth_inc = as.numeric(OIP)*adjinc
           # Note: if adding variables, add between adults and oth_inc
    ) %>%
    group_by(SERIALNO) %>%
    select(SERIALNO, worker, adults:oth_inc) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    rename(num_in_labor_force = worker)

  household_averages <- people %>%
    mutate(adult_avg_age = replace(age, age < 18, NA),
           child_avg_age = replace(age, age >= 18, NA),
           worker_avg_age = replace(age, worker != 1, NA),
           avg_educ_idx = replace(educ_idx, age <= 18, NA),
           avg_hrs_worked = replace(as.numeric(WKHP), worker != 1, NA)
    ) %>%
    group_by(SERIALNO) %>%
    select(SERIALNO, adult_avg_age:avg_hrs_worked) %>%
    summarize_all(mean, na.rm = TRUE)

  oldest_own_child <- people %>%
    filter(rel_code > 1 & rel_code < 5) %>%
    select(SERIALNO,age) %>%
    group_by(SERIALNO) %>%
    summarize(max_child_age = max(age), num_own_kids = n())

  extended_family <- people %>%
    filter(rel_code >= 5) %>%
    group_by(SERIALNO) %>%
    summarize(num_extended = n())

  householders <- people %>%
    filter(RELP == "00") %>%
    left_join(oldest_own_child) %>%
    left_join(extended_family) %>%
    mutate(spouse_present = if_else(MSP == "1", 1,0),
           child_present = case_when(max_child_age == NA ~ 0,
                                     max_child_age<18 ~ 1,
                                     max_child_age>18 ~ 0),
           sex_hhldr = as.factor(case_when(SEX=="1" ~ "1",
                                           SEX=="2" ~ "2")),
           age_hhldr = age,
           age_cat_hhldr = case_when(age_hhldr <= 24 ~ "15-24",
                                     age_hhldr <= 34 ~ "25-34",
                                     age_hhldr <= 44 ~ "35-44",
                                     age_hhldr <= 54 ~ "45-54",
                                     age_hhldr <= 59 ~ "55-59",
                                     age_hhldr <= 64 ~ "60-64",
                                     age_hhldr <= 74 ~ "65-74",
                                     age_hhldr <= 84 ~ "75-84",
                                     age_hhldr >= 85 ~ "85plus"),
           rac_hhldr = as.factor(case_when(RAC1P=="1" ~ "white",
                                           RAC1P=="2" ~ "black",
                                           RAC1P=="6" ~ "asian",
                                           TRUE ~ "other/multi")),
           white_hhldr = if_else(rac_hhldr == "white", 1, 0),
           educ_hhldr = educ_idx,
           educ_cat_hhldr = factor(case_when(educ_hhldr %in% c("0","10","11") ~ "no_hs",
                                             educ_hhldr == "12" ~ "hs",
                                             educ_hhldr %in% c("13", "14") ~ "some_college",
                                             educ_hhldr %in% c("15", "16") ~ "college(+)")),
           empl_hhldr = as.numeric(ESR),
           num_extended = replace_na(num_extended,0)) %>%
    select(SERIALNO,max_child_age:empl_hhldr)

  household_values <- households %>%
    left_join(householders) %>%
    mutate(family_type = as.factor(case_when(spouse_present==1 & people==2 ~ 1,
                                             #spouse present and someone else too
                                             spouse_present==1 & num_extended==0 & max_child_age<6 ~ 2,
                                             spouse_present==1 & num_extended==0 & max_child_age<=17 ~ 3,
                                             spouse_present==1 & num_extended==0 & max_child_age>17 ~ 4,
                                             #not a married couple and own kids only household
                                             spouse_present==1 ~ 5,
                                             #not a married couple household
                                             max_child_age<18 & sex_hhldr=="male" ~ 6,
                                             max_child_age<18 & sex_hhldr=="female" ~ 7,
                                             people==1 ~ 8,
                                             #doesn't meet any of the previous criteria
                                             TRUE ~ 9))
    ) %>%
    left_join(household_totals) %>%
    left_join(household_averages) %>%
    #averages below will be na if there are no household members in the relevant
    #category. Set these to zero so they don't drop out of the regressions
    replace_na(list(avg_educ_idx = 0,
                    child_avg_age = 0,
                    worker_avg_age = 0,
                    avg_hrs_worked = 0)) %>%
    mutate(heating_fuel = as.factor(heating_fuel),
           stAbbrev = !!stAbbrev) %>%
    left_join(census_regions)
}

#' generate household totals for raking purposes
#' @param households table of household data from the pums sample
#' @param people table of individual data from the pums sample
#' @return dataset of household characteristics used to categorize households
#' on the raking categories
#' @export
generate_raking_stats <- function(households, people){
  householders <- people %>%
    filter(RELP == "00") %>%
    select(SERIALNO, AGEP, RAC1P)

  household_counts <- people %>%
    mutate(worker = case_when(COW=="" ~ 0,
                              COW!="" ~ 1)) %>%
    group_by(SERIALNO) %>%
    summarize(hh_size = n(), hh_workers = sum(worker))

  hh_raking_stats <- households %>%
    left_join(householders) %>%
    left_join(household_counts) %>%
    mutate(hh_base_weight = as.numeric(WGTP)) %>%
    filter(hh_base_weight > 0) #removes empty housing units
}

#' get acs vars for a given raking variable
#' @param rake_var name of variable to rake on, must be one of the options coded
#' @param acs_vars list of variables in the acs_data
#' within the switch call below. Withing the block for a possible rake variable,
#' code must create a variable #' whose name is rake_var and whose values are
#' the possible categories
#' @return list of variables from acs matched to planned categories
#' @export
get_rake_var_vars <- function(rake_var, acs_vars){

  rake_vars = do.call(str_c("get_rake_var_vars.", rake_var),list(acs_vars = acs_vars))
}


#' get raking vars for age
#'
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.age <- function(acs_vars){

  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B19037_")) %>%
    separate(label, into = c("tot", "est", "age", "inc"), sep = "!!", fill = "right") %>%
    filter(!is.na(age),is.na(inc)) %>% #extract only age category totaling
    mutate(age = str_c("age_", str_replace_all(age, c("under " = ".._to_",
                                                      "Householder " = "",
                                                      " years" = "",
                                                      " and over" = "_to_..",
                                                      " to " = "_to_"))),
           variable = str_remove(name, "E")) %>% #acs detailed data is missing trailing E
    select(variable, age)
}

#' get raking vars for income
#'
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.inc <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B19001_")) %>%
    separate(label, into = c("est","tot","inc"), sep = "!!", fill = "right") %>%
    filter(!is.na(inc)) %>%
    mutate(inc = str_replace_all(inc, c("Less than " = "...... to ",
                                        "\\$" = "",
                                        "," = "",
                                        " or more" = " to ......"))) %>%
    separate(inc, into = c("inc_lb", "inc_ub"), sep = " to ", , fill = "right") %>%
    mutate(inc_lb = str_remove(inc_lb, " "),
           inc_ub = str_remove(inc_ub, " "),
           inc = str_c("inc_",
                       str_pad(inc_lb, 6, side = "left", pad = "0"),
                       "_to_",
                       str_pad(inc_ub, 6, side = "left", pad = "0")),
           variable = str_remove(name, "E")) %>%
    select(variable,inc)}

#' raking on housing tenure type and home heating fuel

#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.tenure_fuel <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B25117")) %>%
    separate(label, into = c("tot","est","ten","fuel"), sep = "!!", fill = "right") %>%
    filter(!is.na(fuel)) %>%
    mutate(heating_fuel = case_when(fuel == "Utility gas" ~ "natural_gas",
                                    fuel == "Fuel oil, kerosene, etc." ~ "fuel_oil",
                                    fuel == "Bottled, tank, or LP gas" ~ "bottled_gas",
                                    fuel == "Electricity" ~ "electricity",
                                    TRUE ~ "coal_wood_other"),
           tenure = case_when(str_detect(ten, "Owner") ~ "own",
                              str_detect(ten, "Renter") ~ "rent"),
           tenure_fuel = str_c(tenure,"_", heating_fuel),
           variable = name) %>%
    select(variable,tenure_fuel)
}

#' raking on housing tenure type and home heating fuel general
#'
#used for Washington state where heating fuel needs to be coarsely categorized
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.tenure_fuel_gen <- function(acs_vars){
  rake_vars <- acs_vars %>%
             filter(str_detect(name,"^B25117")) %>%
             separate(label, into = c("tot","est","ten","fuel"), sep = "!!", fill = "right") %>%
             filter(!is.na(fuel)) %>%
             mutate(heating_fuel = case_when(fuel == "Utility gas" ~ "natural_gas",
                                             fuel == "Fuel oil, kerosene, etc." ~ "other",
                                             fuel == "Bottled, tank, or LP gas" ~ "other",
                                             fuel == "Electricity" ~ "electricity",
                                             TRUE ~ "other"),
                    tenure = case_when(str_detect(ten, "Owner") ~ "own",
                                       str_detect(ten, "Renter") ~ "rent"),
                    tenure_fuel_gen = str_c(tenure,"_", heating_fuel),
                    variable = name) %>%
             select(variable,tenure_fuel_gen)
         }

#' just fuel used for Washington state where heating fuel needs to be coarsely categorized
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.fuel_gen <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B25117")) %>%
    separate(label, into = c("tot","est","ten","fuel"), sep = "!!", fill = "right") %>%
    filter(!is.na(fuel)) %>%
    mutate(fuel_gen = case_when(fuel == "Utility gas" ~ "natural_gas",
                                fuel == "Fuel oil, kerosene, etc." ~ "other",
                                fuel == "Bottled, tank, or LP gas" ~ "other",
                                fuel == "Electricity" ~ "electricity",
                                TRUE ~ "other"),
           variable = name) %>%
    select(variable, fuel_gen)
}


#' get raking vars for education
#'
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.educ <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B25013")) %>%
    separate(label, into = c("tot","est","ten","education"), sep = "!!", fill = "right") %>%
    filter(!is.na(education)) %>%
    mutate(educ = case_when(education == "Bachelor's degree or higher" ~ "ba",
                            education == "Some college or associate's degree" ~ "some_college",
                            education == "High school graduate (including equivalency)" ~ "hs",
                            education == "Less than high school graduate" ~ "no_hs",
                            TRUE ~ "other"),
           variable = name) %>%
    select(variable, educ)
}


#' get raking vars for tenure x race crosstab
#'
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.tenure_race <- function(acs_vars){

  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B25003")) %>%
    separate(label, into = c("tot", "est", "ten"), sep = "!!", fill = "right") %>%
    filter(ten != "") %>% #remove category totaling
    mutate(tenure = case_when(str_detect(ten, "Owner") ~ "own",
                              str_detect(ten, "Renter") ~ "rent"),
           hh_race = case_when(str_detect(concept, "AMERICAN INDIAN") ~ "other",
                               str_detect(concept, "ASIAN") ~ "other",
                               str_detect(concept, "BLACK") ~ "black",
                               str_detect(concept, "\\(HISPANIC") ~ "hispanic",
                               str_detect(concept, "PACIFIC") ~ "other",
                               str_detect(concept, "OTHER") ~ "other",
                               str_detect(concept, "TWO") ~ "other",
                               str_detect(concept, "WHITE ALONE ") ~ "white",
                               str_detect(concept, "WHITE ALONE, NOT") ~ "white,nh"),
           tenure_race = str_c(tenure, "_", hh_race),
           variable = str_remove(name, "E$")) %>% #trailing E not present in acs_data
    filter(!(hh_race %in% c("hispanic", "white,nh")), !is.na(tenure_race)) %>% #not matching on hispanic
    select(variable,tenure_race)
}

#' raking on size and number of workers
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.size_wrkrs <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B08202")) %>%
    separate(label, into = c("tot", "est", "size", "wrkrs"), sep = "!!", fill = "right") %>%
    filter(wrkrs != "") %>% #remove category totaling
    mutate(size = str_c("s", case_when(str_detect(size, "1") ~ "1",
                                       str_detect(size, "2") ~ "2",
                                       str_detect(size, "3") ~ "3",
                                       str_detect(size, "4") ~ "4")),
           workers = str_c("w", case_when(str_detect(wrkrs, "No") ~ "0",
                                          str_detect(wrkrs, "1") ~ "1",
                                          str_detect(wrkrs, "2") ~ "2",
                                          str_detect(wrkrs, "3") ~ "3")),
           size_wrkrs = str_c(size, "_", workers),
           size_wrkrs = replace(size_wrkrs,
                                size_wrkrs %in% c("s3_w0", "s4_w0"),
                                "s34_w0"), #combine large, few worker households
           variable = str_remove(name, "E$")) %>% #trailing E not present in acs_data
    select(variable,size_wrkrs)
}


#' get raking vars for number of rooms
#'
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.rooms <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name, "^B25020")) %>%
    separate(label, into = c("tot", "est", "tenure", "rms"), sep = "!!", fill = "right") %>%
    filter(rms != "") %>%
    mutate(rms = as.numeric(substr(rms, 1, 1)),
           rooms = case_when(rms >= 9 ~ "rooms_9",
                             TRUE ~ str_c("rooms_", as.character(rms)))) %>%
    select(variable = name, rooms)
}


#' get raking vars for size_veh crosstab
#'
#' @param acs_vars list of variables in the acs_data
#' @return list of variables from acs matched to planned categories
get_rake_var_vars.size_veh <- function(acs_vars){
  rake_vars <- acs_vars %>%
    filter(str_detect(name,"^B08201")) %>%
    separate(label, into = c("tot", "est", "size", "veh"), sep = "!!", fill = "right") %>%
    filter(veh != "") %>% #remove category totaling
    mutate(size = case_when(str_detect(size, "1") ~ 1,
                            str_detect(size, "2") ~ 2,
                            str_detect(size, "3") ~ 3,
                            str_detect(size, "4") ~ 4),
           vehicles = case_when(str_detect(veh, "No") ~ 0,
                                str_detect(veh, "1") ~ 1,
                                str_detect(veh, "2") ~ 2,
                                str_detect(veh, "3") ~ 3,
                                str_detect(veh, "4") ~ 4),
           vehs = case_when(vehicles<size ~ as.character(vehicles),
                            TRUE ~ str_c(size,"p")),
           size_veh = str_c("s",size, "_v", vehs)) %>%
    select(variable = name,size_veh)
}

#' combine raking data

#' combine data from households with data on householders needed to generate raking data
#' if raking variables are added, make sure they are coded within this function
#' @param hh_raking_data household level data from pums
#' @param pp_raking_data person level data from pums
#' @return household level raking data ready to be processed with process_pums
#' @export
merge_raking_data <- function(hh_raking_data, pp_raking_data){
  householders <- pp_raking_data %>%
    filter(RELP == "00") %>%
    select(SERIALNO, AGEP, RAC1P, SCHL)

  household_counts <- pp_raking_data %>%
    mutate(worker = case_when(COW=="" ~ 0,
                              COW!="" ~ 1)) %>%
    group_by(SERIALNO) %>%
    summarize(hh_size = n(), hh_workers = sum(worker))

  # Note: starting with householders eliminates individuals in group quarters
  households <- householders %>%
    left_join(hh_raking_data) %>%
    left_join(household_counts) %>%
    mutate(hh_base_weight = as.numeric(WGTP)) %>%
    filter(hh_base_weight > 0) #removes empty housing units
}

#' process pums data for a specific raking variable
#' @param rake_var switch for which category to rake on
#' @param rake_var_vars list of acs variables matched to categories for var
#' @param hh_data dataset containing the individual households to be raked
#' @return an output dataset with a variable rake_var_cat containing the household category
#' @export
process_pums <- function(rake_var, rake_var_vars, hh_data) {

  #extract basic info that will be kept with raking variable specific info
  base_vars <- c("SERIALNO", "NP", "PUMA", "hh_base_weight")
  process_pums_args <- list(hh_data = hh_data, base_vars = base_vars)

  if (rake_var %in% c("inc", "age")) {process_pums_args[["cats"]] = rake_var_vars}

  hh_data_out <- do.call(str_c("process_pums.", rake_var), process_pums_args)
}

#' process pums for age
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with age specific info
#' @return an output dataset with a variable age_cat containing the household category
#' @param cats list of acs variables matched to age category
#' @export
process_pums.age <- function(hh_data, base_vars, cats){

  #pull out unique values of age and inc variables
  age_cats <- unique(cats$age)
  age_lbs <- as.numeric(str_replace(substr(age_cats,5,6),"[.]{2}","-Inf"))
  age_ubs <- as.numeric(str_replace(substr(age_cats,11,12),"[.]{2}","Inf"))

  #categorize pums data according to age and inc
  hh_data_out <- hh_data %>%
    mutate(age_cat = "",
           AGEP = as.numeric(AGEP)) #initialize variables that we'll replace below

  #add age categories
  for (i in seq_along(age_cats)) {
    hh_data_out <- hh_data_out %>%
      mutate(age_cat = replace(age_cat,
                               AGEP >= age_lbs[[i]] & AGEP <= age_ubs[[i]],
                               age_cats[[i]]))
  }

  hh_data_out <- hh_data_out %>%
    select(one_of(base_vars), age_cat)
}

#' process pums for inc
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with inc specific info
#' @param cats list of acs variables matched to inc category
#' @return an output dataset with a variable age_cat containing the household category
#' @export
process_pums.inc <- function(hh_data, base_vars, cats){
  inc_cats <- unique(cats$inc)
  inc_lbs <- as.numeric(str_replace(substr(inc_cats,5,10),"[.]{6}","-Inf"))
  inc_ubs <- as.numeric(str_replace(substr(inc_cats,15,20),"[.]{6}","Inf"))

  hh_data_out <- hh_data %>%
    mutate(inc_cat = "")

  for (i in seq_along(inc_cats)) {
    hh_data_out <- hh_data_out %>%
      mutate(inc_cat = replace(inc_cat,
                               as.numeric(HINCP) >= inc_lbs[[i]] & as.numeric(HINCP) <= inc_ubs[[i]],
                               inc_cats[[i]]))
  }

  hh_data_out <- hh_data_out %>%
    select(one_of(base_vars), inc_cat)

}
#' process pums for tenure_race
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with tenure_race specific info
#' @return an output dataset with a variable age_cat containing the household category
#' @export
process_pums.tenure_race <- function(hh_data, base_vars){
  hh_data_out <- hh_data %>%
    mutate(hh_race = case_when(RAC1P=="1" ~ "white",
                               RAC1P=="2" ~ "black",
                               RAC1P %in% c("3","4","5") ~ "other",
                               RAC1P=="6" ~ "other",
                               RAC1P=="7" ~ "other",
                               RAC1P=="8" ~ "other",
                               RAC1P=="9" ~ "other"),
           tenure = case_when(TEN %in% c("3","4") ~ "rent",
                              TEN %in% c("1","2") ~ "own"),
           tenure_race_cat = str_c(tenure, "_", hh_race)) %>%
    select(one_of(base_vars), tenure_race_cat)
}

#' process pums for tenure_fuel
#'
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with inc specific info
#' @return an output dataset with a variable tenure_fuel_cat containing the household category
#' @export
process_pums.tenure_fuel <- function(hh_data, base_vars){
  hh_data_out <- hh_data %>%
    mutate(hh_fuel = case_when(HFL=="1" ~ "natural_gas",
                               HFL=="2" ~ "bottled_gas",
                               HFL=="3" ~ "electricity",
                               HFL=="4" ~ "fuel_oil",
                               HFL %in% c("5","6","8") ~ "coal_wood_other",
                               HFL=="7" ~ "coal_wood_other", #none cannot be inferred from CEX data
                               HFL=="9" ~ "coal_wood_other"), #solar cannot be inferred from CEX data,
           tenure = case_when(TEN %in% c("3","4") ~ "rent",
                              TEN %in% c("1","2") ~ "own"),
           tenure_fuel_cat = str_c(tenure, "_", hh_fuel)) %>%
    select(one_of(base_vars), tenure_fuel_cat)
}

#' process pums for fuel_gen
#'
#' This function defines coarser fuel categories for use in areas that have less variation in fuel type like Washington state. Households are categorized as heating with electricity, natural gas, or other fuels
#'
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with fuel_gen specific info
#' @return an output dataset with a variable fuel_gen_cat containing the household category
#' @export
process_pums.fuel_gen <- function(hh_data, base_vars){
  hh_data_out <- hh_data %>%
    mutate(hh_fuel = case_when(HFL=="1" ~ "natural_gas",
                               HFL=="2" ~ "other",
                               HFL=="3" ~ "electricity",
                               HFL=="4" ~ "other",
                               HFL %in% c("5","6","8") ~ "other",
                               HFL=="7" ~ "other", #none cannot be inferred from CEX data
                               HFL=="9" ~ "other"),
           fuel_gen_cat = hh_fuel) %>%
    select(one_of(base_vars), fuel_gen_cat)
}

#' process pums for educ
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with educ specific info
#' @return an output dataset with a variable educ_cat containing the household category
process_pums.educ <- function(hh_data, base_vars){
  hh_data_out <- hh_data %>%
    mutate(educ_cat = case_when(SCHL<=15 ~ "no_hs",
                                SCHL<=17 ~ "hs",
                                SCHL<=20 ~ "some_college",
                                SCHL>20 ~ "ba")) %>%
    select(one_of(base_vars), educ_cat)
}

#' process pums for size_wrkrs
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with inc specific info
#' @return an output dataset with a variable age_cat containing the household category
process_pums.size_wrkrs <- function(hh_data, base_vars){
  hh_data_out <- hh_data %>%
    mutate(tc_size = replace(hh_size,hh_size > 4,4),
           tc_wrkrs = replace(hh_workers,hh_workers > 3,3),
           size_wrkrs_cat = str_c("s",tc_size,"_","w",tc_wrkrs),
           size_wrkrs_cat = replace(size_wrkrs_cat,tc_size >= 3 & tc_wrkrs == 0,"s34_w0")) %>%
    select(one_of(base_vars), size_wrkrs_cat)
}

#' process pums for rooms
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with inc specific info
#' @return an output dataset with a variable rooms_cat containing the household category
#' @export
process_pums.rooms <- function(hh_data, base_vars){
  hh_data_out <- hh_data %>%
    mutate(rooms_cat = case_when(as.numeric(RMSP) >= 9 ~ "rooms_9",
                                 TRUE ~ str_c("rooms_", as.numeric(RMSP)))) %>%
    select(one_of(base_vars), rooms_cat)
}

#' process pums for size_veh
#' @param hh_data dataset containing the individual households to be raked
#' @param base_vars basic info that is kept along with with size_veh specific info
#' @return an output dataset with a variable size_veh_cat containing the household category
#' @export
process_pums.size_veh <- function(hh_data, base_vars){
           hh_data_out <- hh_data %>%
             mutate(tc_size = replace(hh_size,hh_size > 4,4),
                    tc_veh = replace(VEH, VEH > 4, 4),
                    vehs = case_when(tc_veh < tc_size ~ as.character(tc_veh),
                                     TRUE ~ str_c(tc_size,"p")),
                    size_veh_cat = str_c("s", tc_size, "_v", vehs)) %>%
             select(one_of(base_vars), size_veh_cat)
         }


#' process acs data to generate constraint values for a given raking var
#' @param rake_var name of variable to rake on
#' @param rake_var_vars acs variable associated with the raking variable
#' @param geography the geography level at which to compute the constraints
#' Must be a geography recognized by the tidycensus package
#' @param state two letter abbreviation for the state to rake on
#' @param survey_type acs survey type
#' @param end_year acs end year
#' @return a list of GEOID, category labels and corresponding counts
#' @export
process_acs <- function(rake_var, rake_var_vars, geography, state, survey_type, end_year){

  if (geography == "zcta") {
    # note: since some zctas cross state boundaries, we can't limit the call by state here
    raw_data <- tidycensus::get_acs(geography = "zcta", variables = rake_var_vars$variable,
                                    year = end_year, survey = survey_type, cache = T)
  } else {
    raw_data <- tidycensus::get_acs(geography = geography,
                                    state = state,
                                    variables = rake_var_vars$variable,
                                    year = end_year,
                                    survey = survey_type, cache = T)
  }
  these_constraints <- raw_data %>%
    select(GEOID, variable, estimate) %>%
    right_join(rake_var_vars) %>%
    select(GEOID,estimate,matches(rake_var)) %>%
    group_by_at(vars("GEOID",all_of(rake_var))) %>%
    summarize(estimate = sum(estimate)) %>%
    spread(rake_var, estimate) %>%
    ungroup() %>%
    mutate(cat_total = rowSums(select(.,-GEOID))) %>%
    filter(cat_total > 30) %>%
    select(-cat_total)
}

#' adjust output weights for cross-PUMA targets

#' Some target geographies cross PUMA boundaries. Weights
#' produced by puma_weight assume the whole target geography population
#' target needs to be met with households from within the given PUMA, but we need
#' to adjust this down since some households will come from another PUMA. The
#' adjustment process assumes that both parts of the target geography meet
#' the population goals in proportion, which is unlikely to be true but we have no
#' more specific information.
#' @param hh_weights input weights from raking procedure by household/target geography
#' @param target_to_puma_alloc weights from the Missouri data center that allocate
#' target geography households to specific PUMAs
#' @return adjusted weights for each household/target geography combo
#' @export
adjust_output_weights <- function(hh_weights, target_to_puma_alloc){
  hh_weights_adjusted <- hh_weights %>%
    left_join(target_to_puma_alloc) %>%
    mutate(adj_hh_weight = hh_weight*to_puma_alloc)
}

#' adjust weights for target geography-based categories like urban/psu_pop_size
#'
#' Note that running this function will increase the size of the sample as any
#' input household located in a target geography that has households with diff
#' values of the category (e.g. rural and urban) will be duplicated with weights
#' summing to the original weight
#'
#' @param input_weights input weights from prior stages
#' @param target_to_cat_alloc weights from the Missouri data center that
#' allocate population from the target geography to a given category
#' should contain only the GEOID, category variables, and a to_cat_alloc
#' variable that allocates the target population across different
#' sub-categories
#' @param cat_name name of variable containing the category id
#' @return household ids, category ids, and adjusted weights
#' @export
add_cat <- function(input_weights,target_to_cat_alloc) {

  output_weights <- target_to_cat_alloc %>%
    left_join(input_weights) %>%
    mutate(adj_hh_weight = adj_hh_weight * to_cat_alloc) %>%
    select(-to_cat_alloc)
}

#' loop through PUMAs and rake households for each PUMA

#' @param ind_hh categorization for households on each raking variable;
#' includes starting weights?
#' @param constraints category totals for target geography for each raking variable
#' @param puma_list list of PUMAs to rake
#' @param rake_cat_list list of categories to rake on
#' @param iterations raking iterations, default = 20
#' @param integer_weights logical for whether to convert weights to integer (default = F)
#' @return initial raking weights for each household for the target geography
#' @export
rake_pumas <- function(ind_hh, constraints, puma_list, rake_cat_list, iterations = 20, integer_weights = F){

  hh_weights <- tibble(SERIALNO = character(),
                       GEOID = character(),
                       PUMA = character(),
                       hh_weight = integer())
  for (i in seq_along(puma_list)) {
    this_weight <- puma_weight(constraints, ind_hh, puma_list[[i]], rake_cat_list, iterations = iterations, integer_weights = integer_weights)
    hh_weights <- hh_weights %>%
      bind_rows(this_weight)
  }
  return(hh_weights)

}

#' rake households for a specific PUMA

#' adapts the rakeR package's raking function to address starting weights. For
#' my problem, the starting weight for a household is the product of a constant
#' household weight reported in the acs data and a GEOID specific allocation
#' factor drawn from the Missouri Data Center. This means that the starting
#' weight for a specific household will depend on which GEOID totals we're
#' trying to match in a given iteration
#' @param cons_data dataset of constraints and weights
#' for each GEOID, identifies the PUMA, the PUMA/GEOID allocation factor, and
#' the category totals for each raking variable
#' @param ind_hh dataset of households categories, by household id, gives
#' starting weight from the acs, the household category on each raking variable
#' @param puma_id puma code to rake
#' @param vars_to_match list of variables to rake on
#' @param iterations [20] iterations for the IPF algorithm
#' @param integer_weights logical for whether to return integer weights (default = F)
#' @import dplyr
#' @export

puma_weight <- function(cons_hh, ind_hh, puma_id, vars_to_match, iterations = 20, integer_weights = F) {
  puma_cons <- cons_hh %>%
    filter(PUMA == puma_id,to_target_alloc > 0)

  #removes categories that have no members in this PUMA, note that the selection
  #will keep the GEOID because it is a positive numeric code
  non_zero <- function(x) sum(as.numeric(x)) > 0
  cons <- puma_cons %>%
    dplyr::select(-PUMA,-to_target_alloc) %>%
    dplyr::select_if(non_zero)

  puma_hh <- ind_hh %>%
    filter(PUMA == puma_id)

  cons <- cons[, -1]
  cons <- as.matrix(cons)
  cons[] <- as.numeric(cons[])

  inds <- puma_hh %>%
    dplyr::select(one_of(vars_to_match))

  # Use rakeR trick to
  # Create a list of survey based matrices to match cons matrices
  # Easiest way is to create 'dummy variables' (i.e. 0, 1) using model.matrix.
  # The '-1' drops the intercept, and puts the first variable back in
  inds <- lapply(as.list(vars_to_match), function(x) {
    stats::model.matrix( ~ inds[[x]] - 1)
  })

  # Fix colnames
  for (i in seq_along(vars_to_match)) {  # for loop ok; typically only <= 12 columns
    colnames(inds[[i]]) <- gsub("inds\\[\\[x\\]\\]", "", colnames(inds[[i]]))
  }
  rm(i)

  # one ind table based on unique levels in inds is easier to check and use
  ind_cat <- do.call(cbind, inds)

  # check colnames match exactly at this point
  # this is crucial to ensure the simulation doesn't provide incorrect results
  if (!isTRUE(all.equal(colnames(ind_cat), colnames(cons)))) {
    tryCatch(
      #see if the columns are all available in cons but simply in the wrong order
      cons <- cons[, colnames(ind_cat)],
      error = function(e) {
        stop("Column names don't match.\n
         Are the first columns in cons and inds a zone code/unique ID?
         Check the unique levels in inds and colnames in cons match EXACTLY.
         Unique levels identified by weight():\n\n",
             vapply(seq_along(colnames(ind_cat)), function(x)
               paste0(colnames(ind_cat)[x], " "), ""),"\n",
             "PUMA ID: ", puma_id
        )})
  }

  # give ind_cat sequential column names to ensure they're entered into the
  # model in the correct order
  colnames(ind_cat) <-
    paste0(
      seq_along(colnames(ind_cat)),
      "_",
      colnames(ind_cat)
    )
  colnames(cons) <- colnames(ind_cat)

  # initialize result weights matrix to be num_hh x num_GEOID
  weights <- matrix(ncol = nrow(cons), nrow = nrow(ind_cat))

  for (i in 1:nrow(cons)) {
    tmp_out <- ipfp::ipfp(cons[i,],
                          t(ind_cat),
                          x0 = puma_hh$hh_base_weight*puma_cons$to_target_alloc[[i]],
                          maxit = iterations)
    if (any(is.nan(tmp_out))){browser()}
    weights[, i] <- tmp_out
  }
  rm(i)

  # The sum of weights will form the simulated population so this must match
  # the population from cons
  if (!isTRUE(all.equal(sum(weights), (sum(cons) / length(vars_to_match))))) {
    browser()
    stop("Weight populations don't match constraint populations.
         Usually this means the populations for each of your constraints
         are slightly different\n",
         "Sum of simulated population:  ", sum(weights), "\n",
         "Sum of constraint population: ", (sum(cons) / length(vars_to_match)), "\n",
         "PUMA:  ", puma_id)
  }

  # The colSums of weights will form the simulated population in each zone so
  # these should match the actual populations in each zone from cons
  if (!isTRUE(colSums(weights) - (rowSums(cons) / length(vars_to_match))) < 1L) {
    stop("Simulated weights by zone differ from constraint weights by zone\n",
         "Sum of the differences between zones (should be <1): ",
         sum(colSums(weights) - (rowSums(cons) / length(vars_to_match)))
    )
  }

  if (integer_weights){
    for (i in 1:ncol(weights)) {
      # use method from https://spatial-microsim-book.robinlovelace.net/smsimr.html#sintegerisation to integerize weights
      int_weight <- floor(weights[, i])
      resid <- weights[, i] - int_weight
      deficit <- round(sum(resid))
      if (deficit > 0) {
        topup <- sample(length(int_weight), size = deficit, prob = resid)
        int_weight[topup] <- int_weight[topup] + 1
      }
      weights[, i] <- int_weight

    }
  }

  colnames(weights) <- as.vector(unlist(puma_cons$GEOID))

  puma_weights <- as_tibble(weights) %>%
    dplyr::mutate(SERIALNO = puma_hh$SERIALNO) %>%
    tidyr::gather(key = "GEOID", value = "hh_weight", -SERIALNO) %>%
    dplyr::mutate(PUMA = puma_id) %>%
    dplyr::filter(hh_weight>0)

  return(puma_weights)
}

#' load target to puma weight
#' @param state_short a lower case two letter abbreviation for a state
#' @param raking_geography an abbreviation for the raking geography to use;
#' code will fail if the raking geography hasn't been defined appropriately
#' throughout the code -- need a geocorr file with this abbreviation in name
#' @param target_to_puma_filename name of geocorr csv file containing wgt data
#' @return success/failure on last write operation
#' @export
load_target_to_puma_wgt <- function(state_short, raking_geocorr, target_to_puma_file){

  input <- read_csv(target_to_puma_file, col_names = T) %>%
    slice(-1)

  switch(raking_geocorr,
         zcta5 = {weight <- input %>% rename(GEOID = zcta5)},
         sldl14 = {weight <- input %>% mutate(GEOID = str_c(state, sldl14))},
         sldl16 = {weight <- input %>% mutate(GEOID = str_c(state, sldl16))},
         tract = {weight <- input %>% mutate(GEOID = str_c(county, str_remove(tract, fixed("."))))},
         stop(str_c("raking_geocorr ", raking_geocorr, " not recognized")))

  target_to_puma_wgt <- weight %>%
    mutate(to_puma_alloc = as.numeric(afact)) %>%
    select(GEOID, PUMA = puma12, to_puma_alloc)
}

#' load puma to target weights
#' @param state_short a lower case two letter abbreviation for a state
#' @param raking_geography an abbreviation for the raking geography to use;
#' code will fail if the raking geography hasn't been defined appropriately
#' throughout the code -- need a geocorr file with this abbreviation in name
#' @param puma_to_target_file name of geocorr csv file containing wgt data
#' @return success/failure on last write operation
#' @export
load_puma_to_target_wgt <- function(state_short, raking_geocorr, puma_to_target_file){

  input <- read_csv(puma_to_target_file, col_names = T) %>%
    slice(-1)  #drop data labels

  switch(raking_geocorr,
         zcta5 = {weight <- input %>% rename(GEOID = zcta5)},
         sldl14 = {weight <- input %>% mutate(GEOID = str_c(state, sldl14))},
         sldl16 = {weight <- input %>% mutate(GEOID = str_c(state, sldl16))},
         tract = {weight <- input %>% mutate(GEOID = str_c(county, str_remove(tract, fixed("."))))},
         stop(str_c("raking_geocorr ", raking_geocorr, " not recognized")))

  target_to_puma_wgt <- weight %>%
    mutate(to_target_alloc = as.numeric(afact)) %>%
    select(GEOID, PUMA = puma12, to_target_alloc)
}


#' use Missouri data center cross-walk data to compute psu_pop_size weights
#'
#' computes the percentage of population in each geoid that is located in a psu
#' of various population sizes or is located in a rural psu (e.g. outside a core
#' based statistical area)
#' @param state_short two letter state abbreviation
#' @param year end year of acs data to use
#' @param survey default = "acs5"
#' @param raking_geography name of a raking geography, verified to work with
#' sldl, tract but may run into issues with other source types
#' @param input_file name of the input file containing the relevant data which
#' must have been downloaded manually from the Missouri data center by selecting
#' the raking geography as the source and urban area/cluster as the target
#' @param psu_file name of the input file containing cbsa codes linked to psu
#' @return table of geoid/cbsa combinations with allocation factors
#' @export
compute_loc_type <- function(state_short, year, acs_type = "acs5", raking_geocorr, input_file, psu_file) {

  if (raking_geocorr == "zcta5") {
    prefix = ""
  }  else {
    fips_code <- gsub("[^0-9]", "",  tigris::lookup_code(state = state_short))
  }

  population <- tidycensus::get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                                    variables = "B01003_001", year = 2016,
                                    survey = acs_type, cache = T) %>%
    select(cbsa = GEOID, population = estimate) %>%
    mutate(loc_type = factor(case_when(population > 4000000 ~ "urban_over4M",
                                       population > 1200000 ~ "urban_1.2-4M",
                                       population >  330000 ~ "urban_330-1200k",
                                       population >  125000 ~ "urban_125-330k",
                                       TRUE ~ "urban_under125k"),
                             levels = c("urban_over4M", "urban_1.2-4M", "urban_330-1200k", "urban_125-330k", "urban_under125k", "rural")))

  psu_codes <- read_csv(psu_file,
                        col_types = cols(cbsa = col_character(),
                                         NAME = col_character(),
                                         psu = col_character()))


  combine_with_cbsa <- read_csv(input_file, col_names = T) %>%
    slice(-1) %>% #drops row containing data labels
    select(-starts_with("cbsaname")) %>%
    rename(cbsa = starts_with("cbsa")) %>%
    mutate(to_cbsa_alloc = as.numeric(afact)) %>%
    left_join(population) %>%
    mutate(loc_type = replace_na(loc_type,"rural")) %>%
    left_join(select(psu_codes, cbsa, psu)) %>%
    mutate(psu = replace_na(psu, "XXXX"))


  combined_data <- switch(raking_geocorr,
         zcta5 = {combine_with_cbsa %>% mutate(GEOID = zcta5)},
         sldl14 = {combine_with_cbsa %>% mutate(GEOID = str_c(fips_code, sldl14))},
         sldl16 = {combine_with_cbsa %>% mutate(GEOID = str_c(fips_code, sldl16))},
         tract = {combine_with_cbsa %>% mutate(GEOID = str_c(county, str_remove(tract, fixed("."))))})

  target_to_cbsa_allocation <- combined_data %>%
    group_by(GEOID, loc_type, psu) %>%
    summarize(to_cat_alloc = sum(to_cbsa_alloc)) %>%
    select(GEOID, loc_type, to_cat_alloc, psu)
}


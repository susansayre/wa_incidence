#' find median acs characterisitcs
#' @param synthetic_median_characteristics median characteristics from synthetic data for pulling tract level values and getting tract list
#' @param acs_year end year to use
#' @param acs_vars table returned by tidycensus::load_variables
#' @return median/avg characteristics by tract
#' @export
find_median_acs_chars <- function(synthetic_median_characteristics, acs_year, acs_vars){

  tract_level_variables <- c("hdd","cdd","electricity_price","gasoline_price",
                             "natural_gas_price","heating_oil_price",
                             "propane_price", "region", "loc_type", "psu", "state")

  tract_data <- synthetic_median_characteristics %>%
    select(one_of(tract_level_variables), GEOID, hhkey, final_weight)

  #top-coded at 4 people
  people_data <- tidycensus::get_acs("tract",
                         variables = c("S2501_C01_002", "S2501_C01_003", "S2501_C01_004", "S2501_C01_005"),
                         year = acs_year, state = "WA", cache = T) %>%
    mutate(people = case_when(variable == "S2501_C01_002" ~ 1,
                              variable == "S2501_C01_003" ~ 2,
                              variable == "S2501_C01_004" ~ 3,
                              variable == "S2501_C01_005" ~ 4)) %>%

    group_by(GEOID, people) %>%
    summarize(num_hh = sum(estimate)) %>%
    group_by(GEOID) %>%
    mutate(share = num_hh/sum(num_hh),
           cumshare = cumsum(share)) %>%
    filter(cumshare >= .5) %>%
    arrange(GEOID, cumshare) %>%
    slice(1) %>%
    select(GEOID, people)

  age_cat_data <- tidycensus::get_acs("tract", table = "B25007", year = acs_year, state = "WA", cache = T) %>%
    left_join(acs_vars, by = c("variable" = "name")) %>%
    separate(label, into = c("A", "B", "tenure", "age_cat"), sep = "!!") %>%
    filter(!is.na(age_cat)) %>%
    group_by(GEOID, age_cat) %>%
    summarize(num_hh = sum(estimate)) %>%
    mutate(age_cat_hhldr = factor(case_when(age_cat == "Householder 15 to 24 years" ~ "15-24",
                                               age_cat == "Householder 25 to 34 years" ~ "25-34",
                                               age_cat == "Householder 35 to 44 years" ~ "35-44",
                                               age_cat == "Householder 45 to 54 years" ~ "45-54",
                                               age_cat == "Householder 55 to 59 years" ~ "55-59",
                                               age_cat == "Householder 60 to 64 years" ~ "60-64",
                                               age_cat == "Householder 65 to 74 years" ~ "65-74",
                                               age_cat == "Householder 75 to 84 years" ~ "75-84",
                                               age_cat == "Householder 85 years and over" ~ "85plus"),
                                  levels = levels(synthetic_median_characteristics$age_cat_hhldr))) %>%
    group_by(GEOID) %>%
    mutate(share = num_hh/sum(num_hh),
           cumshare = cumsum(share)) %>%
    filter(cumshare >= .5) %>%
    arrange(GEOID, cumshare) %>%
    slice(1) %>%
    select(GEOID, age_cat_hhldr)

  #Note table changed between 2016 and 2017
  #in 2016, S2502_C01_002E is the percent white,
  #in 2017, it is the number white and needs to be divided by 001E to get percent
  white_hhldr_data <- tidycensus::get_acs("tract", variables = c("S2502_C01_001", "S2502_C01_002"),
                              year = acs_year, state = "WA", cache = T, output = "wide") %>%
    mutate(white_hhldr = S2502_C01_002E/100) %>%
    select(GEOID, white_hhldr)

  #Note: probably need to adjust cex/pums variable to group some college and associates which are reported jointly in the acs data. Combined group is mapped to some college (13) here rather than associate's degree (14). Not ideal because its the most common median
  educ_hhldr_data <- tidycensus::get_acs("tract", table = "B25013",
                             year = acs_year, state = "WA", cache = T) %>%
    mutate(educ_cat_hhldr = factor(case_when(variable %in% c("B25013_008", "B25013_003") ~ "no_hs",
                                            variable %in% c("B25013_009", "B25013_004") ~ "hs",
                                            variable %in% c("B25013_010", "B25013_005") ~ "some_college",
                                            variable %in% c("B25013_011", "B25013_006") ~ "college(+)"),
                               levels = levels(synthetic_median_characteristics$educ_cat_hhldr))) %>%
    filter(!is.na(educ_cat_hhldr)) %>%
    group_by(GEOID, educ_cat_hhldr) %>%
    summarize(num_hh = sum(estimate)) %>%
    arrange(GEOID, educ_cat_hhldr) %>%
    group_by(GEOID) %>%
    mutate(share = num_hh/sum(num_hh),
           cumshare = cumsum(share)) %>%
    filter(cumshare >= .5) %>%
    arrange(GEOID, cumshare) %>%
    slice(1) %>%
    select(GEOID, educ_cat_hhldr)

  # Note: 3 is actually 3 or more, but is unlikely to be the median category
  num_in_labor_force_data <- tidycensus::get_acs("tract",
                                     variables = c("B08202_002","B08202_003","B08202_004","B08202_005"),
                                     year = acs_year, state = "WA", cache = T) %>%
    mutate(num_in_labor_force = case_when(variable == "B08202_002" ~ 0,
                                          variable == "B08202_003" ~ 1,
                                          variable == "B08202_004" ~ 2,
                                          variable == "B08202_005" ~ 3)) %>%
    group_by(GEOID) %>%
    mutate(share = estimate/sum(estimate),
           cumshare = cumsum(share)) %>%
    filter(cumshare >= .5) %>%
    arrange(GEOID, cumshare) %>%
    slice(1) %>%
    select(GEOID, num_in_labor_force)

  #Note: uses tract level unemployment rate times labor force participation. Possibly questionable since the median number unemployed is probably zero.
  num_unemployed_data <- tidycensus::get_acs("tract",
                                 variable = "S2301_C04_001",
                                 year = acs_year, state = "WA", cache = T) %>%
    left_join(num_in_labor_force_data) %>%
    mutate(num_unemployed = num_in_labor_force * estimate/100) %>%
    select(GEOID, num_unemployed, num_in_labor_force)

  # tract level average not median
  food_stamp_data <- tidycensus::get_acs("tract",
                             variable = "S2201_C04_001",
                             year = acs_year, state = "WA", cache = T) %>%
    mutate(food_stamp = estimate/100) %>%
    select(GEOID, food_stamp)

  #note censored at 3 or more but probably not median
  num_vehicles_data <- tidycensus::get_acs("tract",
                               variables = c("DP04_0058", "DP04_0059", "DP04_0060", "DP04_0061"),
                               year = acs_year, state = "WA", cache = T) %>%
    mutate(num_vehicles = case_when(variable == "DP04_0058" ~ 0,
                                    variable == "DP04_0059" ~ 1,
                                    variable == "DP04_0060" ~ 2,
                                    variable == "DP04_0061" ~ 3)) %>%
    group_by(GEOID) %>%
    mutate(share = estimate/sum(estimate),
           cumshare = cumsum(share)) %>%
    filter(cumshare >= .5) %>%
    arrange(GEOID, cumshare) %>%
    slice(1) %>%
    select(GEOID, num_vehicles)

  #Note table changed between 2016 and 2017. Use C03 with 2017
  tot_income_data <- tidycensus::get_acs("tract",
                             variables = c("S1903_C02_001"),
                             year = acs_year, state = "WA", cache = T) %>%
    select(GEOID, tot_income = estimate)

  sf_detached_data <- tidycensus::get_acs("tract",
                              variables = c("B25032_001", "B25032_003", "B25032_014"),
                              year = acs_year, state = "WA", cache = T, output = "wide") %>%
    mutate(sf_detached = (B25032_003E + B25032_014E)/B25032_001E) %>%
    select(GEOID, sf_detached)

  homeowner_data <- tidycensus::get_acs("tract",
                            variables = c("B25003_001", "B25003_002"),
                            year = acs_year, state = "WA", cache = T, output = "wide") %>%
    mutate(homeowner = B25003_002E/B25003_001E) %>%
    select(GEOID, homeowner)

  ## top coded at 9 and some tracts do have 9 as the median ---
  num_rooms_data <- tidycensus::get_acs("tract",
                            table = "B25020",
                            year = acs_year, state = "WA", cache = T) %>%
    mutate(num_rooms = case_when(variable == "B25020_003" ~ 1,
                                 variable == "B25020_004" ~ 2,
                                 variable == "B25020_005" ~ 3,
                                 variable == "B25020_006" ~ 4,
                                 variable == "B25020_007" ~ 5,
                                 variable == "B25020_008" ~ 6,
                                 variable == "B25020_009" ~ 7,
                                 variable == "B25020_010" ~ 8,
                                 variable == "B25020_011" ~ 9,
                                 variable == "B25020_013" ~ 1,
                                 variable == "B25020_014" ~ 2,
                                 variable == "B25020_015" ~ 3,
                                 variable == "B25020_016" ~ 4,
                                 variable == "B25020_017" ~ 5,
                                 variable == "B25020_018" ~ 6,
                                 variable == "B25020_019" ~ 7,
                                 variable == "B25020_020" ~ 8,
                                 variable == "B25020_021" ~ 9)) %>%
    filter(!is.na(num_rooms)) %>%
    group_by(GEOID, num_rooms) %>%
    summarize(num_hh = sum(estimate)) %>%
    group_by(GEOID) %>%
    mutate(share = num_hh/sum(num_hh),
           cumshare = cumsum(share)) %>%
    filter(cumshare >= .5) %>%
    arrange(GEOID, cumshare) %>%
    slice(1) %>%
    select(GEOID, num_rooms)

  year_built_data <- tidycensus::get_acs("tract",
                             table = c("B25035"),
                             year = acs_year, state = "WA", cache = T) %>%
    mutate(year_built = factor(case_when(estimate<1940 ~ "pre1940",
                                         estimate<1950 ~ "1940s",
                                         estimate<1960 ~ "1950s",
                                         estimate<1970 ~ "1960s",
                                         estimate<1980 ~ "1970s",
                                         estimate<1990 ~ "1980s",
                                         estimate<2000 ~ "1990s",
                                         estimate<2005 ~ "early2000s",
                                         estimate>=2005 ~ "post2005",
                                         is.na(estimate) ~ "missing"),
                               levels = c("rent", "missing", "pre1940", "1940s", "1950s",
                                          "1960s", "1970s", "1980s", "1990s",
                                          "early2000s", "post2005"),
                               ordered = T)) %>%
             select(GEOID, year_built)

  # heat_data <- tidycensus::get_acs("tract",
  #                              variables = c("DP04_0062", "DP04_0063", "DP04_0064", "DP04_0065", "DP04_0066"),
  #                              year = acs_year, state = "WA", cache = T, output = "wide") %>%
  #   mutate(electric_heat = DP04_0065E/DP04_0062E,
  #          bottled_gas_heat = DP04_0064E/DP04_0062E,
  #          ng_heat = DP04_0063E/DP04_0062E,
  #          oil_heat = DP04_0066E/DP04_0062E) %>%
  #   select(GEOID, contains("heat"))

  heat_data <- tidycensus::get_acs("tract",
                       variables = c("DP04_0062", "DP04_0063", "DP04_0064", "DP04_0065", "DP04_0066"),
                       year = acs_year, state = "WA", cache = T) %>%
    mutate(heating_fuel = factor(case_when(variable == "DP04_0065" ~ "electricity",
                                              variable == "DP04_0064" ~ "bottled gas",
                                              variable == "DP04_0063" ~ "natural gas",
                                              variable == "DP04_0066" ~ "fuel oil"),
                                 levels = levels(synthetic_median_characteristics$heating_fuel))) %>%
    filter(!is.na(heating_fuel)) %>%
    arrange(GEOID, desc(estimate)) %>%
    group_by(GEOID) %>%
    slice(1) %>%
    select(GEOID, heating_fuel)

  spouse_present_data <- tidycensus::get_acs("tract",
                                 variables = c("B11001_001", "B11001_003"),
                                 year = acs_year, state = "WA", cache = T, output = "wide") %>%
    mutate(spouse_present = B11001_003E/B11001_001E) %>%
    select(GEOID, spouse_present)

  child_var_codes <- read_csv("data/decennial-data/DECENNIALSF12010.PCT16_metadata_2021-02-20T172748.csv",
                              col_names = c("var_name", "description")) %>%
    separate(description,
             into = c("total", "family", "hhldr", "children"),
             sep = "!!",
             fill = "right") %>%
    mutate(children = case_when(family == "Family households" ~ children,
                                family == "Nonfamily households" ~ hhldr),
           num_children = case_when(children == "With no children under 18 years" ~ 0,
                                    children == "With one child under 18 years" ~ 1,
                                    children == "With two children under 18 years" ~ 2,
                                    children == "With three children under 18 years" ~ 3,
                                    children == "With four or more children under 18 years" ~ 4))

  children_data <- read_csv("data/decennial-data/DECENNIALSF12010.PCT16_data_with_overlays_2021-02-20T172748.csv",
                            col_names = child_var_codes$var_name,
                            skip = 2) %>%
    pivot_longer(cols = contains("PCT"),
                 names_to = "var_name",
                 values_to = "num_hh") %>%
    left_join(child_var_codes) %>%
    filter(!is.na(num_children)) %>%
    group_by(GEO_ID, num_children) %>%
    summarize(num_hh = sum(num_hh)) %>%
    group_by(GEO_ID) %>%
    mutate(share = num_hh/sum(num_hh),
           cum_share = cumsum(share)) %>%
    filter(cum_share >= .5) %>%
    arrange(GEO_ID, cum_share) %>%
    slice(1) %>%
    mutate(GEOID = substr(GEO_ID,10,20)) %>%
    select(GEOID, children = num_children)

  acs_median_households <- tract_data %>%
    left_join(people_data) %>%
    left_join(white_hhldr_data) %>%
    left_join(homeowner_data) %>%
    left_join(num_rooms_data) %>%
    left_join(num_unemployed_data) %>%
    left_join(num_vehicles_data) %>%
    left_join(sf_detached_data) %>%
    left_join(year_built_data) %>%
    left_join(heat_data) %>%
    left_join(age_cat_data) %>%
    left_join(food_stamp_data) %>%
    left_join(tot_income_data) %>%
    left_join(educ_hhldr_data) %>%
    left_join(spouse_present_data) %>%
    left_join(children_data) %>%
    mutate(adults = people - children)

  factor_var_list <- synthetic_median_characteristics %>%
    select(where(is.factor)) %>%
    colnames()

  for (varname in factor_var_list) {
    acs_median_households <- acs_median_households %>%
      mutate(across(matches(varname), ~factor(.x,
                                              levels = levels(synthetic_median_characteristics[[varname]]),
                                              ordered = is.ordered(synthetic_median_characteristics[[varname]]))))
  }

  return(acs_median_households)
}

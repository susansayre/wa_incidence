#contains functions used by create_cex_prediction_data

#Functions to read in and process CEX files
#Many functions rely heavily on code written by Arcenis Rojas and posted on the
#CEX website at https://www.bls.gov/cex/pumd_doc.htm#2016 under sample code: R code

#' clean fmli_data
#'
#' cleans the raw fmli data and creates variables needed for the prediction step
#'
#' Specifics about how various data situations were handled:
#' The CEX suppresses state and census division idenfitication in some cases. In
#' particular, the state is suppressed for all of the psu's that are under 125k
#'
#' @param sample_cu_ids household ids for output sample
#' @param sample_newids last interview newids for output sample
#' @param fmli_input fmli input file
#' @return a cleaned fmli data set with relevant variables defined and extracted
#' @export
clean_fmli <- function(sample_cu_ids,
                       sample_newids,
                       fmli_input)
{
  ##Issues: ----
  # questions about food stamps changed between 2012 and 2013
  # in 2012, the question was "" and the mean of imputed amounts was recorded in
  # the # variable FOODSMPM
  # from 2012 - 2016, the questtion was "" and the mean of imputed amounts was
  # recorded in the variable JFS_AMTM
  #
  # In 2012, "income or loss from your own farm" was recorded in  FFRMINCM and
  # "income or loss # from your own nonfarm business, partnership, or professional
  # practice" was recorded in FNONFRMM.
  # From 2013 on, "self-employment income or loss" from "own businesses (farm or
  # non-farm) including proprietorships and partnerships" was recorded in FSMPFRXM
  #
  # In 2012, "the total amount of income from pensions or annuities from private
  # companies, military, Government, IRA, or Keogh received by ALL CU members"
  # was recorded as PENSIONM
  #intrdvx, jfs_amt, othregxm, retsurvm introduced to fmli in 2013
  #
  # heatfuel was present in 2012 survey -- might allow verification of a method
  # for guessing its value in later years
  #
  # 2015 data has a desflg variable in the fmli file that tells us whether the
  # household was sample under the 2000 design and thus includes a bounding
  # interview as interview 1 and "real" interviews 2-5 or the 2010 design where
  # the bounding interview was removed and the real interviews are 1-4

  #prepare fmli_data ----
  fmli_data <- fmli_input %>%
    #replace missing with zero. Note that a missing observation here means that the household reported no expenditures in these categories
    mutate_at(vars(fnonfrmm, ffrmincm, fsmpfrxm, fssixm, frretirm, welfarem,
                   retsurvm, intrdvxm, othrincm, totexppq, foodpq, sheltpq,
                   utilpq, gasmopq, healthpq, cashcopq, alcbevpq, educapq),
              list(~ replace(., is.na(.), 0))) %>%
    mutate(num_vehicles = vehq + vehql,
           food_stamp = case_when(jfs_amtm>0 ~ 1,
                                  foodsmpm>0 ~ 1,
                                  TRUE ~ 0),
           homeowner = case_when(as.numeric(cutenure)<=3 ~ 1,
                                 as.numeric(cutenure)>=4 ~ 0),
           tot_income = fincbtxm,
           wag_salary = fsalarym,
           has_mortgage = cutenure == 1,
           self_income = fsmpfrxm + fnonfrmm + ffrmincm,
           soc_sec_inc = frretirm + fssixm,
           pa_inc = welfarem,
           ret_inc = retsurvm,
           int_inc = intrdvxm,
           oth_inc = othrincm,
           unistrq = as.factor(unistrq),
           built = as.numeric(built),
           urban = as.numeric(bls_urbn)==1,
           region = as.numeric(region),
           region = as.factor(case_when(region == 1 ~ "northeast",
                                        region == 2 ~ "midwest",
                                        region == 3 ~ "south",
                                        region == 4 ~ "west")),
           in_msa = smsastat == 1,
           taxable_expend = totexppq - foodpq - sheltpq - utilpq - gasmopq -
             healthpq - cashcopq - alcbevpq - educapq,
           popsize = as.numeric(popsize),
           psu_pop_size = case_when(popsize == 1 ~ "over4M",
                                    popsize == 2 ~ "1.2-4M",
                                    popsize == 3 ~ "330-1200k",
                                    popsize == 4 ~ "125-330k",
                                    popsize == 5 ~ "under125k"),
           house_type = case_when(unistrq == "1" ~ "other",
                                  unistrq == "2" ~ "mobile",
                                  unistrq == "3" ~ "sf_detached",
                                  unistrq == "4" ~ "sf_attached",
                                  unistrq == "5" ~ "apts_2",
                                  unistrq == "6" ~ "apts_3_4",
                                  unistrq == "7" ~ "apts_5_9",
                                  unistrq == "8" ~ "apts_10_19",
                                  unistrq == "9" ~ "apts_20_49",
                                  unistrq == "10" ~ "apts_50"),
           sf_detached = if_else(house_type == "sf_detached", 1, 0),
           loc_type = case_when(urban == 0 ~ "rural",
                                TRUE ~ str_c("urban_",psu_pop_size))) %>%
    rename(age_hhldr = age_ref, people = fam_size,
           num_rooms = roomsq, adults64plus = persot64) %>%
    select(-one_of(c("vehq", "vehql", "jfs_amtm", "foodsmpm", "cutenure", "fnonfrmm", "ffrmincm", "fsmpfrxm"))) %>%
    right_join(sample_cu_ids)

  treat_as_factor  <-  c("house_type","ref_race","sex_ref","educ_ref", "fam_type", "building", "state","occucod1",
                         "urban", "region", "in_msa", "psu_pop_size", "loc_type", "sf_detached", "psu")

  #collapse to numeric data to single observations by averaging variables
  average_fmli <- fmli_data %>%
    group_by(cu_id) %>%
    select(-one_of(treat_as_factor),-one_of(c("newid","sourcefile","interview_num","desflg","unistrq"))) %>%
    summarize_all(mean, na.rm=T) %>%
    #cex reports house age in five year increments from 1920-1990, annual thereafter
    #pums reports house age in ten year increments from 1940-2000, five year
    #increment from 2000-2004 and yearly thereafter
    mutate(year_built = factor(case_when(built<1940 ~ "pre1940",
                                         built<1950 ~ "1940s",
                                         built<1960 ~ "1950s",
                                         built<1970 ~ "1960s",
                                         built<1980 ~ "1970s",
                                         built<1990 ~ "1980s",
                                         built<2000 ~ "1990s",
                                         built<2005 ~ "early2000s",
                                         built>=2005 ~ "post2005",
                                         is.na(built) ~ "missing"),
                               levels = c("rent", "missing", "pre1940", "1940s", "1950s",
                                          "1960s", "1970s", "1980s", "1990s",
                                          "early2000s", "post2005"),
                               ordered = T),
           # scale quarterly up to annual
           totexp = totexppq*4,
           taxable_expend = taxable_expend*4)

  final_fmli <- fmli_data %>%
    select(cu_id,newid,one_of(treat_as_factor)) %>%
    right_join(sample_newids) %>%
    left_join(average_fmli)
}

#' clean mtbi_data
#'
#' Future implementation should make the ucc table a function input
#' @param sample_cu_ids household ids for output sample
#' @param mtbi_input mtbi input file
#' @return a cleaned set of expenditures from the mtbi file based on the
#' categories and names listed in the ucc table embedded in the function
#' @export
clean_mtbi <- function(sample_cu_ids,
                       mtbi_input)
{
  # Construct expenditure variables from mtbi files ----
  # match ucc codes output category
  hh_ucc_to_var <- tribble(
    ~ucc, ~variable, ~convert_to_expense,
    220211, "property_tax", 1,
    #rent -- includes many payments made to improve the home (check pums def?)
    210110, "rent", 1,
    230121, "rent", 1,
    230141, "rent", 1,
    230150, "rent", 1,
    240111, "rent", 1,
    240121, "rent", 1,
    240211, "rent", 1,
    240221, "rent", 1,
    240311, "rent", 1,
    240321, "rent", 1,
    320611, "rent", 1,
    320621, "rent", 1,
    320631, "rent", 1,
    350110, "rent", 1,
    790690, "rent", 1,
    990920, "rent", 1,
    #mortgage includes principal & interest on mortgages, home equity loans and
    #home equity lines of credit for owned homes only,
    830201, "mortgage", -1, #mortage principal payment, recorded as neg
    830203, "mortgage", -1, #home equity load principal payment, recorded as neg
    880120, "mortgage", -1, #home equity line of credit principal, recorded as neg
    880110, "mortgage", 1, #interest on home equity line of credit
    220313, "mortgage", 1, #interest on home equity loan
    220311, "mortgage", 1, #interest on mortgage
    #homeowners insurance on owned dwellings
    220121, "home_insurance", 1,
    #estimated property value of owned home
    800721, "property_value", 1
  )

  #Note: verify that property value was divided to arrive at a "monthly" number

  mtbi_hh_data <- hh_ucc_to_var %>%
    left_join(mtbi_input) %>%
    right_join(sample_cu_ids) %>%
    group_by(cu_id, variable) %>%
    mutate(true_cost = cost*convert_to_expense) %>%
    summarize(cost = sum(true_cost)) %>%
    group_by(cu_id) %>%
    spread(variable, cost)
}

#' clean opb_data
#' @param sample_newids newids for output sample
#' @param opb_input fmli input file
#' @return cleaned opb data
#' @export
clean_opb <- function(sample_newids,
                      opb_input)
{opb_data <- opb_input %>%
  #extract only info on homes currently lived in
  filter(ownyb == "100") %>%
  mutate(tenure_year_num = as.numeric(substr(qyear,1,4)) - acquiryr) %>%
  #select(cu_id, newid, tenure_year_num, tenure_years) %>%
  right_join(sample_newids) %>%
  select(-sourcefile) %>%
  #some opb records appear twice in two different years of data
  #the following line only removes completely duplicate rows
  distinct()
}

#' clean opi_data
#' @param sample_cu_ids household ids for output sample
#' @param opi_input fmli input file
#' @return cleaned opi data
#' @export
clean_opi <- function(sample_cu_ids,
                      opi_input)
{  opi_data <- opi_input %>%
  #extract only info on homes currently lived in
  filter(ownyi == "100") %>%
  mutate(prop_value = propvalx) %>%
  select(cu_id, newid, interview_num, prop_value) %>%
  right_join(sample_cu_ids) %>%
  #some opb records appear twice in two different years of data
  #the following line only removes completely duplicate rows
  distinct() %>%
  select(cu_id, prop_value) %>%
  group_by(cu_id) %>%
  #property value estimates fluctuate within a household, average across households
  summarize_all(mean, na.rm=T)
}


#' clean rent_data
#' @param sample_cu_ids household ids for output sample
#' @param rent_input fmli input file
#' @return cleaned rent data
#' @export
clean_rent <- function(sample_cu_ids,
                       rent_input)
{  rent_data <- rent_input %>%
  mutate(electric_in_rent = 2 - as.numeric(rtelect),
         gas_in_rent = 2 - as.numeric(rtgas),
         water_in_rent = 2 - as.numeric(rtwater),
         heat_in_rent = 2  - as.numeric(rtheat)) %>%
  select(cu_id, contains("_in_rent")) %>%
  group_by(cu_id) %>%
  #note: some households have different flag values for different interviews
  #the following line computes an "average" flag
  summarize_all(mean, na.rm = T) %>%
  right_join(sample_cu_ids)
}

#' clean memi and combine cex data
#' @param sample_cu_ids cu_ids to use
#' @param cleaned_fmli cleaned fmli data
#' @param cleaned_mtbi cleaned mtbi data
#' @param cleaned_opb cleaned opb data
#' @param cleaned_rent cleaned rent data
#' @param memi_input raw memi data
#' @return a tibble containing a combined cex data set
#' @export
combine_cex_data <- function(sample_cu_ids,
                             cleaned_fmli,
                             cleaned_mtbi,
                             cleaned_opb,
                             cleaned_opi,
                             cleaned_rent,
                             memi_input)
{

  #extract data on household members from memi file ----
  memi <- memi_input %>%
    mutate(worker = case_when(is.na(earner) ~ 0,
                              earner == "2" ~ 0,
                              earner == "1" ~ 1),
           unemployed = case_when(is.na(incnonwk) ~ 0,
                                  incnonwk == "5" ~ 1,
                                  TRUE ~ 0)) %>%
    right_join(sample_cu_ids)

  hh_totals <- memi %>%
    mutate(adults = age>=18,
           adults18_44 = age>=18 & age<=44,
           adults45_64 = age>=45 & age<=64,
           children = age<18,
           children_6 = age<6,
           children6_12 = age>=6 & age<=12,
           children13_17 = age>=13 & age<=17,
           college_students = in_coll %in% c("1","2"), #includes full and part time enrollment at college or university
           num_military = arm_forc=="1",
           num_in_labor_force = worker,
           num_unemployed = unemployed) %>%
    group_by(newid, cu_id) %>%
    select(newid, cu_id,  adults:num_unemployed) %>%
    summarize_all(sum, na.rm = TRUE)

  hh_averages <- memi %>%
    mutate(adult_avg_age = replace(age, age<18, NA),
           child_avg_age = replace(age, age>=18, NA),
           worker_avg_age = replace(age, worker!=1, NA),
           avg_educ_idx = replace(educa, age<=18, NA),
           avg_hrs_worked = as.numeric(replace(inc_hrsq, worker!= 1, NA))
    ) %>%
    group_by(newid, cu_id) %>%
    select(newid, cu_id,  adult_avg_age:avg_hrs_worked) %>%
    summarize_all(mean, na.rm = TRUE)

  hh_annual_averages <- hh_totals %>%
    left_join(hh_averages) %>%
    group_by(cu_id) %>%
    select(-newid) %>%
    summarize_all(mean, na.rm = TRUE)

  # merge component datasets ----
  hh_data <- memi %>%
    #extract householders
    select(cu_id, prinearn = membno, educa, worker) %>%
    group_by(cu_id, prinearn) %>%
    summarize_all(mean, na.rm=T) %>% #computes average educa and worker values for each member
    #match primary earner with fmli data
    #right join matches only cu_id, prinearn/membno combos that appear in fmli
    right_join(cleaned_fmli) %>%
    mutate(empl_hhldr = worker) %>%
    left_join(hh_annual_averages) %>%
    #fix averages
    mutate(child_avg_age = replace(child_avg_age, children == 0, NA),
           worker_avg_age = replace(worker_avg_age, num_in_labor_force == 0, NA),
           avg_hrs_worked = replace(avg_hrs_worked, num_in_labor_force == 0, NA)) %>%
    left_join(cleaned_rent) %>%
    left_join(cleaned_opb) %>%
    left_join(cleaned_opi) %>%
    left_join(cleaned_mtbi)
}

#' clean combined cex
#' Note that this code sets prop_value and year_built on rented homes to 0. For
#' a very small subset of the rented homes, we do have actual data on these
#' values. The correct interpretation of these variables is now as an
#' interaction between the underlying data and a dummy for owned home
#' @param cex_hh_data input dataset that needs to be processed
#' @return cleaned dataset
#' @export
clean_hh_data <- function(cex_data){

  rental_data <- cex_data %>%
    filter(homeowner == 0) %>%
    mutate(tenure_years = "rent",
           mortgage = 0,
           property_tax = 0,
           home_insurance = 0,
           prop_value = 0,
           year_built = "rent"
    )

  homeowner_data <- cex_data %>%
    filter(homeowner == 1) %>%
    mutate(electric_in_rent = 0,
           gas_in_rent = 0,
           heat_in_rent = 0,
           water_in_rent = 0,
           rent = 0)

  combined_data <- rental_data %>%
    bind_rows(homeowner_data)

  cleaned_data <- combined_data %>%
    mutate(tenure_years = case_when(tenure_year_num < 1 ~ "<1",
                                    tenure_year_num < 2 ~ "1-2",
                                    tenure_year_num < 4 ~ "2-4",
                                    tenure_year_num < 10 ~ "5-9",
                                    tenure_year_num < 20 ~ "10-19",
                                    tenure_year_num < 30 ~ "20-29",
                                    homeowner == 0 ~ "rent",
                                    is.na(tenure_year_num) ~ "NA",
                                    TRUE ~ ">30")) %>%
    #   filter(!is.na(state)) %>%
    mutate(nobuild_date = is.na(built)) %>%
    #averages below will be na if there are no household members in the relevant
    #category. Set these to zero so they don't drop out of the regressions
    replace_na(list(avg_educ_idx = 0,
                    child_avg_age = 0,
                    worker_avg_age = 0,
                    avg_hrs_worked = 0,
                    built = 0))  %>%
    mutate(rac_hhldr = case_when(ref_race == "1" ~ "white",
                                 ref_race == "2" ~ "black",
                                 ref_race == "4" ~ "asian",
                                 TRUE ~ "other/multi"),
           white_hhldr = if_else(rac_hhldr == "white", 1, 0),
           sex_hhldr = sex_ref,
           educ_hhldr = educ_ref,
           educ_cat_hhldr = factor(case_when(educ_hhldr %in% c("0","10","11") ~ "no_hs",
                                             educ_hhldr == "12" ~ "hs",
                                             educ_hhldr %in% c("13", "14") ~ "some_college",
                                             educ_hhldr %in% c("15", "16") ~ "college(+)"),
                                   levels = c("no_hs", "hs", "some_college", "college(+)"),
                                   ordered = T),
           family_type = fam_type,
           spouse_present = case_when(fam_type <= 5 ~ 1, TRUE ~ 0),
           age_cat_hhldr = factor(case_when(age_hhldr <= 24 ~ "15-24",
                                            age_hhldr <= 34 ~ "25-34",
                                            age_hhldr <= 44 ~ "35-44",
                                            age_hhldr <= 54 ~ "45-54",
                                            age_hhldr <= 59 ~ "55-59",
                                            age_hhldr <= 64 ~ "60-64",
                                            age_hhldr <= 74 ~ "65-74",
                                            age_hhldr <= 84 ~ "75-84",
                                            age_hhldr >= 85 ~ "85plus"),
                                  levels = c("15-24","25-34","35-44","45-54",
                                             "55-59","60-64","65-74","75-84","85plus"),
                                  ordered = T),
           state = replace_na(state, 99),
           psu = replace_na(psu,"XXXX"))
}

#' infer heating fuel
#' @param monthly_energy_expenditures data from the cex mtbi on energy expenditures
#' @return tibble with household ids and inferred heating fuel choice
#' @export
infer_heating_fuel <- function(monthly_energy_expenditures) {
  expenditure_vars <- monthly_energy_expenditures %>%
    group_by(cu_id, ref_yr, ref_mo) %>%
    spread(key = category, value = cost, fill = 0)

  winter_summer_expend <- expenditure_vars %>%
    mutate(season = case_when(ref_mo<=4|ref_mo>=10 ~ "winter",
                              ref_mo>=5&ref_mo<=9 ~ "summer",
                              TRUE ~ "error")) %>%
    group_by(cu_id, season) %>%
    select(cu_id, season, coalwoodother, electricity, fueloil, gasbottledtank, naturalgas) %>%
    summarize_all(mean, na.rm = T) %>%
    tidyr::gather(fuel, avg_expend, -cu_id, -season) %>%
    spread(season, avg_expend) %>%
    filter(winter>0)

  winter_surplus <- winter_summer_expend %>%
    mutate(winter_surplus = winter - summer)

  heating_fuel_test <- winter_surplus %>%
    group_by(cu_id) %>%
    summarise(max_winter_surplus = max(winter_surplus)) %>%
    right_join(winter_surplus) %>%
    mutate(is_heating_fuel = as.numeric(winter_surplus == max_winter_surplus),
           fuel = str_c(fuel, "_heat")) %>%
    select(cu_id, fuel, is_heating_fuel)

  multiple_fuels <- heating_fuel_test %>%
    group_by(cu_id) %>%
    summarize(num_max = sum(is_heating_fuel)) %>%
    filter(num_max>1) %>%
    right_join(winter_surplus) %>%
    filter(!is.na(num_max)) %>%
    select(cu_id, fuel, winter_surplus) %>%
    mutate(fuel = str_c(fuel, "_winter_surplus")) %>%
    spread(fuel, winter_surplus)

  #Note: there are 83 households for which this procedure does not make a
  #prediction about heating fuel. Occurs when either summer and winter averages
  #are the same for all fuels or when two fuels have the same reported surplus.
  #In those cases, heating fuels are assigned in the order below if the fuel
  #is a possible primary heating fuel.

  heating_fuel <- heating_fuel_test %>%
    spread(fuel, is_heating_fuel, fill = 0) %>%
    mutate(heating_fuel = case_when(fueloil_heat == 1 ~ "fuel oil",
                                    gasbottledtank_heat == 1 ~ "bottled gas",
                                    coalwoodother_heat == 1 ~ "coal wood other",
                                    naturalgas_heat == 1 ~ "natural gas",
                                    electricity_heat == 1 ~ "electricity"),
           heating_fuel = as.factor(heating_fuel)
    ) %>%
    select(cu_id, heating_fuel) %>%
    mutate(electric_heat = if_else(heating_fuel == "electricity", 1, 0),
           bottled_gas_heat = if_else(heating_fuel == "bottled gas", 1, 0),
           ng_heat = if_else(heating_fuel == "natural gas", 1, 0),
           oil_heat = if_else(heating_fuel == "fuel oil", 1, 0))
}

#' identify sample newids by identifying households observed for twelve months
#' and identifying the newid corresponding to their last interview. This
#' approach implies using the houshold characteristic data from the final
#' interview
#' @param expenditure_data mbti data
#' @return table with sample_newids for last interview
#' @export
find_sample_newids <- function(expenditure_data) {
  monthly_expense <-  expenditure_data %>%
    mutate(year_mo = ref_yr*100 + ref_mo) %>%
    group_by(cu_id,year_mo,interview_num) %>%
    summarize(total_expenditures = sum(cost))

  observation_sums <- monthly_expense %>%
    group_by(cu_id) %>%
    summarize(num_months = n(), start_month = min(year_mo), end_month = max(year_mo))%>%
    filter(num_months == 12) %>%
    select(cu_id, start_month, end_month)
  #Note: there are 571 households that are observed for 13 months. All of these
  #households had their first interview observation in quarter 1 2012 and their
  #last observation in quarter 1 2013. I drop these households

  add_last_interview <- observation_sums %>%
    left_join(monthly_expense, by = c("cu_id", "end_month" = "year_mo")) %>%
    mutate(newid = str_c(cu_id,interview_num)) %>%
    select(cu_id, newid)
}

#' extract sample cu_ids from newids
#' @param sample_newids sample newids
#' @return list of sample consumer units
#' @export
find_sample_cu_ids <- function(sample_newids) {
  sample_cu_ids <- sample_newids %>%
    select(cu_id)
}

#' generate sample monthly energy expenditures
#' @param monthly_expenditure_data raw data
#' @param energy_categories categories to total
#' @param sample_cu_ids cu ids to include in sampel
#' @return energy by month for the sample ids
#' @export
compute_sample_energy_expense <- function(monthly_expenditure_data, energy_categories, sample_cu_ids) {
  sample_households <- sample_cu_ids %>%
    select(cu_id)

  energy_by_month <- monthly_expenditure_data %>%
    right_join(energy_categories) %>%
    right_join(sample_households) %>%
    filter(vacation == 0) %>%
    group_by(cu_id, category, ref_yr, ref_mo) %>%
    summarize(cost = sum(cost))
}

#' energy categories
#'
#' Load a tibble containing the ucc codes, energy type, and and a flag set to 1
#' for expenditures on fuels at vacation homes and for gasoline purchases on
#' trips -- these may not be subject to a MA carbon tax
#' @return a tibble with vars ucc (numeric), category (text), and vacation (0/1)
#' @export
load_energy_categories <- function() {tribble(
  # categories from Ummel where consumers would directly bear costs,
  # ucc codes taken from https://www.bls.gov/cex/2016/csxintvwdata.pdf
  ~ucc, ~category, ~vacation,
  #the vacation flag is set to 1 for expenditures on fuels at vacation homes
  #and for gasoline purchases on trips -- these may not be subject to a MA carbon tax
  #electricity codes from stub file (113/114 are for vacation homes)
  260111, "electricity", 0,
  260112, "electricity", 0,
  260113, "electricity", 1,
  260114, "electricity", 1,
  #gasoline -- consider excluding out of town trips (470113)
  470111, "gasoline", 0,
  470113, "gasoline", 1,
  #fuel oil (113/114 are for vacation homes
  250111, "fueloil", 0,
  250112, "fueloil", 0,
  250113, "fueloil", 1,
  250114, "fueloil", 1,
  #diesel
  470112, "diesel", 0,
  #coal, wood, and other fuels (913/914 are for vacation homes)
  250911, "coalwoodother", 0,
  250912, "coalwoodother", 0,
  250913, "coalwoodother", 1,
  250914, "coalwoodother", 1,
  #natural gas (213/214 are for vacation homes)
  260211, "naturalgas", 0,
  260212, "naturalgas", 0,
  260213, "naturalgas", 1,
  260214, "naturalgas", 1,
  #bottled or tank gas (213/214 are for vacaction homes)
  250211, "gasbottledtank", 0,
  250212, "gasbottledtank", 0,
  250213, "gasbottledtank", 1,
  250214, "gasbottledtank", 1,
  #public transportation (add 53012 (trips)? subsidy 910042)
  #categories don't include intercity trains/buses, ship fares, or airlines
  530311, "transit", 0,
  530412, "taxi", 0, #excludes trips
  530902, "school_bus", 0)}

#' verify presence of necessary cex data
#'
#' check assumes that if the mtbi file for a given exists, all files were downloaded
#' and unzipped correctly. Implies that if a function fails during download, we
#' need to delete the entire directory.
#'
#' @param cex_dir target root directory for cex data
#' @param years years of data to use
#' @export
verify_cex_dir <- function(cex_dir,years) {
  if (!file.exists(cex_dir)) {
    lapply(years, function(x) unzip_cex_data(x, cex_dir))
  } else {
    years_exist <- lapply(years, function(x) file.exists(file.path(cex_dir,as.character(x))))
    #check assumes that if a year's directory exists all files were downloaded/unzipped correctly
    unzip_years <- years[years_exist==FALSE]
    lapply(unzip_years, function(x) unzip_cex_data(x, cex_dir))
  }

  return(cex_dir)
}

#' unzip cex data
#' @param year A four digit year > 2000
#' @param cex_root_dir A folder to which the data should be unzipped
#' @return Not intended to be used with a return value, likely returns a logical
#' for whether the last unzip step succeeded
#' @export
unzip_cex_data <- function(year, cex_root_dir) {
  short_year <- year - 2000
  cex_yr_dir <- file.path(cex_root_dir, as.character(year))
  dir.create(cex_yr_dir, showWarnings = FALSE, recursive = TRUE)

  # download interview data
  cex_int <- file.path(cex_root_dir, str_c("intrvw", short_year, ".zip"))
  unzip(cex_int, exdir = cex_yr_dir, junkpaths = T)
  file.copy(file.path(cex_root_dir,"bls.gov_cex_pumd_2013_csxintstub.txt"),
            file.path(cex_yr_dir, "csxintstub.txt"))
  
  # # download diary data
  # cex_diary <- file.path(cex_yr_dir, str_c("diary", short_year, ".zip"))
  # download.file(url = str_c("https://www.bls.gov/cex/pumd/data/comma/diary",
  #                           short_year,
  #                           ".zip"),
  #               destfile = cex_diary,
  #               headers = c("User-Agent" = "custom user agent"))
  # unzip(cex_diary, exdir = cex_yr_dir, junkpaths = T)
  # 
  # download.file(url = str_c("https://www.bls.gov/cex/pumd/",
  #                           year,
  #                           "/csxintstub.txt"),
  #               destfile = file.path(cex_yr_dir,"csxintstub.txt"),
  #               headers = c("User-Agent" = "custom user agent"))

}

#' variable information for the integrated file
#' code adapted from Integrated Mean and SE.R
#' @param stub_path input file location
#' @return table with variable information from the integrated stub file
intstub <- function(stub_path) {
  # Create a temporary file on the local disk..
  tf <- tempfile()

  # Read the stub file into memory
  st <- readLines(stub_path)

  # Remove all the lines before the row with the title "Number of consumer
  # units"
  st <- st[grep("Number of consumer units", st): length(st)]

  # Replace these two tabs with seven spaces instead
  st <- gsub("\t" , "   " , st)

  # Keep only the rows which start with type "*", "1", "2" and with the
  # 4th character as a blank space or a digit from 1 through 9
  st <- st[substr(st, 1, 1) %in% c("*", 1, 2) &
             substr(st, 4, 5) %in% paste0(c(" ", 1:9), " ")]

  # save to the temporary file created above
  writeLines(st , tf)

  # Read in the cleaner version of the stub file in fixed-width format
  stub <- read_fwf(
    file = tf,
    fwf_empty(
      tf, n = 1000L,
      col_names = c("type", "level", "title", "UCC", "survey", "factor",
                    "group")
    )
  ) %>% select(-factor)

  # Concatenate the titles that run beyond 1 line into their respective first
  # lines
  for (i in seq(length(stub$type))) {
    if (stub$type[i] %in% "2") {
      l1_row <- max(which(stub$type[1:i] %in% "1"))
      stub$title[l1_row] <- paste(stub$title[l1_row], stub$title[i])
    }
  }

  # Make the names of the stub file columns lower case
  stub %<>%
    data.table::setnames(old = names(.), new = tolower(names(.))) %>%

    # Filter for rows of type "1" or "*" and filter out unnecessary rows
    filter(
      type %in% c("1", "*"),
      !title %in%
        c("Percent distribution of consumer units", "Lower limit",
          grep("weight", .$title, value = T))
    ) %>%

    # Add a row number column
    mutate(rownum = as.character(seq(nrow(.))))

  # Change the title of the row where the number of consumer units will be shown
  stub$title[stub$title == "Number of consumer units (in thousands)"] <-
    "Number of consumer units"
}

#' generate cu_id and interview num from newid
#' @param input_tibble table that needs cu_id added
#' @return output tibble with cu_id added
#' @export
create_cu_id <- function(input_tibble) {
  output <- input_tibble %>%
    mutate(newid = as.character(newid),
           #newids implicity contain a leading zero that is not present in the file
           #as a result, the cu_id is the first six digits of newid and the
           #interview num is the 7th digit
           cu_id = substr(newid, 1,6),
           interview_num = substr(newid,7,8))

}

#' reads in and stacks the mtbi files for a given year
#' @param year four digit data year
#' @param cex_dir root directory containing subdirectories with cex_data
#' @return combined data set of monthly mtbi records
#' @export
combine_mtbi <- function(year,cex_dir) {
  data_dir = file.path(cex_dir, year)

  mtbi <- lapply(dir(data_dir, pattern = "^mtbi.*[.]csv$",full.names = TRUE),
                 data.table::fread,
                 select = c("NEWID", "COST", "UCC", "REF_YR", "REF_MO"),
                 na.strings = c("", ".", "NA")
  ) %>%
    bind_rows() %>%
    # Change the column names to lower case
    data.table::setnames(old = names(.), new = tolower(names(.))) %>%
    # Filter for expenditures made in the given year
    filter(ref_yr %in% year) %>%
    # Change "newid" to a character variable
    create_cu_id()
}

#' reads in and stacks the fmli files for a given year
#'
#' Note: due to variable redefinitions, not all variables are available in each
#' year which can generate warnings when the function is called. Careful
#' treatment of each of these variables is required in clean_fmli and is
#' described in detail in the comments to that function.
#' @param year four digit data year
#' @param cex_dir root directory containing subdirectories with cex_data
#' @param varstokeep list of variables to output
#' @return combined data set of monthly mtbi records
#' @export
combine_fmli <- function(year, cex_dir, varstokeep){
  # loadRead in and stack the fmli files
  data_dir <- file.path(cex_dir, year)
  #the following line skips the fmli###x.csv files that duplicate first quarter prev year interviews
  fmli <- lapply(dir(data_dir, pattern = "^fmli[0-9]{3}[.]csv$", full.names = TRUE),
                 function(x) {
                   data.table::fread(x,
                                     select = toupper(varstokeep),
                                     na.strings = c("", ".", "NA")
                   ) %>%
                     mutate(sourcefile = x,
                            PSU = as.character(PSU))
                 }
  ) %>%
    bind_rows() %>%
    data.table::setnames(old = names(.), new = tolower(names(.))) %>%
    mutate(qintrvmo = as.numeric(qintrvmo),
           qintrvyr = as.numeric(qintrvyr),
           cex_year = year) %>%
    create_cu_id()
}

#' reads in and stacks generic cex files for a given year
#' @param filetype filename prefix
#' @param year four-digit year
#' @param cex_dir root directory containing subdirectories with cex_data
#' @param varstokeep list of variables to output
#' @return combined data set of monthly mtbi records
#' @export
read_quarterly_cex <- function(filetype, year, cex_dir, varstokeep) {
  data_dir = file.path(cex_dir, year)
  #the following line skips the XXXX###x.csv files that duplicate first quarter prev year interviews
  output_data <- lapply(dir(data_dir,
                            pattern = str_c("^", filetype, "[0-9]{3}[.]csv$"),
                            full.names = TRUE
  ),
  function(x) {
    data.table::fread(x,
                      select = toupper(varstokeep),
                      na.strings = c("", ".", "NA")
    ) %>%
      mutate(sourcefile = x)
  }
  ) %>%
    bind_rows() %>%
    data.table::setnames(old = names(.), new = tolower(names(.))) %>%
    create_cu_id()
}

#' reads in and stacks generic cex files for a given year
#' @param filetype filename prefix
#' @param year four-digit year
#' @param cex_dir root directory containing subdirectories with cex_data
#' @param varstokeep list of variables to output
#' @return combined data set of monthly mtbi records
#' @export
read_cex <- function(filetype, year, cex_dir, varstokeep) {
  data_dir = file.path(cex_dir, year)
  output_data <- lapply(dir(data_dir,
                            pattern = str_c("^", filetype, ".*[.]csv$"),
                            full.names = TRUE
  ),
  function(x) {
    data.table::fread(x,
                      select = toupper(varstokeep),
                      na.strings = c("", ".", "NA")
    ) %>%
      mutate(sourcefile = x)
  }
  ) %>%
    bind_rows() %>%
    data.table::setnames(old = names(.), new = tolower(names(.))) %>%
    create_cu_id()
}

#functions not specific to pums or cex data
#
#

#' read-in energy prices downloaded from EIA
#'
#' @return price data
#' @export
read_energy_prices <- function(energy_price_file){
  energy_prices <- read_csv(energy_price_file,
                            col_names = c("year_name","month","series","price"),
                            col_types = "cicd",
                            skip = 1) %>%
    mutate(year = as.numeric(year_name))

}

#' read-in and combine state energy prices --
#' @param state_price_match a state by state match for which price series to use
#' @param price_data a character variable containing the name of a csv file
#' containing the actual price data
#' @return files with energy prices by state and month
#' @export
load_energy_prices <- function(state_price_match_file, price_data) {

  state_price_match <- read_csv(state_price_match_file)
  energy_prices <- read_csv(price_data)

  state_energy_prices <- state_price_match %>%
    mutate(series = str_replace(electricity_price_series,"-",".")) %>%
    inner_join(energy_prices) %>%
    rename(electricity_price = price) %>%
    left_join(energy_prices, by = c("natural_gas_price_series" = "series", "year", "month")) %>%
    rename(natural_gas_price = price) %>%
    left_join(energy_prices, by = c("propoane_price_series" = "series", "year", "month")) %>%
    rename(propane_price = price) %>%
    left_join(energy_prices, by = c("heating_oil_price_series" = "series", "year", "month")) %>%
    rename(heating_oil_price = price) %>%
    left_join(energy_prices, by = c("gasoline_price_series" = "series", "year", "month")) %>%
    rename(gasoline_price = price)

  us_prices <- state_energy_prices %>%
    filter(state == "US") %>%
    select(year, month, us_heating_oil = heating_oil_price, us_propane = propane_price)

  replace_missing <- state_energy_prices %>%
    left_join(us_prices) %>%
    mutate(heating_oil_price = case_when(is.na(heating_oil_price) ~ us_heating_oil,
                                         TRUE ~ heating_oil_price),
           propane_price = case_when(is.na(propane_price) ~ us_propane,
                                     TRUE ~ propane_price)) %>%
    select(-us_heating_oil, -us_propane)
}

#' Download degree days
#'
#' Downloads degree days from noaa website
#' @param target_dir location to store files
#' @param download logical to allow re-using download, default is TRUE
#' @return two element list with
#' first element containing the name of the cdd file and
#' second element containing the name of the hdd file
#' @export
download_degree_days <- function(target_dir, download=TRUE) {
  destfile_cdd = file.path(target_dir,"cdd.txt")
  destfile_hdd = file.path(target_dir,"hdd.txt")

  if (download | !file.exists(destfile_cdd) | !file.exists(destfile_hdd)){

    dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

    procdate <- read_lines("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/procdate.txt")
    #note: these files change names on noaa website regularly and will need manual updating if out of date
    download.file(url = str_c("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-cddcst-v1.0.0-", procdate),
                  destfile = destfile_cdd)

    download.file(url = str_c("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-hddcst-v1.0.0-", procdate),
                  destfile = destfile_hdd)
  }
  download_degree_days = c(destfile_cdd,destfile_hdd)
}

#' get heating and cooling degree days
#' @param degree_days_files character list containing two elements
#' first element is cooling degree file, second is heating degree file
#' @return tibble with columns year month hdd, cdd, state_name, state
#' @export
load_degree_days <- function(degree_days_dir) {
  #read in  heating and cooling degree data
  cdd <- read_table(file.path(degree_days_dir,"cdd.txt"), col_names = c("code", month.abb)) %>%
    mutate(state_code = substr(code,1,3),
           divno = substr(code,4,4),
           element = substr(code,5,6),
           year = substr(code,7,10)) %>%
    select(all_of(month.abb), state_code, year) %>%
    tidyr::gather(key = "month_name", value = "cdd", month.abb)

  hdd <- read_table(file.path(degree_days_dir, "hdd.txt"), col_names = c("code", month.abb)) %>%
    mutate(state_code = substr(code,1,3),
           divno = substr(code,4,4),
           element = substr(code,5,6),
           year = substr(code,7,10)) %>%
    select(month.abb, state_code, year) %>%
    tidyr::gather(key = "month_name", value = "hdd", month.abb)

  state_match <- readLines("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/state-readme.txt")
  start_num <- grep("Alabama", state_match)
  stop_num <- grep("New Mexico", state_match)

  state_codes <- read_fwf("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/state-readme.txt",
                          fwf_positions(c(11,15,31,35), end = c(13,30,33,NA)),
                          skip = start_num-2, n_max = 30)

  state_codes <- state_codes %>%
    select(state_code = X1, state_name = X2) %>%
    bind_rows(state_codes %>%
                select(state_code = X3, state_name = X4)) %>%
    filter(as.numeric(state_code)<100 | as.numeric(state_code == "110"))

  degree_days <- cdd %>%
    full_join(hdd) %>%
    left_join(state_codes) %>%
    left_join(tibble(month_name = month.abb,
                         month = 1:12)) %>%
    select(year, month, hdd, cdd, state_code, state_name) %>%
    mutate(year = as.numeric(year)) %>%
    left_join(tibble(state_name = state.name,
                     state = state.abb)) %>%
    mutate(state_name = replace(state_name, state_code == 110, "United States"),
           state = replace(state, state_code == 110, "US")) %>%
    filter(!is.na(state))
}

#' combine datasets
#' @param cex_hh_data dataframe of household cex data
#' @param energy_by_month dataframe of cex expenditures on energy by month
#' @param degree_days dataframe of degree days by state
#' @param state_energy_prices dataframe of energy prices by state
#' @param analysis_variables dataframe of variables to output
#' @return cleaned data ready for analysis
#' @export
combine_prediction_datasets <- function(cex_hh_data,
                                        energy_by_month,
                                        degree_days,
                                        state_energy_prices,
                                        heating_fuels){
  cu_id_states <- cex_hh_data %>%
    select(cu_id, fips = state) %>%
    filter(!fips %in% c(15,2))

  monthly_vars <- energy_by_month %>%
    group_by(cu_id, ref_yr, ref_mo) %>%
    spread(key = category, value = cost, fill = 0) %>%
    right_join(cu_id_states) %>%
    filter(!is.na(ref_mo)) %>%
    left_join(state_energy_prices,
              by = c("fips", "ref_mo" = "month", "ref_yr" = "year")) %>%
    #pull VA hdd/cdd for dc and national for unknown locations
    mutate(state = case_when(fips == 99 ~ "US",
                             fips == 11 ~ "VA",
                             TRUE ~ state)) %>%
    left_join(degree_days,
              by = c("state", "ref_mo" = "month", "ref_yr" = "year"))

  annual_energy_expend <- monthly_vars %>%
    ungroup() %>%
    select(cu_id, coalwoodother:transit) %>%
    group_by(cu_id) %>%
    summarize_all(sum, na.rm = T)

  average_vars <- monthly_vars %>%
    ungroup() %>%
    select(cu_id, electricity_price, natural_gas_price, propane_price,
           heating_oil_price, gasoline_price, hdd, cdd) %>%
    group_by(cu_id) %>%
    summarize_all(mean, na.rm = T)

  all_data <- annual_energy_expend %>%
    left_join(cex_hh_data) %>%
    left_join(average_vars) %>%
    left_join(heating_fuels)
}

#' find predictor vars
#' @param analysis_variables tibble of analysis variables categorized
#' @return list of predictor variable names
#' @export
find_predictor_vars <- function(analysis_variables) {
  predictor_variables <- analysis_variables %>% filter(cat=="P")
  predictor_var_list <- predictor_variables$var_name
}

#' find dependent vars
#' @param analysis_variables tibble of analysis variables categorized
#' @return list of dependent variable names
#' @export
find_dependent_vars <- function(analysis_variables) {
  dependent_variables <- analysis_variables %>% filter(cat == "O")
  dependent_var_list <- dependent_variables$var_name
}

#' prune prediction data to valid subset
#' @param analysis_variables tibble of variables categorized by type and use
#' @param prediction_data data to use in fitting system
#' @param remove_missing option to remove data with missing observations for any needed data (default = F)
#' @param remove_outliers optional percentage of outliers to remove, can be either a single two-element numeric vector that is applied to all dependent variables or a named list of two-element vectors that are independently applied to each dependent variable. In all cases, the first element covering the percentage to eliminate at the lower end and the second element is the percentage to eliminate at the upper end
#' @return a dataset with the subset of the observations in prediction_data
#' for which all of the variables listed in analysis_variables are non-missing
#' @export
prune_data <- function(analysis_variables, prediction_data,
                       remove_missing = F,
                       remove_outliers = c()){
  predictor_variables <- analysis_variables %>%
    filter(cat == "P")

  predictor_var_list <- predictor_variables$var_name

  dependent_variables <- analysis_variables %>% filter(cat == "O")
  dependent_var_list <- dependent_variables$var_name

  factor_vars <- variables_to_use %>%
    filter(type %in% c("F","O"))

  final_data <- all_data %>%
    mutate(dplyr::across(all_of(factor_vars$var_name), as_factor),
           dplyr::across(tidyselect::everything(), ~ replace(., is.nan(.), NA)))

  needed_data <- prediction_data %>%
    ungroup() %>%
    select(one_of(dependent_var_list),one_of(predictor_var_list))

  if (length(remove_outliers)>0) {

    for (i in dependent_var_list) {

      remove_shr <- tryCatch({remove_outliers[[i]]}, error = function(e) {remove_outliers})

      pvals <- c(remove_shr[1], 1 - remove_shr[2])
      bounds <- quantile(needed_data[[i]], pvals, na.rm = T)

      needed_data[[i]] <- replace(needed_data[[i]], needed_data[[i]] < bounds[1], NA)
      needed_data[[i]] <- replace(needed_data[[i]], needed_data[[i]] > bounds[2], NA)

    }

  }

  if (remove_missing) {
    needed_data <- needed_data %>%
      drop_na()
  }

  return(needed_data)
}

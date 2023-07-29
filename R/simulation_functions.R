#' functions used in simulations
#'
#' #'compute income quintiles
#'@param synthetic_households data on synthetic household including weights and income
#'@return state level income quintile breakpoints
#'@export
compute_income_quintiles <- function(households){

  total_weight <- households %>%
    summarize(tot_wgt = sum(final_weight))

  ordered_income <- households %>%
    mutate(wgt = final_weight/total_weight$tot_wgt) %>%
    arrange(tot_income) %>%
    mutate(cumwgt = cumsum(wgt))

  percentiles <- ordered_income %>%
    mutate(income_quintile = case_when(cumwgt<=.2 ~ "bottom",
                                       cumwgt<=.4 ~ "second",
                                       cumwgt<=.6 ~ "middle",
                                       cumwgt<=.8 ~ "fourth",
                                       cumwgt<=1 ~ "top")) %>%
    select(-cumwgt, -wgt)
}

#'attach extra variable by location
#'@param synthetic_households synthetic households returned by raking process
#'@param degree_days dataset of degree days
#'@param state_energy_prices dataset of state energy prices
#'@param stateAbb state to use
#'@param simulation_year year to simulate
#'@return combined dataset
#'@export
attach_locVars <- function(synthetic_households, degree_days, state_energy_prices, stateAbb, simulation_year){

  dd_price <- degree_days %>%
    right_join(state_energy_prices) %>%
    filter(state == toupper(stateAbb), year == simulation_year) %>%
    select(hdd,cdd,contains("price")) %>%
    mutate(hdd = ifelse(hdd == -9999, NA, hdd),
           cdd = ifelse(cdd == -9999, NA, cdd)) %>%
    select(-contains("price_series"),-contains("price_type")) %>%
    summarize_all(mean, na.rm = T)  %>%
    merge(synthetic_households) %>%
    mutate(state = factor(substr(GEOID,1,2)))
}

#' create useful subset of synthetic household data
#'@param synthetic_households synthetic households w characteristics
#'@param analysis_variables list of variables that may be used in prediction relationships
#'@param predictor_info name of file containing information on factor levels
#'@param fit_dir directory containing fit info including factor levels
#'@return usable data set with all non-missing predictor variables
#'@export
create_useful_data <- function(synthetic_households, analysis_variables, predictor_info, fit_dir) {

  needed_variables <- analysis_variables %>%
    filter(cat %in% c("P", "S"))
  var_list <- needed_variables$var_name

  useful_data <- synthetic_households %>%
    select(one_of(var_list), hhkey, GEOID, final_weight)

  factor_levels <- readRDS(predictor_info)

  #ordered factors
  for (varname in names(factor_levels$ordered_var_levels)) {
    useful_data <- useful_data %>%
        mutate(across(matches(varname),
                      ~factor(.x,
                              levels = factor_levels$ordered_var_levels[[varname]],
                              ordered = T)))
    }

  #other factors
  for (varname in names(factor_levels$factor_var_levels)) {
    useful_data <- useful_data %>%
      mutate(across(matches(varname),
                    ~factor(.x,
                            levels = factor_levels$factor_var_levels[[varname]])))
  }

  return(useful_data)
}

#'compute median/modal value of characteristics
#'@param useful_data household level data to summarize
#'@param analysis_variables list of variables identifying types
#'@return dataset with one household for each GEOID representing the median/modal value of each characteristic
#'@export
find_median_characteristics <- function(useful_data, analysis_variables){

  GEOID_wgts <- useful_data %>%
    group_by(GEOID) %>%
    summarize(final_weight = sum(final_weight))

  numeric_vars <- analysis_variables %>%
    filter(cat %in% c("S", "P") & type == "N")

  numeric_medians <- purrr::map(numeric_vars$var_name,
                                function(.x){compute_GEOID_wgt_median(useful_data, .x)}) %>%
    reduce(left_join)

  factor_vars <- analysis_variables %>%
    filter(cat %in% c("S", "P") & type == "F")

  modal_values <- purrr::map(factor_vars$var_name,
                             function(.x){compute_GEOID_wgt_mode(useful_data, .x)}) %>%
    reduce(left_join)

  median_characteristics <- modal_values %>%
    left_join(numeric_medians) %>%
    left_join(GEOID_wgts)

  ordered_factors <- analysis_variables %>%
    filter(cat %in% c("S", "P") & type == "O" & var_name != "year_built")

  homeowner_ordered <- analysis_variables %>%
    filter(cat %in% c("S", "P") & type == "O" & var_name == "year_built")

  if(!is_empty(ordered_factors$var_name)){
    median_data <- purrr::map(ordered_factors$var_name,
                              function(.x){compute_GEOID_wgt_ordered_median(useful_data, .x)}) %>%
      reduce(left_join) %>%
      right_join(median_characteristics)} else {
        median_data <- median_characteristics
      }

  if(!is_empty(homeowner_ordered$var_name)) {
    homeowner_data <- useful_data %>%
      filter(year_built != "rent")

    median_data <- purrr::map(homeowner_ordered$var_name,
                              function(.x){compute_GEOID_wgt_ordered_median(homeowner_data, .x)}) %>%
      reduce(left_join) %>%
      right_join(median_data)} else {
        median_data <- median_data
      }


  binary_vars <- analysis_variables %>%
    filter(cat %in% c("S","P") & type == "B")

  if(!is_empty(binary_vars$var_name)){
    median_characteristics <- useful_data %>%
      mutate(across(all_of(binary_vars$var_name), ~ .x*final_weight)) %>%
      group_by(GEOID) %>%
      summarize(across(all_of(c(binary_vars$var_name, "final_weight")), ~sum(.x, na.rm = T))) %>%
      mutate(across(all_of(binary_vars$var_name), ~ .x/final_weight)) %>%
      select(-final_weight) %>%
      right_join(median_data)} else {
        median_characteristics <- median_data
      }

  return_data <- median_characteristics %>%
    ungroup() %>%
    mutate(hhkey = row_number())

}

#'compute weighted GEOID mode
#'@param dataset data to summarize
#'@param varname name of variable to summarize
#'@return dataframe with GEOID and median value
#'@export
compute_GEOID_wgt_mode <- function(useful_data, varname){

  modal_value <- useful_data %>%
    group_by(across(one_of("GEOID", varname))) %>%
    summarize(freq = sum(final_weight)) %>%
    arrange(GEOID, desc(freq)) %>%
    slice(1) %>%
    select(GEOID, matches(varname))
}

#'compute weighted GEOID "median" from an ordered factor variable
#'@param dataset dataset to compute median for
#'@param varname name of variable whose median we want to compute
#'@return dataframe with GEOID and median value
#'@export

compute_GEOID_wgt_ordered_median <- function(useful_data, varname){

  median <- useful_data %>%
    select(GEOID, matches(varname), final_weight) %>%
    arrange(GEOID, across(matches(varname))) %>%
    group_by(GEOID) %>%
    mutate(cum_shr = cumsum(final_weight)/sum(final_weight)) %>%
    filter(cum_shr <= .5) %>%
    arrange(GEOID, desc(cum_shr)) %>%
    slice(1) %>%
    select(GEOID, matches(varname))
}

#'compute weighted GEOID median
#'
#'Uses the first household whose weight pushes us above 50% as the median
#'because some numeric variables need to be integers to work properly
#'@param dataset dataset to summarize
#'@param varname variable to summarize
#'@return dataframe with GEOID and median value
#'@export

compute_GEOID_wgt_median <- function(useful_data, varname){

  cum_wgts <- useful_data %>%
    select(GEOID, matches(varname), final_weight) %>%
    arrange(GEOID, across(matches(varname))) %>%
    group_by(GEOID) %>%
    mutate(cum_shr = cumsum(final_weight)/sum(final_weight))

  above_median <- cum_wgts %>%
    filter(cum_shr >= .5) %>%
    arrange(GEOID, cum_shr) %>%
    slice(1) %>%
    select(GEOID, matches(varname))
}

#'predict mean expenditure for synthetic households
#'@param useful_data synthetic households w characteristics
#'@param cex_fit fit object recognized by predict
#'@return datasets with predicted expenditure by household
#'@export
predict_mean_consumption <- function(useful_data, cex_fit) {

  data_IDs <- useful_data %>%
    select(hhkey, GEOID, final_weight)

  expected_consumption <- predict(cex_fit, newdata = useful_data) %>%
    bind_cols(data_IDs) %>%
    tidyr::gather(key = "product", value = "expected_consumption", -hhkey, -GEOID, -final_weight) %>%
    mutate(product = str_replace(product,".pred",""))

}

#' scale consumption
#' @param consumption unscaled consumption
#' @param scale_factors product specific scale factors
#' @return scaled consumption values
#' @export
scale_consumption <- function(consumption, scale_factors){

    scaled_consumption <- consumption %>%
      left_join(scale_factors) %>%
      mutate(consumption = consumption * replace_na(scale_factor,1)) %>%
      select(-scale_factor)

    if (!("final_weight" %in% colnames(scaled_consumption))) {
      scaled_consumption <- scaled_consumption %>%
        mutate(final_weight = 1)
    }

    return(scaled_consumption)
}

#' compute scale factors
#' @param consumption dataset of predicted consumption
#' @param state_total_file name of file containing state totals
#' @return scale_factors tibble of product specific scale factors
#' @export
compute_scale_factors <- function(consumption, state_total_file){

  if (!("case_id" %in% colnames(consumption))) {
    consumption <- consumption %>%
      mutate(case_id = 1)
  }
    state_totals = read_csv(state_total_file)

  if (!("final_weight" %in% colnames(consumption))) {
    consumption <- consumption %>%
      mutate(final_weight = 1)
  }
    prediction_ratios <- consumption %>%
      group_by(product, case_id) %>%
      summarize(total = sum(consumption*final_weight/1000000), na.rm = T) %>%
      left_join(state_totals) %>%
      mutate(scale_factor = ifelse(total > 0, sales_million/total, 1)) %>%
      select(product, scale_factor, case_id)

}

#' load tax values
#'
#'
#' @param tax_amount tax size in dollars per metric ton of Co2e
#' @param tax_type an indicator used to run different tax types. Since code was
#' originally written to apply to Massachusetts, which participates in RGGI and
#' to analyze bills that exempt electricity, default is *not* to tax electricity.
#' If tax_type = "net_price" then electricity sales will
#' be charged based on marginal emission rates, CO2 prices, and electricity
#' prices stored in the list electric_info, which must include named elements
#' * marginal_emissions_rate
#' * co2 price
#' * price
#' If tax_type = "electric_varies" then electricity percent increases vary across GEOIDs.
#' In this case, electric_info must be tibble containing the following columns
#' * GEOID : a list of the GEOIDs at which analysis will be conducted
#' * carbon_content : average carbon content per kwh of electricity sold in the GEOID
#' * price : average price per kwh of electricity sold in the GEOID
#' @return tibble of percentage increases in price due to carbon tax. If `tax_type` =
#' "electric_varies" then it has four columns: product, GEOID, percent_increase, tax_type.
#' In other cases, it has only three columns, omitting product. Downstream merges with
#' sample consumption will be by either product or product/GEOID
#' @export
load_tax_values <- function(tax_amount = 20, state_prices, state_name, sim_year, tax_type = "noE", electric_info) {

  non_electric_increase <- state_prices %>%
    filter(state == toupper(state_name), year == sim_year) %>%
    select(ends_with("price")) %>%
    summarize_all(mean, na.rm = T) %>%
    # emission factors from https://www.eia.gov/environment/emissions/co2_vol_mass.php
    # converted from kg/unit to metric tons per unit by dividing by 1000
    mutate(naturalgas = tax_amount*0.05487/natural_gas_price,
           gasoline = tax_amount*0.0081/gasoline_price,
           fueloil = tax_amount*0.0119/heating_oil_price,
           gasbottledtank = tax_amount*0.00575/propane_price,
           merge_col = "a") %>%
    select(-contains("price"))

  switch(tax_type,

         #charge tax net of any carbon price embedded in retail electricity
         netPrice = {
           wide_tax_values <- non_electric_increase %>%
             select(-merge_col) %>%
             mutate(electricity = electric_info["marginal_emission_rate"]*(tax_amount - electric_info["co2_price"]/electric_info["price"]))
         },

         #default is to exclude electricity
         noE = {
           wide_tax_values <- non_electric_increase %>%
             select(-merge_col)
           },

         wa = {

           #compute percent increase in electricity price due to carbon cost
           electric_percent_increase <- electric_info %>%
             mutate(electricity = tax_amount*carbon_content/price,
                    merge_col = "a") %>%
             select(GEOID, electricity, merge_col)

          wide_tax_values <-  non_electric_increase %>%
             left_join(electric_percent_increase) %>%
             select(-merge_col)
         },

         error(str_c("Tax type ", tax_type, " not recognized"))
  )

  final_tax_values <- wide_tax_values %>%
    pivot_longer(cols = -contains("GEOID"),
                 names_to = "product",
                 values_to = "percent_increase") %>%
    mutate(tax_type = !!tax_type)

  if ("GEOID" %in% colnames(final_tax_values)) {
    final_tax_values <- final_tax_values %>%
      mutate(GEOID = as.character(GEOID))}

  return(final_tax_values)
}

#' draw ml errors
#' @param expected_consumption  synthetic sample households (one row per hh)
#' @param errors error_distribution
#' @param sample_id sample number (stored in output)
#' @return list of errors by product, hhkey, and GEOID (num products rows per hh)
#' @export
draw_errors.ml <- function(expected_consumption, errors, sample_id) {

  products <- str_remove(unique(errors$product),"test_")
  num_errors <- nrow(errors)/length(products)

  draw_values <- runif(nrow(expected_consumption)/length(products), 0, 1)
  error_id_list <- floor(draw_values*num_errors) + 1

  error_rows <- errors %>%
    select(product, error, error_id) %>%
    pivot_wider(names_from = product, values_from = error)

  hh_list <- expected_consumption %>%
    filter(product == products[[1]]) %>%
    mutate(error_id = error_id_list) %>%
    select(hhkey, GEOID, final_weight, error_id) %>%
    left_join(error_rows) %>%
    pivot_longer(cols = one_of(products), names_to = "product", values_to = "error")

  sample_w_error <- expected_consumption %>%
    right_join(hh_list) %>%
    mutate(consumption = expected_consumption + error,
           sample_id = sample_id) %>%
    select(product, consumption, hhkey, GEOID, final_weight)

}

#' uses variance matrix from sur regression to construct sample consumption
#' normal distribution for each household
#' @param expected_consumption dataset of predicted consumption levels for synthetic households
#' @param cex_fit a systemfit object with a residual covariance matrix
#' @param sample_id the sample name (begins with the word consumption on input)
#' @return sample consumption prediction for synthetic households
#' @export
draw_sample_consumption.fit_sur <- function(expected_consumption, cex_fit, sample_id) {
  num_output_vars <- nrow(cex_fit$residCov)
  draw_errors <- as_tibble(MASS::mvrnorm(n = nrow(expected_consumption)/num_output_vars, mu = rep(0,num_output_vars), Sigma = cex_fit$residCov))

  draw_errors$hhID <- 1:nrow(draw_errors)

  errors <- draw_errors %>%
    group_by(hhID) %>%
    tidyr::gather(key = "product", value = "error", -hhID)

  #next line relies on the fact that both expected consumption and errors are sorted by product then by hhkey/GEOID or hhID
  errors$hhkey <- expected_consumption$hhkey
  errors$GEOID <- expected_consumption$GEOID

  sample_consumption <- errors %>%
    ungroup() %>%
    select(-hhID) %>%
    left_join(expected_consumption) %>%
    mutate(consumption = expected_consumption + error,
           sample_ID = str_remove(!!sample_id,"consumption_"))
}

#' compute tax payments for a sample
#' @param sample_consumption dataset of sample consumption levels by product,
#' household, and GEOID
#' @param tax_values tibble of percent increases by product (and optionally by GEOID)
#' @param elasticity optional tibble containing elasticity estimates that can vary by product and GEOID
#' @return household tax payments by product
#' @export
compute_tax_payment <- function(sample_consumption, tax_values, elasticity = NULL){

 base_tax_payment <- sample_consumption %>%
    left_join(tax_values)  %>%
    mutate(tax_payment = consumption*percent_increase) %>%
    filter(percent_increase > 0)

 if (!is.null(elasticity)) {
   base_tax_payment <- base_tax_payment %>%
     left_join(elasticity) %>%
     mutate(cs_triangle = -.5*elasticity*percent_increase*tax_payment,
            tax_payment = tax_payment*(1 + elasticity*percent_increase))
 }

 keys <- intersect(c("hhid", "hhkey", "GEOID"), colnames(base_tax_payment))

 expenditure_data <- sample_consumption %>%
   select(one_of(keys), product, consumption) %>%
   filter(product %in% c("totexp", "taxable_expend")) %>%
   pivot_wider(id_cols = one_of(keys),
               names_from = "product",
               values_from = consumption,
               values_fill = 0)

 wide_tax_payment <- base_tax_payment %>%
   select(one_of(keys), product, tax_payment) %>%
   pivot_wider(id_cols = one_of(keys),
               names_from = "product",
               names_prefix = "tax_",
               values_from = tax_payment,
               values_fill = 0)

 final_tax_payments <- base_tax_payment %>%
   group_by(across(one_of(keys))) %>%
   summarize(tax_payment = sum(tax_payment)) %>%
   left_join(wide_tax_payment) %>%
   left_join(expenditure_data)
}

#' compute sample impacts for a policy that allocates all tax payments to per household rebates.
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @param state_totals (optional) state totals from alternate dataset
#' @return household level impacts
#' @export
compute_sample_impacts.pHHr <- function(sample_tax_payment,
                                        household_data,
                                        state_total = NULL){

  household_stats <- synchronize_key_wgt(sample_tax_payment, household_data) %>%
    select(hhid, GEOID, final_weight, tax_payment,
           people, adults, tot_income, children, spouse_present, totexp, taxable_expend)

  if (is.null(state_total)) {
    state_total <- compute_state_totals.pHHr(household_stats)
  }
  eitc_data <- tribble(
    ~dependents, ~credit_rate, ~max_credit, ~phase_out_rate, ~base_phase_out,
    0,           .0765,          496,       .0765,            8110,
    1,           .34,           3305,       .1598,           17830,
    2,           .40,           5460,       .2106,           17830,
    3,           .45,           6143,       .2106,           17830
  )

  #2016 federal poverty levels
  poverty_levels <- tribble(
    ~people, ~poverty_level,
    1,        11880,
    2,        16020,
    3,        20160,
    4,        24300,
    5,        28440,
    6,        32580,
    7,        36730,
    8,        40890
  )

  household_rebate <- household_stats %>%
    mutate(sales_tax_savings = 121.59*(0.001 * tot_income)^0.6919/8.95,
           #note: calculation assumes that dependents = children under 18 and all married couples living together file jointly
           dependents = pmin(children,3),
           tax_status = ifelse(spouse_present, "married", "single")) %>%
    left_join(eitc_data) %>%
    mutate(phase_out = ifelse(tax_status == "married", base_phase_out + 5430, base_phase_out),
           base_eitc = pmin(max_credit, pmax(0, credit_rate * tot_income)),
           eitc = pmax(0, base_eitc - pmax(0, tot_income - phase_out)*phase_out_rate),
           wf_tax_exemption = .25*eitc,
           rebate = state_total$tax_payments/state_total$num_hh,
           netGain = rebate - tax_payment,
           poverty_level = as.numeric(case_when(people == 1 ~ 11880,
                                                people == 2 ~ 16020,
                                                people == 3 ~ 20160,
                                                people == 4 ~ 24300,
                                                people == 5 ~ 28440,
                                                people == 6 ~ 32580,
                                                people == 7 ~ 36730,
                                                people == 8 ~ 40890,
                                                people > 8 ~ 40890 + 4160*(people - 8))),
           income_denom = pmax(tot_income, poverty_level),
           pctGain = netGain/income_denom,
           pctExpend = netGain/totexp)
}

#' compute state totals from an existing sample impacts for use in comparisons
#' @param sample_impacts data containing pre-calculated state totals
#' @return state totals
#' @export
compute_state_totals.pHHr <- function(sample_impacts){
  state_totals <- sample_impacts %>%
    ungroup() %>%
    summarize(num_hh = sum(final_weight),
              tax_payments = sum(tax_payment*final_weight))
}

#' compute sample impacts for a policy that allocates all tax payments to per household rebates.
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @param state_totals (optional) state totals from alternate dataset
#' @return household level impacts
#' @export
compute_sample_impacts.pPPr <- function(sample_tax_payment, household_data, state_totals = NULL){

  household_stats <- synchronize_key_wgt(sample_tax_payment, household_data) %>%
    select(hhid, GEOID, final_weight, tax_payment,
           people, adults, tot_income, children, spouse_present)

  if (is.null(state_totals)) {
    state_totals <- compute_state_totals.pPPr(household_stats)
  }

  household_rebates <- household_stats %>%
    mutate(rebate = state_totals$tax_payments/state_totals$num_people*(adults + 0.5*children),
           netGain = rebate - tax_payment,
           poverty_level = as.numeric(case_when(people == 1 ~ 11880,
                                                people == 2 ~ 16020,
                                                people == 3 ~ 20160,
                                                people == 4 ~ 24300,
                                                people == 5 ~ 28440,
                                                people == 6 ~ 32580,
                                                people == 7 ~ 36730,
                                                people == 8 ~ 40890,
                                                people > 8 ~ 40890 + 4160*(people - 8))),
           income_denom = pmax(tot_income, poverty_level),
           pctGain = netGain/income_denom)
}

#' compute state totals from an existing sample impacts for use in comparisons
#' @param sample_impacts data containing pre-calculated state totals
#' @return state totals
#' @export
compute_state_totals.pPPr <- function(sample_impacts){
  state_totals <- sample_impacts %>%
    ungroup() %>%
    summarize(num_people = sum((adults + 0.5*children)*final_weight),
              tax_payments = sum(tax_payment*final_weight))
}
#' Synchronize keys and weights
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics needed for computing rebates
#' @return combined data with proper hhid and final_weight
#' @export
synchronize_key_wgt <- function(sample_tax_payment, household_data){

  if ("hhid" %in% colnames(sample_tax_payment)) {
    #randomization was conducted individually for each replicant of a household
    # when computing consumption
    #household key is hhid and final_weight should be 1

    household_stats <- sample_tax_payment %>%
      left_join(household_data) %>%
      ungroup() %>%
      select(-hhkey) %>%
      mutate(final_weight = 1)
  } else {
    #randomization was either not done or a single value drawn for all replicants of a hhkey
    #hhid should be set to hhkey and final_weight comes from raking
    household_stats <- sample_tax_payment %>%
      left_join(household_data) %>%
      mutate(hhid = hhkey) %>%
      ungroup() %>%
      select(-hhkey)
  }

  return(household_stats)
}

#' add location variable
#' @param sample_impacts input sample impacts
#' @param geoType geography to group on
#' @return sample_impacts with added \code{location} column
#' @export
add_location <- function(sample_impacts,geoType){

  switch(geoType,
         state = {sample_impacts <- sample_impacts %>%
           mutate(location = "state")},
         incQuintile = {
           sample_impacts <- compute_income_quintiles(sample_impacts) %>%
             mutate(location = str_c(GEOID,"_",income_quintile))
         },
         rakeLevel = {sample_impacts$location = sample_impacts$GEOID},
         stop(str_c("geoType ", geoType, " not recognized"))
  )

  sample_impacts

}

#' compute GEOID summary stats for a given sample
#' @param sample_impacts computed impacted on given households for a given sample
#' @param weightType how to summarize: HHwgt (by household), PPwgt (by person),
#' VTwgt (by adult = potential voter)
#' @param summaryVar name of variable to summarize
#' @param geoType geography to group on
#' @param caseID identifier for the case
#' @return summary stats
#' @export
rank_impacts <- function(sample_impacts, weightType, summaryVar, geoType){

  sample_impacts <- add_location(sample_impacts, geoType) %>%
    rename(impact = !!summaryVar)

  switch(weightType,
         HHwgt ={
           weighted_impacts <- sample_impacts %>%
             mutate(stat_weight = final_weight)
         },
         PPwgt = {
           weighted_impacts <- sample_impacts %>%
             mutate(stat_weight = final_weight*people)
         },
         VTwgt = {
           weighted_impacts <- sample_impacts %>%
             mutate(stat_weight = final_weight*adults)
         },
         stop(str_c("weight ", weight, " not recognized")))

  total_weight <- weighted_impacts %>%
    #group by variables include location and other sub categories, using
    #contains() lets the code work even if the variables aren't there (e.g.
    #product which will only be there for the datasets looking at by product
    #taxes)
    group_by_at(vars(location, contains("_type"), contains("product"), contains("_ID", ignore.case = F))) %>%
    summarize(tot_wgt = sum(stat_weight))

  ordered_impacts <- weighted_impacts %>%
    left_join(total_weight) %>%
    group_by(location) %>%
    mutate(wgt = stat_weight/tot_wgt) %>%
    arrange(location,impact) %>%
    mutate(cumwgt = cumsum(wgt),
           weightType = !!weightType,
           summaryVar = !!summaryVar,
           geoType = !!geoType)
}

#' compute mean and percentiles of impacts
#'
#' Becasuse households are weighted, the function computes the impact at a given
#' quantile as the maximum impact occuring at or below that quantile
#' Given the coverage, this should produce a good estimate as long as the quantiles
#' are at least whole percentage points. Future modifications could interpolate
#' between max below and min above
#' @param ordered_impacts impacts ranked and given percentiles
#' @param pvals percentiles to extract
#' @return stats
#' @export
compute_stats <- function(ordered_impacts, pvals){

  pvals = tibble(
    ub = c(pvals,1.0001),
    percentile = str_c("p", as.character(round(ub*100)))
  )

  percentiles <- ordered_impacts %>%
    mutate(stat_name = as.character(cut(cumwgt,
                                        breaks = c(0, pvals$ub),
                                        labels = pvals$percentile,
                                        right = F))) %>%
    group_by_at(vars(location, stat_name, contains("type"), contains("product"), contains("_ID", ignore.case = F))) %>%
    summarize(stat = max(impact))

  stats <- ordered_impacts %>%
    group_by_at(vars(location, contains("type"), contains("product"), contains("_ID", ignore.case = F))) %>%
    summarize(mean = sum(impact*wgt)/sum(wgt), p0 = min(impact)) %>%
    pivot_longer(cols = mean:p0, names_to = "stat_name", values_to = "stat") %>%
    bind_rows(percentiles)
}


#' compute share above threshhold in each location for a given sample
#' @param sample_impacts  computed impacts on individual household for a sample
#' @param netGain_threshholds dollar value cutoffs for size of gain by household
#' @param shr_threshholds percentage of population cutoffs
#' @param share_type compute shares of HHshr = households (default),
#' of PPshr = population, or of VT shr = adults (potential voters)
#' @param geoType (default = rake_level) location to compute shares by
#' @return statistics by GEOID for all possible combinations of netGain and shr thresholds
#' @export
compute_shr_above <- function(sample_impacts,
                              netGain_threshholds,
                              shr_threshholds,
                              share_type = "HHshr",
                              geoType = "rakeLevel") {
  sample_impacts <- add_location(sample_impacts, geoType)

  switch(share_type,
         HHshr = {
           wgt_impacts <- sample_impacts %>% mutate(weight = final_weight)
         },
         PPshr = {
           wgt_impacts <- sample_impacts %>% mutate(weight = final_weight*people)
         },
         VTshr = {
           wgt_impacts <- sample_impacts %>% mutate(weight = final_weight*adults)
         },
         stop(str_c("share_type ", share_type, " not recognized"))
  )

  gain_stats <- wgt_impacts %>%
    group_by(location) %>%
    arrange(location, netGain) %>%
    mutate(cumwgt = cumsum(weight)/sum(weight),
           gain_cutoff = cut(netGain,
                             c(-Inf, netGain_threshholds$gain_cutoff),
                             labels = netGain_threshholds$gain_cutoff)) %>%
    filter(!is.na(gain_cutoff)) %>%
    group_by(location, gain_cutoff) %>%
    summarize(shr_at_below = max(cumwgt)) %>%
    mutate(shr_above = 1 - shr_at_below,
           share_type = !!share_type,
           geoType = !!geoType)

  check_share <- function(gain_stats, shr_cutoff){
    gain_stats %>%
      mutate(above_shr_thresh = shr_above>shr_cutoff,
             shr_cutoff = shr_cutoff)
  }

  gain_shr_stats <- map_dfr(shr_threshholds, ~ check_share(gain_stats, .x))
}

#' compute probability across samples that at least xx percent of households experience a gain of at least $xx
#' @param sample_shares shares achieving a target in individual samples
#' @return probabilities by GEOID, gain_threshhold, and share_threshhold
#' @export
summarize_shares <- function(sample_shares){
  summary_stats <- sample_shares %>%
    group_by_at(vars(location, contains("_type"), gain_cutoff, shr_cutoff)) %>%
    summarize(prob_satisfied = mean(above_shr_thresh))
}

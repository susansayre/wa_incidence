#' create a targets list for creating cex output data
#'
#' assumes existence of cex data in the prepared-data/cex folder
#' code is designed to extract information from a variety of files and construct
#' a dataset of annual expenditures for households observed for four consecutive
#' quarters
#'
#' CEX data is combined with energy price and degree day information at the state
#' level if state locations are available and at the national level otherwise
#' @param region optional parameter to restrict fitting sample to a particular region
#' @return list of targets
#' @export
cex_factory <- function(region = NULL){
  list(
    tar_target(cex_data_dir,
               "prepared-data/cex",
               format = "file"),

    tar_target(degree_days_dir,
               "data/degree_days",
               format = "file"),

    tar_target(state_price_match_file,
               "data/state_price_match.csv",
               format = "file"),

    tar_target(energy_price_file,
               "data/energy_prices.csv",
               format = "file"),

    tar_target(state_energy_prices,
               load_energy_prices(state_price_match_file, energy_price_file)),

    tar_target(degree_days,
               load_degree_days(degree_days_dir)),

    if (is.null(region)) {tar_target(cex_data,
               create_cex_prediction_data(cex_data_dir, degree_days, state_energy_prices))}
        else {
          tar_eval(tar_target(cex_data,
                         create_cex_prediction_data(cex_data_dir, degree_days, state_energy_prices) %>%
                         filter(region == region_id)),
                   values = list(region_id = region))},

    tar_target(cex_data_file,
               save_data(cex_data, here::here("output-data", "cex_data.rds")),
               format = "file")
  )
}

#' create a targets list for fitting functions to a list of cex data
#'
#'
#' @param method fitting method to use (e.g. dhxgboost)
#' @param var_set variables to use
#' @param products output values to fit functions for
#' @param standalone logical for whether to return a standalone list (T) or
#' one that needs to be coupled with a list created by cex_factory
#' @param reuse logical for whether to reuse existing fit files
#' @return list of targets
#' @export
fit_factory <- function(method, var_set, products, standalone = F, reuse = F){

  name_list_1 <- list(
    var_file = rlang::sym(str_c(var_set,"_file")),
    var_file_name = here::here("data", str_c("analysis_vars_", var_set, ".rds")),
    vars = rlang::sym(str_c(var_set,"_vars")),
    prediction_set = rlang::syms(str_c(var_set, "_prediction_set")),
    predictor_list = rlang::syms(str_c(var_set, method, "predictor_list", sep = "_")),
    fit_function = rlang::syms(str_c("fit_and_save.", method)),
    test_function = rlang::syms(str_c("test_fit.", method)),
    fit_dir = here::here("output-data", "fits", str_c(var_set, "_", method)),
    samples = rlang::syms(str_c(var_set, "_samples")),
    reuse = reuse,
    test_names = str_c(str_c("test", var_set, method, products, sep = "_"), collapse = ","))

  if (!file.exists(name_list_1$fit_dir)) {dir.create(name_list_1$fit_dir, showWarnings = F, recursive = T)}
  product_list <- create_replace_list(products, method, var_set)

  my_list <- list(

    tar_eval(tar_target(var_file,
                        var_file_name,
                        format = "file"),
             values = name_list_1[c("var_file", "var_file_name")]),

    tar_eval(tar_target(vars,
                        readRDS(var_file)),
             values = name_list_1[c("vars", "var_file")]),

    tar_eval(tar_target(prediction_set,
                        create_prediction_set(readRDS(cex_data_file),
                                              vars,
                                              remove_missing = T)),
             values = name_list_1[c("prediction_set", "vars")]),

    tar_eval(tar_target(predictor_list,
                        save_predictor_var_info(prediction_set,
                                                vars,
                                                fit_dir),
                        format = "file"),
             values = name_list_1[c("predictor_list", "prediction_set", "vars", "fit_dir")]),

    tar_eval(tar_target(samples,
                        split_samples(prediction_set, 0.8)),
             values = name_list_1[c("samples", "prediction_set")]))


  if (str_detect(method,"^mv")) {

    fit_name_list <- append(name_list_1,
                            list(
                              fit_files = rlang::syms(str_c("fit_files", var_set, method, sep = "_")),
                              fit_info = rlang::syms(str_c("fit_info", var_set, method, sep = "_")),
                              test = rlang::sym(str_c("test_", method)),
                              product_list = list(products)
                            ))

    fit_targets <- list(tar_eval(tar_target(fit_files,
                                            fit_function(samples,
                                                         vars,
                                                         product_list,
                                                         fit_dir,
                                                         reuse),
                                            format = "file"),
                                 values = fit_name_list),

                        tar_eval(tar_target(fit_info,
                                            fit_files,
                                            format = "file"),
                                 values = fit_name_list),

                        tar_eval(tar_target(test,
                                            test_function(samples,
                                                          product_list,
                                                          fit_files)),
                                 fit_name_list)
    )
  } else {

    mapped <- tar_map(product_list,
                      unlist = F,
                      names = all_of(c("var_set", "method", "product")),

                      tar_eval(tar_target(fit_files,
                                          fit_function(samples,
                                                       vars,
                                                       product,
                                                       fit_dir,
                                                       reuse),
                                          format = "file"),
                               values = list(fit_function = rlang::sym(str_c("fit_and_save.", method)),
                                             reuse = reuse)),

                      tar_eval(tar_target(test,
                                          test_function(samples,
                                                        product,
                                                        fit_files)),
                               values = list(test_function = rlang::sym(str_c("test_fit.", method)))))

    combined <- list(tar_eval(tar_combine(rmse,
                                     mapped[[2]],
                                     command = dplyr::bind_rows(!!!.x),
                                     use_names = F),
                         values = list(rmse = rlang::syms(str_c("rmse", var_set, method, sep = "_")))),
                     tar_eval(tar_combine(fit_info,
                                          mapped[[1]],
                                          command = list(!!!.x)),
                              values = list(fit_info = rlang::syms(str_c("fit_info",
                                                                         var_set,
                                                                         method,
                                                                         sep = "_")))
                              )
                     )

    fit_targets <- list(mapped, combined)

  }

  my_list <- list(my_list, fit_targets)

  if (standalone) {

    add_this <-  tar_target(cex_data_file,
                            here::here("output-data", "cex_data.rds"),
                            format = "file")

    my_list <- list(add_this, my_list)
  }

  return(my_list)
}

#' generate product maps
#' @param products character vector of outputs to predict
#' @param method shortcut description of method to use for fitting
#' @param var_set shortcut name of variable set to use as predictor variables
#' @return tibble of data to replace in map functions
#' @export
create_replace_list <- function(products, method, var_set){
  product_list <- tibble(product = products,
                         method = method,
                         var_set = var_set,
                         samples = rlang::syms(str_c(var_set, "_samples")),
                         vars = rlang::syms(str_c(var_set, "_vars")),
                         fit_dir = here::here("output-data", "fits", str_c(var_set, "_", method)))
}

#' rake to create synthetic data
#'
#'@param rake_vars_case code describing list of variables to rake on
#'@param state lower case two letter state abbreviation
#'@param year end year of acs data to use
#'@param raking_geography geographic level to rake at (tract, zcta and sldl are coded,
#'others would need to be added)
#'@return list of raking targets
#'@export
rake_factory <- function(rake_vars_case, state, year, raking_geography,
                         geocorr_yr = 2014,
                         reuse = NULL,
                         use_existing = F){
  
  if (!file.exists(here::here("output-data", "rake-sets"))) {
    dir.create(here::here("output-data", "rake-sets"),
               showWarnings = F, recursive = T)}
  
  sub_list <- list(rake_vars = rlang::syms(rake_vars_case),
                   rake_vars_file = rlang::syms(str_c("rake_vars_file_", rake_vars_case)),
                   rake_vars_filename = str_c("data/rake_vars_", rake_vars_case, ".rds"),
                   pums_hh = rlang::syms(str_c("pums_hh_", state, year)),
                   pums_pp = rlang::syms(str_c("pums_pp_", state, year)),
                   pums_hh_file = rlang::syms(str_c("pums_hh_file_", state, year)),
                   pums_pp_file = rlang::syms(str_c("pums_pp_file_", state, year)),
                   pums_hh_filename = here::here("prepared-data", "pums",
                                                 str_c("pums_hh",
                                                       "_", state, year, ".rds")),
                   pums_pp_filename = here::here("prepared-data", "pums",
                                                 str_c("pums_pp",
                                                       "_", state, year, ".rds")),
                   state = state,
                   year = year,
                   geocorr_yr = geocorr_yr,
                   raking_geography = raking_geography,
                   use_existing = use_existing,
                   filename = here::here("output-data",
                                         "rake-sets",
                                         str_c("synthetic_households_",
                                               state, year, "_",
                                               raking_geography, "_",
                                               rake_vars_case, ".rds")),
                   synth_hh = rlang::syms(str_c("synth_hh_",
                                                state, year, "_",
                                                raking_geography, "_",
                                                rake_vars_case))
  )
  
  state_year_list <- list(
    
    tar_eval(tar_target(pums_hh_file,
                        pums_hh_filename,
                        format = "file"),
             values = sub_list),
    
    tar_eval(tar_target(pums_pp_file,
                        pums_pp_filename,
                        format = "file"),
             values = sub_list),
    
    tar_eval(tar_target(pums_hh,
                        readRDS(pums_hh_file)),
             values = sub_list),
    
    tar_eval(tar_target(pums_pp,
                        readRDS(pums_pp_file)),
             values = sub_list)
    
  )
  
  my_list <- list(
    tar_eval(tar_target(rake_vars_file,
                        rake_vars_filename,
                        format = "file"),
             values = sub_list),
    tar_eval(tar_target(rake_vars,
                        readRDS(rake_vars_file)),
             values = sub_list),
    
    if ("state" %in% reuse & "year" %in% reuse) {} else {state_year_list},
    
    tar_eval(tar_target(synth_hh,
                        create_and_save_synthetic_households(state, acs_type = "acs5", year,
                                                             raking_geography, rake_vars,
                                                             pums_hh,
                                                             pums_pp,
                                                             filename,
                                                             integer_weights = T,
                                                             geocorr_yr = geocorr_yr,
                                                             use_existing = use_existing),
                        format = "file"),
             values = sub_list)
  )
  
  return(my_list)
}

#' create list of targets for simulation
#'
#'@param vars code for predictor var set to use
#'@param method fitting method
#'@param rake_vars_case code for raking
#'@param output_vars list of predictor variables
#'@param state lower case two letter state abbreviation
#'@param year end year of acs data to use
#'@param raking_geography geographic level to rake at (tract, zcta and sldl are coded,
#'others would need to be added)
#'@param tax_type description of type of tax to implement
#'three options have been coded: noE, netPrice, and wa
#'@param rebate_type type of rebate to issue
#'coded options include: pHHr, pPPr, wa732
#'@param tax_amt dollar amount of tax per metric ton co2e
#'@param electric_case (optional) tag of csv file electric_info_xxx.csv containing carbon
#'intensity of electricity by GEOID. Default is tract.
#'@param elasticity_case (optional) name of rds file containing a tibble with
#'elasticity info. Current implementation allows elasticity to vary by product and GEOID
#'@param agg_method (default = ai) aggregate by computing average incidence or synthetic incidence on average (siota) or incidence on acs average (aiota).
#'@return list of targets for simulation step
#'@param id_tags list of parameters that should be used to tag the target objects
#'@export
simulate_factory <- function(vars,
                             method,
                             rake_vars_case,
                             output_vars,
                             state,
                             year,
                             raking_geography,
                             tax_type,
                             rebate_type,
                             tax_amt,
                             electric_case = "tract",
                             elasticity_case = NULL,
                             agg_method = "ai",
                             id_tags = c("vars", "rake", "method", "elas", "tax")){
 # create sublist ----
 vars_id = if_else("vars" %in% id_tags, str_c("_", vars), "")
 method_id = if_else("method" %in% id_tags, str_c("_", method), "")
 rake_id = if_else("rake" %in% id_tags, str_c("_", rake_vars_case), "")
 elas_id = if_else("elas" %in% id_tags, str_c("_", elasticity_case), "")
 tax_id = if_else("tax" %in% id_tags, str_c("_", tax_type, tax_amt), "")
 elec_id = if_else("elec" %in% id_tags, str_c("_", electric_case), "")
 rebate_id = if_else("rebate" %in% id_tags, str_c("_", rebate_type), "")
 agg_id = if_else(agg_method == "ai", "", str_c("_", agg_method))

 sub_list <- list(
   # arguments of the factory function that need to be substituted in appropriate places
   state = state,
   year = year,
   raking_geography= raking_geography,
   tax_type = tax_type,
   rebate_type = rebate_type,
   tax_amt = tax_amt,
   vars = rlang::sym(str_c(vars,"_vars")),
   product_list = list(output_vars),
   fit_info = rlang::syms(str_c("fit_info", vars, method, sep = "_")),
   impact_function = rlang::syms(str_c("compute_sample_impacts.", rebate_type)),
   method = method,
   predict_function = rlang::syms(str_c("predict_expenditures.", method)),

   # target names and corresponding files that need to be substituted
   predictor_list = rlang::syms(str_c(vars, method, "predictor_list", sep = "_")),
   input_hh = rlang::syms(str_c("input_hh", rake_id)),
   synth_hh = rlang::syms(str_c("synth_hh_",
                                state, year, "_",
                                raking_geography, "_",
                                rake_vars_case)),
   synth_data = rlang::syms(str_c("synth_data", rake_id)),
   useful_data = rlang::syms(str_c("useful_data", vars_id, method_id, rake_id)),
   siota_data = rlang::syms(str_c("siota_data", vars_id, method_id, rake_id)),
   aiota_data= rlang::syms(str_c("aiota_data", vars_id, method_id, rake_id)),
   case_data = switch(agg_method,
                      ai = rlang::syms(str_c("useful_data", vars_id, method_id, rake_id)),
                      siota = rlang::syms(str_c("siota_data", vars_id, method_id, rake_id)),
                      aiota = rlang::syms(str_c("aiota_data", vars_id, method_id, rake_id))),
   consumption = rlang::syms(str_c("consumption", vars_id, method_id, rake_id, agg_id)),
   state_totals = rlang::syms(str_c(state, year, "_totals")),
   state_total_file = str_c(state,year,"_totals.csv"),
   scale_factors = rlang::syms(str_c("scale_factors", vars_id, method_id, rake_id)),
   scaled_consumption = rlang::syms(str_c("scaled_consumption", vars_id, method_id, rake_id, agg_id)),
   elasticity = rlang::sym(str_c("elasticity", elas_id)),
   elasticity_file = rlang::sym(str_c("elasticity_file", elas_id)),
   elasticity_file_name = str_c("data/elas_", elasticity_case, ".csv"),
   electric_info = rlang::sym(str_c("electric_info", elec_id)),
   electric_info_file = rlang::sym(str_c("electric_info_file", elec_id)),
   electric_info_filename = str_c("prepared-data/electric_info_", electric_case, ".csv"),
   tax_values = rlang::syms(str_c("tax_values", tax_id, elec_id)),
   tax_payments = rlang::syms(str_c("tax_payments", vars_id, method_id, rake_id, tax_id,
                                    elas_id, elec_id, agg_id)),
   impacts = rlang::syms(str_c("impacts", vars_id, method_id, rake_id, tax_id,
                               elas_id, elec_id, rebate_id, agg_id)),
   median_effect = rlang::syms(str_c("median_impact", vars_id, method_id, rake_id, tax_id,
                                     elas_id, elec_id, rebate_id, agg_id))
 )

# create target list ----
my_list <- list(
  tar_eval(tar_target(input_hh,
                      readRDS(synth_hh)),
           values = sub_list),

  tar_eval(tar_target(synth_data,
                      attach_locVars(input_hh,
                                     degree_days,
                                     state_energy_prices,
                                     toupper(state),
                                     year)),
           values = sub_list),

  switch(agg_method,

         ai = tar_eval(tar_target(useful_data,
                                  create_useful_data(synth_data,
                                                     vars,
                                                     predictor_list,
                                                     fit_dir)),
                       values = sub_list),

         siota = tar_eval(tar_target(siota_data,
                                     find_median_characteristics(useful_data,
                                                                 vars)),
                          values = sub_list),

         aiota = tar_eval(tar_target(aiota_data,
                                     find_median_acs_chars(siota_data,
                                                           year,
                                                           tidycensus::load_variables(year,
                                                                                      "acs5",
                                                                                      cache = T))),
                          values = sub_list),
         error(str_c("agg_method ", agg_method, " unknown"))),

  tar_eval(tar_target(consumption,
                      predict_function(case_data,
                                       product_list,
                                       fit_info)),
           values = sub_list),

  if (agg_method == "ai") {list(tar_eval(tar_target(state_totals,
                                                    file.path(here::here("data"),
                                                              state_total_file),
                                                    format = "file"),

                                         values = sub_list),

                                tar_eval(tar_target(scale_factors,
                                                    compute_scale_factors(consumption,
                                                                          state_totals)),
                                         values = sub_list))} else {},

  tar_eval(tar_target(scaled_consumption,
                      scale_consumption(consumption,
                                        scale_factors)),

           values = sub_list),

  if (is.null(elasticity_case)) {
    tar_target(elasticity,
               NULL)
  } else {
    list(tar_eval(tar_target(elasticity_file,
                             elasticity_file_name,
                             format = "file"),

                  values = sub_list),
         tar_eval(tar_target(elasticity,
                             read_csv(elasticity_file)),
                  values = sub_list))
  },

  tar_eval(tar_target(electric_info_file,
                      electric_info_filename,
                      format = "file"),
           values = sub_list),

  tar_eval(tar_target(electric_info,
                      read_csv(electric_info_file)),
           values = sub_list),

  tar_eval(tar_target(tax_values,
                      load_tax_values(tax_amt,
                                      state_energy_prices,
                                      state,
                                      year,
                                      tax_type,
                                      electric_info)),
           values = sub_list),

    tar_eval(tar_target(tax_payments,
                        compute_tax_payment(scaled_consumption,
                                            tax_values,
                                            elasticity)),
             values = sub_list),

    tar_eval(tar_target(impacts,
                        impact_function(tax_payments, case_data)),
             values = sub_list),

  if (agg_method == "ai"){
    list(tar_eval(tar_target(median_effect,
                             impacts %>% 
                               group_by(GEOID) %>% 
                               uncount(final_weight) %>% 
                               summarize(median_impact = median(netGain))),
                  values = sub_list))
    })
}

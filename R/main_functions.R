#' create full set of prediction data from cex data
#' @param cex_data_dir data containing already downloaded and prepped cex data; use verify_cex_dir() to download if it doesn't exist and prepare_input_data.R to prepare the input data if starting from scratch
#' @param degree_day_dir directory containing the degree days data
#' @param state_energy_prices dataframe containing state energy prices
#' @return data frame containing households level data for both output and predictor data
#' @export
create_cex_prediction_data <- function(cex_data_dir, degree_days, state_energy_prices){

  energy_categories <- load_energy_categories()
  mtbi_data <- readRDS(file.path(cex_data_dir, "combined_mtbi.rds"))
  fmli_data <- readRDS(file.path(cex_data_dir, "combined_fmli.rds"))
  memi_data <- readRDS(file.path(cex_data_dir, "combined_memi.rds"))
  opb_data <- readRDS(file.path(cex_data_dir, "combined_opb.rds"))
  opi_data <- readRDS(file.path(cex_data_dir, "combined_opi.rds"))
  rent_data <- readRDS(file.path(cex_data_dir, "combined_rnt.rds"))

  sample_newids <- find_sample_newids(mtbi_data)
  sample_cu_ids <- find_sample_cu_ids(sample_newids)
  energy_by_month <- compute_sample_energy_expense(energy_categories, mtbi_data, sample_cu_ids) #energy totaled by household
  cleaned_fmli <- clean_fmli(sample_cu_ids = sample_cu_ids, sample_newids = sample_newids, fmli_input = fmli_data) #pull all observations for a household
  cleaned_mtbi <- clean_mtbi(sample_cu_ids,mtbi_data) #expenses totaled by household
  cleaned_opb <- clean_opb(sample_newids,opb_data) #aquire year last interviews
  cleaned_opi <- clean_opi(sample_cu_ids,opi_data) #average values across interviews
  cleaned_rent <- clean_rent(sample_cu_ids,rent_data) #average values across interviews
  cex_data <- combine_cex_data(sample_cu_ids,
                               cleaned_fmli,
                               cleaned_mtbi,
                               cleaned_opb,
                               cleaned_opi,
                               cleaned_rent,
                               memi_data)

  cex_hh_data <- clean_hh_data(cex_data)

  heating_fuels <- infer_heating_fuel(energy_by_month)

  prediction_data <- combine_prediction_datasets(cex_hh_data,
                                                 energy_by_month,
                                                 degree_days,
                                                 state_energy_prices,
                                                 heating_fuels)
}

#' create synthetic households
#' @param state two letter lower case abbreviation for state whose pums data
#' will be used for analysis
#' @param acs_type abbreviation for acs data to use, must be a valid option
#' for the tidycensus package (default = acs5)
#' @param end_year end year for acs data
#' @param raking_geography abbreviation for destination raking geography,
#' appropriately named geocorr files must be downloaded and the abbreviation
#' and its match to the tidycensus geography options must be coded into the
#' list within process_acs_args step below
#' @param rake_var_list list of stems for the raking variables to use. Every
#' element of the list must be defined as a switch option in process_pums AND
#' get_rake_var_vars. Variables necessary to compute its value in the household
#' pums data must also be added to the hh_raking_data or pp_raking_data calls in
#' this function
#' @param pums_db name of sqlite database containing pums data
#' @param filename name of file to save resulting data in
#' @param integer_Weights logical determining whether to return integer weights (default = T)
#' @return filename containing the synthetic household data
#' @export
create_and_save_synthetic_households <- function(state, acs_type = "acs5", end_year,
                                        raking_geography, rake_var_list,
                                        pums_hh_data,
                                        pums_pp_data,
                                        filename,
                                        integer_weights = T,
                                        geocorr_yr = 2014,
                                        use_existing = F){
  
  if (!use_existing) {
  geo_long <- switch(raking_geography,
                     sldl = "state legislative district (lower chamber)",
                     zcta = "zcta",
                     tract = "tract",
                     stop(str_c("raking_geography", raking_geography, " not recognized"))
  )

  raking_geocorr <- switch(raking_geography,
                    sldl = ifelse(geocorr_yr > 2014, "sldl16", "sldl14"),
                    zcta = "zcta5",
                    tract = "tract",
                    stop(str_c("raking_geogrpahy", raking_geogrpahy, " not recognized"))
  )

  loc_alloc_file <- file.path("input-data/pums",
                              str_c("geocorr", geocorr_yr, "_",
                                    state, "_", raking_geography,
                                    "_to_cbsa.csv"))

  cbsa_id_file <- file.path("input-data/pums/id_psu.csv")

  target_to_puma_file <- file.path("input-data/pums",
                                   str_c("geocorr", geocorr_yr, "_",
                                         state, "_", raking_geography,
                                         "_to_puma.csv"))

  puma_to_target_file <- file.path("input-data/pums",
                                   str_c("geocorr", geocorr_yr, "_",
                                         state, "_puma_to_", raking_geography,
                                         ".csv"))

  loc_type_alloc <- compute_loc_type(state, year, acs_type, raking_geocorr, loc_alloc_file, cbsa_id_file)
  target_to_puma <- load_target_to_puma_wgt(state, raking_geocorr, target_to_puma_file)
  puma_to_target <- load_puma_to_target_wgt(state, raking_geocorr, puma_to_target_file)

  load("input-data/census_regions.rdata")
  prediction_pums <- merge_pums_prediction_data(pums_hh_data, pums_pp_data, census_regions, state)


  acs_vars <- tidycensus::load_variables(end_year, acs_type, cache = T)

  rvars <- map(rake_var_list, get_rake_var_vars, acs_vars = acs_vars)

  # Extract data on raking variable values for households in the pums data
  raking_data <- merge_raking_data(pums_hh_data, pums_pp_data)
  rpums <- map2(rake_var_list, rvars, process_pums, hh_data = raking_data)
  raking_individuals <- reduce(rpums, left_join) %>% filter(!NP == "00")

  # Extract data on raking_geo level totals
  # Note: the process_acs function drops census tracts with fewer than 30 households
  racs <- map2(rake_var_list, rvars, process_acs,
                geography = geo_long,
                state = toupper(state),
                end_year = end_year,
                survey_type = acs_type)

  raking_constraints <- reduce(racs,inner_join) %>% inner_join(puma_to_target)

  #perform raking
  var_list <- str_c(rake_var_list, "_cat")

  puma_list <- unique(raking_constraints$PUMA)

  acs_puma_cat_count <- raking_constraints %>%
    group_by(PUMA) %>%
    select(-to_target_alloc, -GEOID) %>%
    summarize(across(everything(), sum)) %>%
    pivot_longer(-PUMA, names_to = "category")

  pums_puma_cat_count <- raking_individuals %>%
    pivot_longer(cols = contains("_cat"), names_to = "rake_var_cat", values_to = "category") %>%
    group_by(PUMA, category) %>%
    summarize(pums_hh = n())

  compare_counts <- acs_puma_cat_count %>%
    full_join(pums_puma_cat_count)

  if (any(filter(compare_counts, is.na(pums_hh)))) {
    print("some categories may be missing PUMS households in a particular PUMA")
    filter(compare_counts, is.na(pums_hh))
    browser()
  }

  hh_weights <- rake_pumas(raking_individuals, raking_constraints, puma_list, var_list, integer_weights = F)

  puma_adj_weights <- adjust_output_weights(hh_weights, target_to_puma)
  loc_type_weight <- add_cat(puma_adj_weights,loc_type_alloc)
  household_data <- left_join(loc_type_weight,prediction_pums) %>%
    group_by(GEOID, SERIALNO) %>%
    mutate(hhkey = str_c(SERIALNO, ".", row_number())) %>%
    select(-SERIALNO)

  if (integer_weights) {
    # integerize weights using TRS method from
    # https://spatial-microsim-book.robinlovelace.net/smsimr.html#sintegerisation
    weight_data <- household_data %>%
      group_by(GEOID) %>%
      filter(!is.na(hhkey)) %>% #drops 3 GEOIDs with no households
      mutate(wgt_int = floor(adj_hh_weight),
             resid = adj_hh_weight - wgt_int,
             def = sum(resid))

    data_list <- split(weight_data, weight_data$GEOID)

    integerize_geoid <- function(geoid_data){

      topup <- sample(length(geoid_data$adj_hh_weight),
                      size = sum(geoid_data$resid),
                      prob = geoid_data$resid)

      geoid_data$wgt_int[topup] <- geoid_data$wgt_int[topup] + 1

      output_data <- geoid_data %>%
        mutate(final_weight = wgt_int) %>%
        select(-wgt_int, -resid, -def) %>%
        filter(final_weight > 0)
    }


    final_data <- map_dfr(data_list, integerize_geoid)
  } else {
      final_data <- household_data
    }

  saveRDS(final_data, file = filename)
}
  return(filename)
}

#' create training and testing data
#' @param input_data dataset to use for prediction, must contain all the variables
#' listed as O or P in analysis_vars
#' @param analysis_vars dataframe containing three columns: var_name (name of variable),
#' type (N = numeric, O = ordered factor, F = factor), cat (O = output, P = predictor,
#' S = used in simulation but not fitting, and X = unused)
#' @param training_share (default = 0.8) fraction of data to use for training
#' @param remove_missing (default = F) remove observtions with missing data
#' @param remove_outliers optional percentage of outliers to remove, can be either a single two-element numeric vector that is applied to all dependent variables or a named list of two-element vectors that are independently applied to each dependent variable. In all cases, the first element gives the percentage to eliminate at the lower end and the second element is the percentage to eliminate at the upper end
#' @return named list of two elements (train and test data)
#' @export
create_training_data <- function(prediction_data, analysis_vars, training_share = 0.8,
                                 remove_missing = F,
                                 remove_outliers = c()){
input_data <- prune_data(prediction_data, analysis_vars, remove_missing, remove_outliers)
input_samples <- split_samples(prediction_set, training_share = .8)

  }

#' save data
#' @param data_to_save object containing a file we want to save
#' @param path_to_file location to save the file in
#' @return path_to_file
#' @export
save_data <- function(data_to_save, path_to_file){
  saveRDS(data_to_save, file = path_to_file)
  return(path_to_file)
}

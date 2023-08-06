## Functions used in analyzing output ----

#' return percentiles as a tibble
#' @param x vector of values to calculate quantiles
#' @param q (default c(0.25, 0.5, 0.75) quantile points
#' @return tibble with the quantile values in the variable x and the prob in q
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble(x = quantile(x, q), q = q)
}

#' map_median
#' 
#' mpas tract level medians
#' 
#' @param case_name id tag used by targets to label the output we're plotting
#' @param case_id id tag used to designate output
#' @param dir directory to store figure file in
#' @param boundaries shapefile with boundaries
map_median <- function(case_name, case_id, dir, boundaries){
  median_impact <- tar_read_raw(str_c("median_impact_", case_name)) %>%
    select(GEOID, median_impact)
  
  map_data <- tracts %>%
    left_join(median_impact)
  
  median_map <- tm_shape(map_data) + 
    tm_fill(col = "median_impact",
            palette = "BrBG",
            colorNA = "white",
            midpoint = 0,
            title = "Median Impact ($)",
            legend.hist = T) +
    tm_shape(pumas) + tm_borders() +
    tm_layout(legend.outside = T,
              frame = F)
  
  tmap_save(median_map, 
            file.path(dir, str_c("median_impact_map_", case_id, ".png")),
            width = 6,
            height = 3.5)
  
  return(median_map)}

map_median_bw <- function(case_name, case_id, dir, boundaries){
  median_impact <- tar_read_raw(str_c("median_impact_", case_name)) %>%
    select(GEOID, median_impact)
  
  map_data <- tracts %>%
    left_join(median_impact)
  
  median_map_bw <- tm_shape(map_data) + 
    tm_fill(col = "median_impact",
            palette = "Greys",
            colorNA = "white",
            midpoint = 0,
            title = "Median Impact ($)",
            legend.hist = T) +
    tm_shape(pumas) + tm_borders() +
    tm_layout(legend.outside = T,
              frame = F)
  
  tmap_save(median_map_bw, 
            file.path(dir, str_c("median_impact_map_", case_id, "_bw.png")),
            width = 6,
            height = 3.5)
  
  return(median_map_bw)}

#' get density info
get_density_info <- function(state, year, geography){
  density_info <- tidycensus::get_acs(geography = geography,
                                   variable = "B01001_001",
                                   year = year,
                                   state = toupper(state),
                                   survey = "acs5",
                                   geometry = T,
                                   keep_geo_vars = T,
                                   cache_table = T) %>%
  filter(estimate != 0) %>%
  mutate(sqmi = ALAND*0.00000038610215854245,
         ppsqmi = estimate/sqmi,
         log_density = log(ppsqmi),
         density_quantile = ntile(ppsqmi, 5)) 
}

compute_poverty_level_groups <- function(hh_impacts, pl_breaks){
  
  pl_breaks <- hh_impacts %>% 
    mutate(pl_ratio = tot_income/poverty_level,
           pl_ratio_group = cut(pl_ratio, pl_breaks, ordered_result = T))
}

extract_case_results <- function(result_tag){
  
  results <- tar_read_raw(str_c("impacts_baseline_dhxgboost_alpha", result_tag)) %>% 
    select(hhid, GEOID, tot_income, tax_payment, sales_tax_savings,
           wf_tax_exemption, rebate, 
           one_of(c("scaled_sales_tax", "rebate_shares")), 
           poverty_level, netGain, pctGain, final_weight)
}

compare_dist_pl <- function(full_data, cases, fig_dir){
  
  full_data %>%
    mutate(pl_label = str_c("Ratio of income to poverty level in range ", pl_ratio_group)) %>% 
    filter(case_number %in% cases) %>% 
    ggplot(aes(x = netGain, color = case_name, linetype = case_name)) +
    geom_density(position = "identity",
                 key_glyph = draw_key_path) +
    theme_bw() +
    ylab("Kernel Density") +
    xlab("Net gain (loss) from policy ($/hh/year)") +
    geom_vline(xintercept = 0) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") +
    labs(color = "Policy", linetype = "Policy") +
    facet_wrap("pl_label", ncol = 1)
  
  ggsave(here::here(fig_dir, str_c("compare_dist_pl_",
                                   str_c(cases, collapse = "_"), 
                                   ".png")), 
         height = 7, width = 6)
}

compare_dist_pl_bw <- function(full_data, cases, fig_dir){
  
  full_data %>%
    mutate(pl_label = str_c("Ratio of income to poverty level in range ", pl_ratio_group)) %>% 
    filter(case_number %in% cases) %>% 
    ggplot(aes(x = netGain, linetype = case_name)) +
    geom_density(position = "identity",
                 key_glyph = draw_key_path) +
    theme_bw() +
    ylab("Kernel Density") +
    xlab("Net gain (loss) from policy ($/hh/year)") +
    geom_vline(xintercept = 0) +
#    scale_color_brewer(palette = "Greys") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") +
    labs(color = "Policy", linetype = "Policy") +
    facet_wrap("pl_label", ncol = 1)
  
  ggsave(here::here(fig_dir, str_c("compare_dist_pl_",
                                   str_c(cases, collapse = "_"), 
                                   "_bw.png")), 
         height = 7, width = 6)
}

compare_dist_density <- function(full_data, cases, fig_dir){
  
  full_data %>%
    filter(case_number %in% cases) %>% 
    ggplot(aes(x = netGain, color = case_name, linetype = case_name)) +
    geom_density(position = "identity",
                 key_glyph = draw_key_path) +
    theme_bw() +
    ylab("Kernel Density") +
    xlab("Net gain (loss) from policy ($/hh/year)") +
    geom_vline(xintercept = 0) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") +
    labs(color = "Policy", linetype = "Policy") +
    facet_wrap("density_group", ncol = 1)
  
  ggsave(here::here(fig_dir, str_c("compare_dist_density_",
                                   str_c(cases, collapse = "_"), 
                                   ".png")), 
         height = 7, width = 6)
}
## compare median characteristics
compare_median_chars <- function(case_id){
  
  median_chars_synth <- tar_read_raw(str_c("siota_data_", case_id))
  
  median_chars_acs <- tar_read_raw(str_c("aiota_data_", case_id))
  
  compare_median_characteristics <- median_chars_synth %>%
    mutate(case = "synth") %>%
    bind_rows(mutate(median_chars_acs, case = "acs"))
  
}

## summarize discrete comparisons
summarize_discrete_var_comparisons <- function(compare_chars){
  
  discrete_vars <- c("age_cat_hhldr", "educ_cat_hhldr", "heating_fuel", "people", "adults",
                     "children", "num_vehicles", "num_in_labor_force", "num_rooms", "year_built")
  
  compare_discrete <- compare_chars %>%
    mutate(across(all_of(discrete_vars), as.character)) %>%
    select(GEOID, case, all_of(discrete_vars)) %>%
    pivot_longer(cols = all_of(discrete_vars)) %>%
    pivot_wider(names_from = "case") %>%
    group_by(name, synth, acs) %>%
    summarize(n = n()) %>% 
    group_by(name) %>%
    mutate(share = n/sum(n))
  
  discrete_summaries <- compare_discrete %>%
    filter(acs == synth) %>%
    group_by(name) %>%
    summarize(share_matched = sum(share))
  
}

#summarize continuous vars
summarize_continuous_var_comparisons <- function(compare_chars){
  continuous_vars <- c("white_hhldr", "num_unemployed", "food_stamp", "tot_income",
                     "sf_detached", "homeowner", "spouse_present")

  compare_continuous <- compare_chars %>%
    select(GEOID, case, all_of(continuous_vars)) %>%
    pivot_longer(cols = all_of(continuous_vars)) %>%
    pivot_wider(names_from = "case") %>%
    group_by(name) %>%
    mutate(delta = synth - acs,
           acs_sd = sd(acs),
           acs_iqr = IQR(acs),
           scaled_diff_sd = delta/acs_sd,
           scaled_diff_iqr = delta/acs_iqr)
}
  
## sample verification functions
construct_replace1_comparisons <- function(vars_id, fit_method, rake_id){
  
  #load data
  median_chars_synth <- tar_read_raw(str_c("siota_data_", 
                                           vars_id, "_", 
                                           fit_method, "_", rake_id))
  
  median_chars_acs <- tar_read_raw(str_c("aiota_data_", 
                                         vars_id, "_", 
                                         fit_method, "_", rake_id))
  
  tax_values <- tar_read(tax_values)
  
  ima_data <- tar_read_raw(str_c("impacts", vars_id, fit_method, rake_id, "aiota", sep = "_")) %>%
    select(GEOID, ima = netGain)
  
  #exclude variable that are constant
  varnames <- setdiff(colnames(median_chars_synth),
                      c("region","loc_type", "hdd", "cdd", "electricity_price",
                        "gasoline_price", "natural_gas_price", "heating_oil_price",
                        "propane_price", "GEOID", "final_weight", "hhkey"))
  
  products <- tar_read_raw(str_c("rmse", vars_id, fit_method, sep = "_"))  %>% 
    select(product) %>% 
    as.list() %>% 
    unlist()
  
  scale_factors <- tar_read_raw(str_c("scale_factors", vars_id, fit_method, rake_id, sep = "_"))
  
  compute_new_impacts <- function(varname) {
    this_synth_char <- median_chars_synth %>%
      select(GEOID, all_of(varname))
    
    modified_chars <- median_chars_acs %>%
      select(-all_of(varname)) %>%
      left_join(this_synth_char)
    
    fit_info <- tar_read_raw(str_c("fit_info", vars_id, fit_method, sep = "_"))
    
    predict_expenditures.dhxgboost(modified_chars,
                                   products,
                                   fit_info) %>%
      scale_consumption(scale_factors) %>%
      select(product, consumption, hhkey, GEOID, final_weight) %>%
      compute_tax_payment(tax_values) %>%
      compute_sample_impacts.wa732(modified_chars) %>%
      select(GEOID, new_impact = netGain) %>%
      mutate(swap_var = varname)
  }
  
  modified_results <- lapply(varnames, compute_new_impacts) %>%
    bind_rows() %>%
    left_join(ima_data) %>%
    mutate(delta = ima - new_impact) %>%
    filter(!is.na(delta))
}

plot_ai_vs_iota <- function(case_name, case_id){
  median_impact <- tar_read_raw(str_c("median_impact_", case_name)) %>%
    select(GEOID, median_impact)
  
  ims_data <- tar_read_raw(str_c("impacts_", case_name, "_siota")) %>%
    select(GEOID, final_weight, eitc, sales_tax_savings, tax_payment, ims = netGain) %>%
    filter(!is.na(ims))
  
  comparison_data <-  median_impact %>%
    left_join(select(ims_data, GEOID, ims)) %>%
    drop_na()
  
   ggplot(comparison_data, aes(y = median_impact, x = ims)) +
    geom_point(alpha = .1) +
    geom_abline(intercept = 0, slope = 1) +
    ylab("Synthetic: median household impact") +
    xlab("Synthetic: impact on household with median characterisitcs") +
    theme_bw() +
    coord_fixed(xlim = c(-250, 500),
                ylim = c(-250, 500))
  
  ggsave(file.path(fig_dir, str_c("mi_to_ims_", case_id, ".png")),
         width = 5)
}
## custom functions used in the _targets pipeline that are specific to Washington analysis ----
#' compute sample impacts for a rural rebate policy
#'
#' policy matches 732 in total payments using Bauman and Bare formula but allocates 20% of the total rebate amount to payments to households located in areas with. This extra amount is refunded on a 1 share per adult and 0.5 share per dependent basis.
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics needed for computing rebates
#' @param pop_density dataset containing population density of census tracts
#' @return household level impacts
#' @export
compute_sample_impacts.wa732rr <- function(sample_tax_payment, household_data, pop_density){
  
  wa732_rebates <- compute_sample_impacts.wa732(sample_tax_payment, household_data) 
  
  rr_share <- .1818
  household_rebates <- pop_density %>%
    sf::st_drop_geometry() %>% 
    select(GEOID, ppsqmi) %>% 
    right_join(wa732_rebates) %>%
    mutate(eligible_hh = ifelse(ppsqmi <= 500, 1, 0),
           eligible_adults = adults*eligible_hh,
           eligible_children = children*eligible_hh)
  
  state_total <- household_rebates %>%
    ungroup() %>%
    summarize(wf_tax_exemption = sum(wf_tax_exemption*final_weight),
              sales_tax = sum(sales_tax_savings*final_weight),
              eligible_adults = sum(eligible_adults*final_weight),
              eligible_children = sum(eligible_children*final_weight)) %>%
    mutate(rr_fund = rr_share*(wf_tax_exemption + sales_tax),
           sales_tax_share = (1-rr_share)*(sales_tax + wf_tax_exemption)/sales_tax,
           rr_share = rr_fund/(eligible_adults + .5*eligible_children))
  
  rr_rebates <- household_rebates %>%
    mutate(scaled_sales_tax = state_total$sales_tax_share*sales_tax_savings,
           rebate_shares = (eligible_adults + .5*eligible_children)*state_total$rr_share,
           netGain = scaled_sales_tax + rebate_shares - tax_payment,
           pctGain = netGain/income_denom)
}

#' compute sample impacts for a low-income rebate policy
#'
#' policy matches 732 in total payments using Bauman and Bare formula but allocates 20% of the total rebate amount to payments to household's whose income is below 1.5*poverty level. This extra amount is refunded on a 1 share per adult and 0.5 share per dependent basis.
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @return household level impacts
#' @export
compute_sample_impacts.wa732lir <- function(sample_tax_payment, household_data){
  
  household_rebates <- compute_sample_impacts.wa732(sample_tax_payment, household_data) %>%
    mutate(eligible_hh = ifelse(tot_income < 1.5*poverty_level, 1, 0),
           eligible_adults = adults*eligible_hh,
           eligible_children = children*eligible_hh)
  
  lir_share = .1818
  state_total <- household_rebates %>%
    ungroup() %>%
    summarize(wf_tax_exemption = sum(wf_tax_exemption*final_weight),
              sales_tax = sum(sales_tax_savings*final_weight),
              eligible_adults = sum(eligible_adults*final_weight),
              eligible_children = sum(eligible_children*final_weight)) %>%
    mutate(lir_fund = lir_share*(wf_tax_exemption + sales_tax),
           sales_tax_share = (1-lir_share)*(sales_tax + wf_tax_exemption)/sales_tax,
           lir_share = lir_fund/(eligible_adults + .5*eligible_children))
  
  lir_rebates <- household_rebates %>%
    mutate(scaled_sales_tax = state_total$sales_tax_share*sales_tax_savings,
           rebate_shares = (eligible_adults + .5*eligible_children)*state_total$lir_share,
           netGain = scaled_sales_tax + rebate_shares - tax_payment,
           pctGain = netGain/income_denom)
}

#' compute sample impacts for wa732 using Bauman and Bare formula
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @return household level impacts
#' @export
compute_sample_impacts.wa732 <- function(sample_tax_payment, household_data){
  
  household_stats <- synchronize_key_wgt(sample_tax_payment, household_data) %>%
    select(hhid, GEOID, final_weight, tax_payment,
           people, adults, tot_income, children, spouse_present)
  
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
           sales_tax_savings = replace_na(sales_tax_savings,0),
           rebate = sales_tax_savings + wf_tax_exemption,
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

#' compute sample impacts for an only sales tax policy that matches 732 using Bauman and Bare formula
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @return household level impacts
#' @export
compute_sample_impacts.wa732sto <- function(sample_tax_payment, household_data){
  
  household_rebates <- compute_sample_impacts.wa732(sample_tax_payment, household_data)
  
  state_total <- household_rebates %>%
    ungroup() %>%
    summarize(wf_tax_exemption = sum(wf_tax_exemption*final_weight),
              sales_tax = sum(sales_tax_savings*final_weight)) %>%
    mutate(sales_tax_scale = wf_tax_exemption/sales_tax)
  
  sales_tax_only_rebates <- household_rebates %>%
    mutate(netGain = (1 + state_total$sales_tax_scale)*sales_tax_savings - tax_payment,
           pctGain = netGain/income_denom)
  
}

#' compute sample impacts for an only sales tax policy that matches 732 using Bauman and Bare formula
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @return household level impacts
#' @export
compute_sample_impacts.wa732pPPr <- function(sample_tax_payment, household_data){
  
  household_rebates <- compute_sample_impacts.wa732(sample_tax_payment, household_data)
  
  state_total <- household_rebates %>%
    ungroup() %>%
    summarize(wf_tax_exemption = sum(wf_tax_exemption*final_weight),
              sales_tax = sum(sales_tax_savings*final_weight),
              shares = sum((adults + 0.5*children)*final_weight),
              weight = sum(final_weight)) %>%
    mutate(rebate = (wf_tax_exemption + sales_tax)/shares)
  
  pPPr_rebates <- household_rebates %>%
    mutate(netGain = state_total$rebate*(adults + 0.5*children) - tax_payment,
           pctGain = netGain/income_denom)
  
}

#' compute sample impacts for an only sales tax policy that matches 732 using Bauman and Bare formula
#' @param sample_tax_payments dataset of sample tax payments
#' @param household_data dataset to get additional household characteristics need for computing rebates
#' @return household level impacts
#' @export
compute_sample_impacts.no_rebate <- function(sample_tax_payment, household_data){
  
  household_stats <- synchronize_key_wgt(sample_tax_payment, household_data) %>%
    select(hhid, GEOID, final_weight, tax_payment,
           people, adults, tot_income) %>% 
    mutate(netGain = -tax_payment)
  
}

#' test a fit object -- replace default carbonsms function with more detail
#' @param data testing data set
#' @param output_var variable to test
#' @param fit_files directory containing fit models
#' @return tibble row with product name and rmse
#' @export
test_fit.dhxgboost <- function(new_data, output_var, fit_files){
  
  test_data <- tibble(actual = new_data$test[[output_var]],
                      predicted = predict_product.dhxgboost(new_data$test,
                                                            output_var,
                                                            fit_files)) %>%
    mutate(tp = ifelse(actual > 0 & predicted > 0, 1, 0),
           fn = ifelse(actual > 0 & predicted == 0, 1, 0),
           fp = ifelse(actual == 0 & predicted > 0, 1, 0),
           tn = ifelse(actual == 0 & predicted == 0, 1, 0),
           tp_actual = ifelse(tp == 1, actual, NA),
           errorlog = ifelse(tp == 1,
                                log(actual) - log(predicted),
                                NA),
           log_actual = ifelse(tp == 1,
                           log(actual),
                           NA),
           dev_log = log_actual - mean(log_actual, na.rm = T),
           tsa = abs(actual - mean(actual)),
           tss = (actual - mean(actual))^2,
           mad_log = abs(dev_log),
           mae_log = abs(log_actual - log(predicted)),
           mad_tp = abs(tp_actual - mean(tp_actual, na.rm = T)),
           mae_tp = abs(tp_actual - predicted),
           mse_tp = (tp_actual - predicted)^2,
           error = actual - predicted,
           ess = error^2,
           esa = abs(error),
           mse_log = errorlog^2,
           mae_log = abs(errorlog)) %>%
    summarize(across(one_of("tp", "tn", "fp", "fn", "esa", "ess", "tss", "tsa"), sum),
              across(one_of("mse_log", "mae_log", "mad_log", "mae_log",
                            "mad_tp", "mae_tp", "mse_tp"),
                     ~ mean(.x, na.rm = T)),
              sd = sd(actual),
              sd_log = sd(log_actual, na.rm = T),
              sd_tp = sd(tp_actual, na.rm = T),
              count = n()) %>%
    mutate(rmse = sqrt(ess/count),
           rmse_log = sqrt(mse_log),
           rmse_tp = sqrt(mse_tp),
           precision = tp/(tp + fp),
           recall = tp/(tp + fn),
           f1score = 2*tp/(2*tp + fp + fn),
           specificity = tn/(tn + fp),
           miss_rate = (fp + fn)/(tp + tn + fp + fn),
           accuracy = 1 - miss_rate,
           product = output_var,
           mad = tsa/count,
           mae = esa/count,
           rsq = 1 - ess/tss,
           rad = 1 - esa/tsa) %>% 
    select(product, sd, rmse, mad, mae, 
           precision, recall, f1score, accuracy,
           sd_tp, rmse_tp, mad_tp, mae_tp, 
           sd_log, rmse_log, mad_log, mae_log, rsq, rad)
}

create_fit_table <- function(base_rmse, filename){

base_rmse %>%
  select(Product = product,
         `Test sample sd ($/yr)` = sd,
         `RMS prediction error ($/yr)` = rmse,
         `R2 within test sample` = rsq) %>% 
  kableExtra::kable(format = "latex",
                    booktabs = T,
                    digits = 2) %>% 
  kableExtra::save_kable(filename)

return(filename)}

## utility to facilitate working with modelsummary tables
write_no_table_envir <- function(x, file){
strings <- read_lines(x)
begin_table_row <- which(str_detect(strings, "begin\\{table\\}"))
centering_row <- which(str_detect(strings, "centering"))
end_table_row <- which(str_detect(strings, "end\\{table\\}"))
last_midrule <- tail(which(str_detect(strings, "midrule")), n = 1)

main_table <- strings[setdiff(seq(1:length(strings)),
                              c(begin_table_row, 
                                centering_row,
                                last_midrule,
                                end_table_row))]

num_obs <- which(str_detect(main_table, "Num.Obs."))
my_table <- c(main_table[1:(num_obs-1)], 
              "\\midrule", 
              main_table[num_obs:length(main_table)])
write_lines(my_table, file)
}

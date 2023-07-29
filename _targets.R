library(targets)
library(tarchetypes)
options(tidyverse.quiet = T)
source("R/load_functions_packages.R")

tar_option_set(packages = c("tidyverse"),
               workspace_on_error = T)


dir.create(here::here("output-data"), showWarnings = F)
dir.create(here::here("tables"), showWarnings = F)
dir.create(here::here("figures"), showWarnings = F)

my_list <- list(
  cex_factory(),
  
  fit_factory("dhxgboost", "baseline", 
              c("electricity", "gasoline", "naturalgas", "fueloil", "gasbottledtank",
                "totexp"), F, reuse = T),
  
  tar_target(fit_comparison_table,
             create_fit_table(rmse_baseline_dhxgboost, 
                              here::here("tables/spending-fcn-fit.tex")),
             format = "file"),
 
  rake_factory("alpha", "wa", 2016, "tract", use_existing = T),
  
  # compute base case with elasticity = 0 ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   id_tags = c("vars", "rake", "method")),

  # compute base case with ctam elasticity ----
                     
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank", 
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   elasticity_case = "ctam",
                   id_tags = c("vars", "rake", "method", "elas")),


  # synthetic iota baseline
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   agg_method = "siota",
                   id_tags = c("vars", "rake", "method")),
  
  # acs iota baseline ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   agg_method = "aiota",
                   id_tags = c("vars", "rake", "method")),
  
  # only sales tax policy baseline ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732sto",
                   tax_amt = 25,
                   id_tags = c("vars", "rake", "method", "rebate")),
  
  # per pp rebate policy baseline ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732pPPr",
                   tax_amt = 25,
                   id_tags = c("vars", "rake", "method", "rebate")),

  # no rebate policy baseline ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "no_rebate",
                   tax_amt = 25,
                   id_tags = c("vars", "rake", "method", "rebate")),
  
  # low-income rebate policy baseline ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732lir",
                   tax_amt = 25,
                   id_tags = c("vars", "rake", "method", "rebate")),

  # alternate rake variables ----
  rake_factory("beta", "wa", 2016, "tract", reuse = c("state", "year"), use_existing = T),

  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "beta",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   id_tags = c("vars", "rake", "method")),
  
  # alternate siota ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "beta",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   agg_method = "siota",
                   id_tags = c("vars", "rake", "method")),
  
  # alternate aiota ----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "beta",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   agg_method = "aiota",
                   id_tags = c("vars", "rake", "method")),
  
  # stavg electricity prices----
  simulate_factory(vars = "baseline",
                   method = "dhxgboost",
                   rake_vars_case = "alpha",
                   output_vars = c("electricity", "gasoline", "naturalgas",
                                   "fueloil", "gasbottledtank",
                                   "totexp"),
                   state = "wa",
                   year = 2016,
                   raking_geography = "tract",
                   tax_type = "wa",
                   rebate_type = "wa732",
                   tax_amt = 25,
                   electric_case = "stavg",
                   id_tags = c("vars", "rake", "method", "elec")),

  # boundary files ----
  tar_target(tract_boundaries,
             sf::st_read(file.path(here::here(),
                                   "data/shapefiles/censustracts_2016/tl_2016_53_tract.shp"))),
  
  tar_target(puma_boundaries,
             sf::st_read(file.path(here::here(),
                                   "data/shapefiles/pumas_2016/cb_2016_53_puma10_500k.shp"))),
  tar_target(pop_density,
             get_density_info("wa", 2016, "tract") %>% 
    mutate(density_group = factor(case_when(density_quantile == 1 ~ "Density quintile 1 (least dense)",
                                            density_quantile == 2 ~ "Density quintile 2",
                                            density_quantile == 3 ~ "Density quintile 3",
                                            density_quantile == 4 ~ "Density quintile 4",
                                            density_quantile == 5 ~ "Density quintile 5 (most dense)"),
                                  levels = c("Density quintile 1 (least dense)",
                                             "Density quintile 2",
                                             "Density quintile 3",
                                             "Density quintile 4",
                                             "Density quintile 5 (most dense)"),
                                  ordered = T))),
  
  tar_target(impacts_baseline_dhxgboost_alpha_wa732rr,
             compute_sample_impacts.wa732rr(tax_payments_baseline_dhxgboost_alpha,
                                            useful_data_baseline_dhxgboost_alpha,
                                            pop_density)),

  tar_target(median_impact_baseline_dhxgboost_alpha_wa732rr,
             impacts_baseline_dhxgboost_alpha_wa732rr %>% 
               uncount(final_weight) %>% 
               group_by(GEOID) %>% 
               summarize(median_impact = median(netGain)))
)

## Eliminate duplicates ----
my_list <- unlist(my_list)
num_targets = length(my_list)

info <- tibble(target_name = unlist(map(1:num_targets, ~ my_list[[.x]]$settings$name)),
               command = unlist(map(1:num_targets, ~ my_list[[.x]]$command$string)))

my_list[!duplicated(info)]

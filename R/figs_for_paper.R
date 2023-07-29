library(tidyverse)
library(tidycensus)
library(tmap)
library(targets)
library(RColorBrewer)
library(scales)
library(viridis)
library(grid)
library(gridExtra)
library(lvplot)
library(fixest)
source("R/main_functions.R")
source("R/cex_functions.R")
source("R/fit_functions.R")
source("R/sms_functions.R")
source("R/simulation_functions.R")
source("R/acs_median_funcs.R")
source("R/target_factory_functions.R")
source("R/wa_analysis_functions.R")

fig_dir <- here::here("figures")

## load shapefiles for maps ----
tracts <- targets::tar_read(tract_boundaries)
pumas <- targets::tar_read(puma_boundaries)

## set poverty levels and density groups
pl_groups <- c(-Inf, 1, 1.5, 4, 6.25, Inf)

pop_density <- tar_read(pop_density) %>% 
  sf::st_drop_geometry() %>% 
  select(GEOID, density_group)

## Designate main specification ----
vars_id = "baseline"
rake_id = "alpha"
fit_method = "dhxgboost"
case_name = "baseline_dhxgboost_alpha"
case_id = "preferred"

## Construct graphs and maps of main case ----
median_map <- map_median(case_name, case_id, fig_dir, tracts)

## Box plots of impact by poverty level group
impacts <- tar_read_raw(str_c("impacts_", case_name)) %>% 
  uncount(final_weight)

pl_plot_data <- impacts %>% 
  compute_poverty_level_groups(pl_groups) %>% 
  left_join(pop_density)

table(pl_plot_data$pl_ratio_group)/sum(table(pl_plot_data$pl_ratio_group))

pct_gain_plot <- ggplot(pl_plot_data, aes(x = pl_ratio_group, y = pctGain)) +
  geom_boxplot() +
  theme_bw() + 
  xlab("Ratio of income to poverty level") +
  ylab("Gain from policy as % of income*") +
  theme(text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent)
  
level_gain_plot <- ggplot(pl_plot_data, aes(x = pl_ratio_group, y = netGain)) +
  geom_boxplot() +
  theme_bw() + 
  theme(text = element_text(size = 8)) +
  xlab("Ratio of income to poverty level") +
  ylab("Gain from policy ($/hh/yr)") 

plot <- arrangeGrob(level_gain_plot, pct_gain_plot,
                    nrow = 1,
                    top = grid::textGrob("Distribution of gains by poverty level"))

ggsave(str_c(fig_dir, "/pl_dist_", case_id, ".png"), plot,
       width = 6, height = 3.5)

## colored density plot ----
freq_counts <- hist(impacts$netGain, breaks = 100, plot = F)
prob_data <- tibble(interval = seq_along(freq_counts$breaks),
                    density = c(freq_counts$density, tail(freq_counts$density, n = 1)),
                    count = c(freq_counts$count, tail(freq_counts$count, n= 1))) %>% 
  mutate(share = count/sum(count))

quantiles <- quantile(impacts$netGain, probs = seq(from = .1, to = .9, by = .1))
density <- density(impacts$netGain)

plot_data <- as_tibble(density[1:2]) %>% 
  mutate(quant = findInterval(x, quantiles),
         decile = case_when(quant %in% c(0,9) ~ "1st, 10th",
                            quant %in% c(1,8) ~ "2nd, 9th",
                            quant %in% c(2,7) ~ "3rd, 8th",
                            quant %in% c(3,6) ~ "4th, 7th",
                            quant %in% c(4,5) ~ "5th, 6th"))

median_impact <- tar_read_raw(str_c("median_impact_", case_name))
median_freq <- hist(median_impact$median_impact, breaks = freq_counts$breaks, plot = F)
median_frequency_data <- tibble(interval = seq_along(median_freq$breaks),
                                density = c(median_freq$density, 
                                            tail(median_freq$density, n = 1)),
                                count = c(median_freq$count, 
                                          tail(median_freq$count, n= 1))) %>% 
  mutate(share = count/sum(count))

median_plot_data <- tibble(x = seq(from = median_freq$breaks[[1]],
                                   to = tail(median_freq$breaks, n = 1), 
                                   length.out = 10000),
                           interval = findInterval(x, median_freq$breaks)) %>% 
  left_join(median_frequency_data)

median_density <- density(median_impact$median_impact)
median_plot_data <- as_tibble(median_density[1:2])

ggplot(plot_data, aes(x, y)) + 
  geom_ribbon(aes(ymin = 0, ymax = y, fill = decile, group = quant)) +
  geom_line(data = median_plot_data, linewidth = .75) +
  scale_fill_discrete(type = brewer.pal(5,"Paired"),
                      name = "Decile of \nhousehold impacts") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  ylab("Kernel Density") +
  xlab("Household Net Impact ($)") +
  annotate("text", x = -400, y = .005, 
           size = 3,
           label = "census tract\nmedian distribution") +
  annotate("segment", x = -400, xend = -50, y = .0045, yend = .0035, 
           arrow = arrow(length = unit(0.02, "npc"))) +
  annotate("text", x = 750, y = .002, 
           size = 3, 
           label = "household impact\ndistribution") +
  annotate("segment", x = 750, xend = 500, y = .0015, yend = .0001, 
           arrow = arrow(length = unit(0.02, "npc"))) +
  theme(legend.position = c(.85,.7),
        plot.background = element_rect(fill = "white",
                                       linetype = "blank"))
  
ggsave(str_c(fig_dir, "/density_deciles_", case_id, ".png"),
       width = 6, height = 3.5)

## tract level middle distribution ----
deciles <- impacts %>% 
  group_by(GEOID) %>% 
  reframe(quibble(netGain, seq(from = 0, to = 1, by = .1))) %>%
  rename(impact = x,
         percentile = q)

median_order <- deciles %>%
  filter(percentile == .5) %>%
  arrange(impact) %>%
  ungroup() %>%
  mutate(num = row_number()) %>%
  select(GEOID, num) %>%
  left_join(deciles, multiple = "all")

bar_size <- median_order %>%
  arrange(num, percentile) %>%
  group_by(GEOID) %>%
  mutate(ymin = lag(impact),
         colorwgt = round(ifelse(percentile<=.5, .5 - percentile, percentile - .6), 1))

bar_size %>% filter(colorwgt <=.25) %>%
  ggplot(aes(x = num, ymin = ymin, ymax = impact)) +
  geom_linerange(aes(color = as.factor(colorwgt))) +
  geom_line(data = filter(bar_size, percentile == .5),
            aes(num, impact), color = "white") +
  scale_color_manual(values = brewer.pal(5,"Paired"),
                     breaks = c("0.4", "0.3","0.2","0.1","0"),
                     labels = c("1st, 10th", "2nd,9th", "3rd, 8th", "4th, 7th", "5th, 6th"),
                     name = "Decile",
                     guide = guide_legend(override.aes = list(size = 5),
                                          direction = "horizontal")) +
  theme_bw() +
  ylab("Household impact range") +
  xlab("Median household impact rank of census tract") +
  theme(legend.position = "bottom") +
  coord_cartesian(expand = F)

ggsave(file.path(fig_dir, str_c("middle_distribution_", case_id, ".png")),
       width = 6,
       height = 7)

## Distribution separated by PUMA ----
# Merge PUMA info onot results
# First generate GEOID for geocorr
geocorr2014_wa_tract_to_puma <- read_csv(file.path(here::here(),
                                                   "data","pums", 
                                                   "geocorr2014_wa_tract_to_puma.csv")) %>%  
  unite(GEOID,c("county","tract"),sep="") %>%
  mutate(GEOID = str_remove_all(GEOID,"[.]")) %>%
  filter(!GEOID == "County codeTract") 

# Now merge onto ordered_effects
puma_deciles <- deciles %>%
  left_join(geocorr2014_wa_tract_to_puma, by="GEOID")

deciles2 <- puma_deciles %>%
  filter(percentile == .5) %>%
  group_by(puma12) %>%
  summarize(puma_mean = mean(impact,na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(puma_deciles, multiple = "all")

# Arrange by puma12
# Order pumas by median impacts in puma

median_order <- deciles2 %>% 
  filter(percentile == .5) %>% 
  arrange(puma_mean,impact) %>% 
  ungroup() %>% 
  mutate(num = row_number()) %>% 
  select(GEOID, num) %>% 
  left_join(deciles2, multiple = "all")

bar_size <- median_order %>% 
  arrange(num, percentile) %>% 
  group_by(GEOID) %>% 
  mutate(delta =ifelse(percentile == 0, impact, impact - lag(impact)),
         ymin = lag(impact),
         colorwgt = round(ifelse(percentile<=.5, .5 - percentile, percentile - .6), 1))

bar_size %>% filter(colorwgt <=.25) %>%
  ggplot(aes(x = num, ymin = ymin, ymax = impact)) + 
  geom_linerange(aes(color = as.factor(colorwgt))) + 
  geom_line(data = filter(bar_size, percentile == .5), aes(num, impact), color = "white") + 
  scale_color_manual(values = brewer.pal(5,"Paired"),
                     breaks = c("0.4", "0.3","0.2","0.1","0"),
                     labels = c("1st, 10th", "2nd,9th", "3rd, 8th", "4th, 7th", "5th, 6th"),
                     name = "Decile") +
  theme_bw() +
  ylab("Household impact range") +
  xlab("Median household impact rank of census tract") +
  coord_cartesian(expand = F)

ggsave(file.path(fig_dir, str_c("middle_distribution_by_PUMA_", case_id, ".png")),
       width = 6, height = 7)

## case comparison plots ----
comparisons <- tribble(
  ~result_tag,        ~case_name,
  "_wa732sto",    "Sales tax only (STO)",
  "",             "I-732",
  "_wa732lir",    "Low-income rebates (LIR)",
  "_wa732rr",     "Rural rebates (RR)",
  "_wa732pPPr",   "Dividends") %>% 
  mutate(case_number = as.character(row_number()))

comparisons$case_name <- factor(comparisons$case_name, 
                                levels = comparisons$case_name,
                                ordered = T)

income_comparison_data <- map_dfr(comparisons$result_tag,
                                  extract_case_results,
                                  .id = "case_number") %>% 
  left_join(comparisons) %>% 
  mutate(scaled_sales_tax = case_when(case_name == "Sales tax only" ~ netGain + tax_payment,
                                      case_name == "I-732" ~ sales_tax_savings,
                                      TRUE ~ scaled_sales_tax),
         rebate = netGain + tax_payment)
# safe to ignore warning on previous step -- some of the values are only computed in particular rebate cases. Missing values don't cause problems later

comparison_data <- income_comparison_data %>% 
  compute_poverty_level_groups(pl_groups) %>% 
  left_join(pop_density)   

plot_data <- comparison_data %>% 
  uncount(final_weight)

compare_dist_pl(plot_data, c(1,4), fig_dir)
compare_dist_pl(plot_data, c(1,2,3), fig_dir)

plot_data %>% 
  mutate(density_label = str_replace(density_group, "Density quintile ", "")) %>% 
  ggplot(aes(x = fct_rev(density_label), y = netGain, fill = after_stat(LV))) + 
  geom_hline(yintercept = 0) +
  geom_lv(k = 4, width.method = "height", outlier.shape = NA, outlier.size = .5) + 
  scale_fill_brewer(palette = "Blues", direction = -1,
                    labels = c("Median", "IQR", "12.5-25%, 75-87.5%", 
                               "6.25-12.5%, 87.5-93.75%"),
                    name = "") + 
  theme_bw() + 
  xlab("Density quintile of census tract") + 
  ylab("Net gain (loss) from policy ($/hh/yr)") +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip(ylim = c(-200,600), expand = F) +
  facet_wrap("case_name", ncol = 1)

ggsave(here::here(fig_dir, "lv_plots_density_by_policy.png"), width = 6, height = 6)

plot_data %>% 
  ggplot(aes(y = pctGain, x = fct_rev(pl_ratio_group), fill = after_stat(LV))) +
  geom_lv(k = 4, width.method = "height", 
          outlier.shape = NA, outlier.size = .5) + 
  scale_fill_brewer(palette = "Blues", direction = -1,
                    labels = c("Median", "IQR", "12.5-25%, 75-87.5%", 
                               "6.25-12.5%, 87.5-93.75%"),
                    name = "") +
  theme_bw() + 
  xlab("Ratio of income to poverty level") +
  ylab("Percent gain (loss) from policy") +
  coord_flip(ylim = c(-.025,.025), expand = F) + 
  facet_wrap("case_name", ncol = 1)

ggsave(here::here(fig_dir, "lv_plots_pct_pl.png"), width = 6, height = 6)

plot_data %>% 
  filter(case_number == "1") %>% 
  mutate(pct_tax = case_when(pl_ratio < 1 ~ tax_payment/poverty_level,
                             TRUE ~ tax_payment/tot_income)) %>% 
  ggplot(aes(y = pct_tax, x = fct_rev(pl_ratio_group), fill = after_stat(LV))) +
  geom_lv(k = 4, width.method = "height", 
          outlier.shape = NA, outlier.size = .5) + 
  scale_fill_brewer(palette = "Blues", direction = -1,
                    labels = c("Median", "IQR", "12.5-25%, 75-87.5%", 
                               "6.25-12.5%, 87.5-93.75%"),
                    name = "") +
  theme_bw() + 
  xlab("Ratio of income to poverty level") +
  ylab("Tax payment as a percentage of income") +
  coord_flip(ylim = c(0,0.02), expand = F)

ggsave(here::here(fig_dir, "lv_plots_tax_pct_pl.png"), width = 6, height = 3.5)

## construct lir eligibility table ----
impacts_lir <- tar_read(impacts_baseline_dhxgboost_alpha_wa732lir)

lir_by_region <- pop_density %>% 
  right_join(impacts_lir, multiple = "all") %>% 
  uncount(final_weight) %>%
  mutate(wftr_eligible = wf_tax_exemption>0,
         li_policy_type = factor(case_when(wftr_eligible & eligible_hh>0 ~ "both",
                                           wftr_eligible ~ "WFTR only",
                                           eligible_hh>0 ~ "LIR only",
                                           TRUE ~ "neither"),
                                 levels = c("neither", "both", "LIR only", "WFTR only"),
                                 ordered = T)) %>% 
  group_by(density_group, li_policy_type) %>% 
  summarize(num_hh = n(),
            across(c(rebate_shares, wf_tax_exemption, tax_payment, sales_tax_savings),
                   mean)) %>% 
  mutate(shr = num_hh/sum(num_hh))

lir_by_region %>% 
  select(density_group, li_policy_type, shr) %>% 
  mutate(shr = round(shr*100, digit = 1)) %>% 
  pivot_wider(names_from = li_policy_type, values_from = shr) %>% 
  kableExtra::kable(format = "latex",
                    booktabs = "T",
                    col.names = c("Census Tract Density Quintile", "Never eligible", 
                                  "Eligible for WFTR and LIR", "Eligible for LIR only",
                                  "Eligible for WFTR only"),
                    align = "lcccc") %>% 
  kableExtra::column_spec(column = 2:5, width = "0.75in", latex_valign = "b") %>% 
  kableExtra::save_kable(here::here("tables/lir-eligibility.tex"))

## carbon content map
co2_data <- tracts %>% 
  right_join(read_csv("prepared-data/electric_info_tract.csv", col_type = "cdd"))

ggplot(data = co2_data) +
  geom_sf(aes(geometry = geometry, fill = carbon_content), color = NA) +
  scale_fill_viridis_c(trans = "sqrt", name = "Carbon content\n(kg/kWh)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))

ggsave(file.path(fig_dir, "map_co2.png"),
       width = 6)

#compare electric cases ----
hh_impacts_detailed <- tar_read(impacts_baseline_dhxgboost_alpha) %>% 
  select(GEOID, hhid, detailed_impact = netGain, final_weight)

hh_impacts_compare <- tar_read(impacts_baseline_dhxgboost_alpha_stavg) %>% 
  select(GEOID, hhid, stavg_impact = netGain, final_weight) %>% 
  left_join(hh_impacts_detailed) %>% 
  mutate(error = stavg_impact - detailed_impact) %>%
  uncount(final_weight)

hh_impact_by_tract <- hh_impacts_compare %>% 
  group_by(GEOID) %>% 
  summarize(mean_error = mean(error), median_error = median(error))

error_map <- tracts %>% 
  left_join(hh_impact_by_tract) %>% 
  tm_shape() +
  tm_fill(col = "median_error",
          palette = "PuOr", 
          colorNA = "white",
          midpoint = 0,
          title = "Difference in estimated gain ($)",
          legend.hist = T) +
  tm_layout(legend.outside = T,
            frame = F) +
  tm_shape(pumas) + tm_borders()

tmap_save(error_map, file.path(fig_dir, "electricity_error_preferred.png"),
          width = 6)

## AI to IOTA baseline ----
plot_ai_vs_iota("baseline_dhxgboost_alpha", "preferred")

## compare characteristics ----
compare_alpha <- compare_median_chars("baseline_dhxgboost_alpha")
compare_beta <- compare_median_chars("baseline_dhxgboost_beta")

## discrete
combined_discrete_summaries <- summarize_discrete_var_comparisons(compare_alpha) %>% 
mutate(rake_case = "preferred SMS sample") %>%
  bind_rows(mutate(summarize_discrete_var_comparisons(compare_beta),
                   rake_case = "fewer raking variables"))

ggplot(combined_discrete_summaries, aes(y = name, x = share_matched, fill = rake_case)) +
  geom_col(position = position_dodge2(reverse = T)) +
  theme_bw() +
  scale_fill_discrete(name = "") +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
  xlab("Share of tracts where medians match") +
  ylab("")

ggsave(file.path(fig_dir, "discrete_match_combined.png"),
       width = 6,
       height = 4)

## continuous
combined_continuous_summaries <- summarize_continuous_var_comparisons(compare_alpha) %>% 
  mutate(rake_case = "preferred SMS sample") %>%
  bind_rows(mutate(summarize_continuous_var_comparisons(compare_beta),
                   rake_case = "fewer raking variables"))

ggplot(combined_continuous_summaries,
         aes(x = scaled_diff_sd,
             y = name,
             color = rake_case)) +
    geom_boxplot(position = position_dodge2(reverse = T), alpha = .25) +
    theme_bw() +
    xlab("Standard deviations of ACS average") +
    ylab("") +
    scale_color_discrete(name = "") + 
    ggtitle("Difference in tract average") +
    labs(subtitle = "Synthetic - ACS")
  
  ggsave(file.path(fig_dir, "continuous_char_match_combined.png"),
         width = 6, height = 4)
  
## replace1 comparisons ----
combined_replace1_comparison <- construct_replace1_comparisons("baseline", 
                                                               "dhxgboost", 
                                                               "alpha") %>%
  mutate(rake_case = "preferred SMS sample") %>%
  bind_rows(mutate(construct_replace1_comparisons("baseline", "dhxgboost", "beta"),
                   rake_case = "fewer raking variables"))
  
combined_replace1_comparison %>%
  ggplot(aes(x = delta,
             y = swap_var,
             color = rake_case)) +
  geom_boxplot(size = .25,
               position = position_dodge2(reverse = T),
               alpha = .25) +
  theme_bw() +
  scale_color_discrete(name = "") +
  xlab("ACS impact - Replace one impact") +
  ylab("Replaced variable")

ggsave(file.path(fig_dir, "replace1_boxplot_combined.png"),
       width = 6, height = 4)

## compare to zero elasticity case ----

median_map <- map_median("baseline_dhxgboost_alpha_ctam", "ctam_elasticity", fig_dir, tracts)

compare_elasticity <- tar_read(median_impact_baseline_dhxgboost_alpha) %>% 
  select(GEOID, zero_elasticity = median_impact) %>% 
  left_join(select(tar_read(median_impact_baseline_dhxgboost_alpha_ctam),
                   GEOID, ctam_elasticity = median_impact))

ggplot(compare_elasticity, aes(x = ctam_elasticity, y = zero_elasticity)) +
  geom_point(alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Median impact: elasticity = 0") +
  xlab("Median impact: CTAM elasticity estimates") +
  theme_bw() +
  coord_fixed(xlim = c(-150,250), ylim = c(-150,250))

ggsave(file.path(fig_dir, "impact_of_elasticity_preferred.png"), width = 6, height = 6)

## add new figs for revision
impacts <- tar_read(impacts_baseline_dhxgboost_alpha)
combined_data <- tar_read(scaled_consumption_baseline_dhxgboost_alpha) %>% 
  filter(product == "totexp") %>% 
  rename(hhid = hhkey,
         totexp = consumption) %>% 
  right_join(impacts) %>% 
  mutate(pct_spending = netGain/totexp,
         spending_quintile = ntile(totexp, 5),
         pl_ratio = tot_income/poverty_level,
         pl_ratio_group = cut(pl_ratio, c(-Inf, 1, 1.5, 4, 6.25, Inf), ordered_result = T))

ggplot(combined_data, aes(x = pl_ratio_group, y = pct_spending)) + 
  geom_boxplot() +
  theme_bw() + 
  xlab("Ratio of income to poverty level") +
  ylab("Gain from policy as % of projected total spending") +
  theme(text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent)

ggsave(file.path(fig_dir,"pl_dist_baseline_percent_spending.png"),
       width = 3, height = 3.5)

characteristics <- tar_read(useful_data_baseline_dhxgboost_alpha)

impacts_with_chars <- tar_read(impacts_baseline_dhxgboost_alpha_wa732sto) %>% 
  rename(hhkey = hhid) %>% 
  left_join(characteristics) %>% 
  mutate(income = tot_income/1000,
         educ_cat = factor(educ_cat_hhldr, ordered = F),
         age_cat = factor(age_cat_hhldr, ordered = F),
         year_built = factor(year_built,
                             levels = c("pre1940", "1940s", "1950s", "1960s",
                                        "1970s", "1980s", "1990s", "early2000s",
                                        "post2005", "rent"))
  )

geoid_model <- feols(netGain ~ adults + children + spouse_present + 
                       income + age_cat + educ_cat + num_in_labor_force +
                       num_unemployed + food_stamp + num_vehicles + sf_detached +
                       homeowner + num_rooms + year_built + heating_fuel | GEOID, 
                     data = impacts_with_chars)

gtsummary::tbl_regression(geoid_model) %>% 
  gtsummary::as_gt() %>% 
  gt::gtsave(filename = "tables/regression_output.tex")

download_data = !dir.exists("data-raw/form861")
## download (if needed) and process electricity price data
if (download_data){
  dir.create("data-raw/form861")
  for(year in 1990:2021){
    
    destfile = str_c("data-raw/form861/", year, ".zip")
    
    if (year >= 2020) {
      path = "https://www.eia.gov/electricity/data/eia861/zip/f861"
    } else if (year >= 2012) {
      path = "https://www.eia.gov/electricity/data/eia861/archive/zip/f861"
    } else {
      path = "https://www.eia.gov/electricity/data/eia861/archive/zip/861_"
    }
    
    download.file(url = str_c(path,
                              year,
                              ".zip"),
                  destfile = destfile)
    
    unzip(zipfile = destfile, junkpaths = T, exdir = "data-raw/form861")
  }
}

summarize_yearly_data <- function(year){
  
  filename <- str_c("data-raw/form861/Sales_Ult_Cust_", year, ".xlsx")
  if(!file.exists(filename)){
    filename <- str_c("data-raw/form861/Sales_Ult_Cust_", year, ".xls")}
  
  wa_data <- readxl::read_excel(filename,
                                skip = 2,
                                .name_repair = "universal") %>% 
    filter(State == "WA")
  
  res_rev_name <- grep("Dollars", colnames(wa_data), value = T)[1]
  res_sales_name <- grep("Megawatthours", colnames(wa_data), value = T)[1]
  tot_sales_name <- tail(grep("Megawatthours", colnames(wa_data), value = T), n = 1)
  
  if(is.null(res_rev_name)){browser()}
  if(is.null(res_sales_name)){browser()}
  
  if(!"Part" %in% colnames(wa_data)) {
    wa_data$Part = "A"}
  
  if(!"Ownership" %in% colnames(wa_data)) {
    wa_data$Ownership = "NA"}
  
  this_year <- wa_data %>% 
    select(year = Data.Year, Utility.Name, State, Ownership, 
           res_rev = !!res_rev_name, res_sales = !!res_sales_name, tot_sales = !!tot_sales_name, Part) %>% 
    filter(!res_rev == ".") %>% 
    mutate(residential_sales = case_when(Part == "C" ~ 0,
                                         TRUE ~ as.numeric(res_sales)),
           residential_revenue = as.numeric(res_rev),
           total_sales = case_when(Part == "C" ~ 0,
                                   TRUE ~ as.numeric(tot_sales))) %>% 
    group_by(year, Utility.Name, Ownership) %>% 
    summarize(res_rev = sum(residential_revenue), 
              res_sales = sum(residential_sales),
              tot_sales = sum(total_sales))
}

all_data <- map_df(1990:2021, summarize_yearly_data) %>% 
  mutate(res_price = res_rev/res_sales) %>% 
  filter(!str_detect(Utility.Name, "Adjustment"))

util_names <- read_csv("data/utility_list.csv") %>% 
  mutate(Ownership = if_else(is.na(Ownership), "NA", Ownership))

cpi <- read_csv("data/CPIAUCSL.csv") %>% 
  mutate(year = year(DATE),
         cpi_value = as.numeric(CPIAUCSL)) %>% 
  select(year, cpi_value)

base_cpi_year <- cpi %>% 
  filter(year == 2015)

cpi <- cpi %>% 
  mutate(cpi_value = cpi_value/base_cpi_year$cpi_value)

unified_names <- all_data %>% 
  left_join(util_names) %>% 
  filter(!category %in% c("other", "federal")) %>% 
  left_join(cpi) %>% 
  mutate(real_rev = res_rev/cpi_value,
         real_price = real_rev/res_sales)

category_average <- unified_names %>% 
  group_by(year, category) %>% 
  summarize(rev = sum(res_rev), real_rev = sum(real_rev), sales = sum(res_sales)) %>% 
  mutate(price = rev/sales,
         real_price = real_rev/sales)

ggplot(category_average, aes(year, real_price, color = category)) +
  geom_line(linewidth = 1) +
  theme_bw() + 
  ylab("Average residential electricity price ($2015/kwh)") +
  xlab("")

price_changes <- unified_names %>% 
  filter(!is.na(util_fm)) %>% 
  group_by(util_fm) %>%
  arrange(util_fm, year) %>% 
  mutate(price_change = log(res_price) - log(lag(res_price))) %>% 
  filter(!is.na(price_change))

big_utilities <- unified_names %>%
  filter(year > 2015) %>% 
  group_by(util_name, util_fm, category) %>%
  summarize(avg_sales = mean(res_sales)) %>%
  arrange(desc(avg_sales)) %>%
  head(10)

gas_price <- read_csv("data/U.S._Natural_Gas_Citygate_Price.csv", 
                      skip = 5,
                      col_names = c("year", "gas_price")) %>% 
  left_join(cpi) %>% 
  arrange(year) %>% 
  mutate(real_gas_price = gas_price/cpi_value/100,
         delta_gas = log(gas_price) - log(lag(gas_price)))

category_average %>% 
  left_join(gas_price) %>% 
  ggplot(aes(year, real_price, color = category)) +
  geom_line(linewidth = 1) +
  geom_line(aes(year, real_gas_price, linetype = "$/thousand cubic feet"), color = "black") +
  theme_bw() + 
  ylab("Real price (2015 dollars)") +
  xlab("") +
  scale_color_discrete(name = "Electricity price\nby utility type\n($/kWh)") +
  scale_linetype_manual(values = "dashed", name = "Natural gas price")

ggsave(file.path(fig_dir, "average_price_by_category.png"), 
       width = 6, height = 4)

# look at correlations among large utilities
wide_utilities <- price_changes %>% 
  filter(util_name %in% big_utilities$util_name) %>% 
  mutate(util_short = str_remove(util_name, "PUD No [0-9] of | Inc")) %>% 
  ungroup() %>% 
  arrange(year, category, util_short) %>% 
  select(year, util_short, real_price) %>% 
  pivot_wider(names_from = util_short, values_from = real_price, names_repair = "universal") 

wide_utilities %>% 
  select(-year) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(lab = T, hc.order = T) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "price_correlation.png"), 
       width = 6, height = 6)

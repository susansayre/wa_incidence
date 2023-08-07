# name: votes_tract.r
# authors: Nathan W. Chan and Susan S. Sayre
# description: Convert original precinct vote tallies for I-732 to census tract level

#rm(list = ls())

library(here)
library(sf)
library(tidyverse)
library(tigris)
library(tmap)
source("R/wa_analysis_functions.R")

directory <- here::here()

#### Load files ----

# Load raw vote tallies at precinct level
votes_precinct <- read_csv(file.path(directory,"data","votes",
                                     "2016Gen_Precinct_Results_GIS-Ready.csv")) %>%
  select("PrecinctCode","CONGDIST","G16I0732Y","G16I0732N","G16PRSCLIN","G16PRSTRUM") %>%
  rename(I732_yes = G16I0732Y, I732_no = G16I0732N, ST_CODE = PrecinctCode, 
         pres_clinton = G16PRSCLIN, pres_trump = G16PRSTRUM)

# Load precinct shape file
shape_precinct <- st_read(file.path(directory, "data","shapefiles", 
                                    "precincts_2016", "Statewide_Prec_2016.shp")) %>% 
  st_transform("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs") %>% 
  select(-LEGDIST,-CONGDIST,-CCDIST) %>%
  st_make_valid()

# Load census tract shape file
shape_tract <- st_read(file.path(directory, "data","shapefiles", "censustracts_2016", "tl_2016_53_tract.shp")) %>% 
  st_transform("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs") 

# Merge precinct shapefile onto vote tallies
votes_precinct <- merge(shape_precinct, votes_precinct, by="ST_CODE")

#### Intersect precincts with census tracts, get area weights ----
# resource: https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-intersection-transformative-join.html

# Intersect two shape files
votes_prec_tract <- st_intersection(shape_tract,votes_precinct) %>%
  mutate(prec_tract = paste0(ST_CODE,"-",GEOID))

    # # Plot intersections
    # ggplot() +
    #   geom_sf(data = votes_prec_tract, aes(fill = COUNTYCODE), alpha = 0.3) +
    #   scale_fill_discrete(name = "Counties")
    # 
    # # Test plot of Clark County
    # tm_shape(filter(votes_prec_tract, COUNTY == "Clark")) +
    #   tm_polygons(col="prec_tract") +
    #   tm_layout(frame = FALSE)
    # 
    # # Test plot of Tract 042006
    # tm_shape(filter(votes_prec_tract, GEOID == "042006")) +
    #   #  tm_polygons(col="prec_tract") +
    #   tm_polygons(col="ST_CODE") +
    #   tm_layout(frame = FALSE)

# Calculate areas of intersected shapes; create weights
votes_prec_tract <- votes_prec_tract %>%
  #--- get area of each intersected polygon---#
  mutate(area = as.numeric(st_area(.))) %>%
  #--- calculate area of intersection relative to total precinct area ---#
  group_by(ST_CODE) %>%
  mutate(weight = area / sum(area))

    # # Check that sum of prec-tract intersections sum to tract areas
    # votes_prec_tract %>%
    #   filter(GEOID == "042006") %>%
    #   select(area) %>%
    #   summarize(test = sum(area))
    # 
    # shape_tract %>%
    #   filter(GEOID == "042006") %>%
    #   st_area()
    # 
    # # Check that areas for prec-tract intersections sum to precinct areas
    # votes_prec_tract %>%
    #   filter(ST_CODE == "CR00000298") %>%
    #   select(area) %>%
    #   summarize(test = sum(area))
    # 
    # shape_precinct %>%
    #   filter(ST_CODE == "CR00000298") %>%
    #   st_area()
    # 
    # # Check that weights for prec-tract intersections sum to 1 at precinct level
    # votes_prec_tract %>%
    #   filter(ST_CODE == "CR00000298") %>%
    #   select(weight) %>%
    #   summarize(test = sum(weight))

# attribute vote shares according to area weights
votes_tract <- votes_prec_tract %>%
  sf::st_drop_geometry() %>% 
  group_by(GEOID) %>%
  summarize(votes_tract_yes = sum(weight*I732_yes),
            votes_tract_no = sum(weight*I732_no),
            votes_tract_clinton = sum(weight*pres_clinton),
            votes_tract_trump = sum(weight*pres_trump))

    # # Check that total votes are preserved
    # sum(votes_tract$votes_tract_yes)
    # sum(votes_tract$votes_tract_no)   
    # 
    # sum(votes_precinct$I732_yes)
    # sum(votes_precinct$I732_no)
    
# Calculate %yes and %no for each tract
votes_tract <- votes_tract %>%
  mutate(perc_tract_yes = votes_tract_yes/(votes_tract_yes + votes_tract_no),
         perc_tract_no = votes_tract_no/(votes_tract_yes + votes_tract_no),
         perc_tract_clinton = votes_tract_clinton/(votes_tract_clinton + votes_tract_trump),
         perc_tract_trump = votes_tract_trump/(votes_tract_clinton + votes_tract_trump)) %>% 
  mutate(tract_type = factor(case_when(perc_tract_clinton > .65 ~ "strong clinton",
                                       perc_tract_clinton > .55 ~ "clinton",
                                       perc_tract_clinton > .45 ~ "swing",
                                       perc_tract_clinton > .35 ~ "trump",
                                       !is.na(perc_tract_clinton) ~ "strong trump"),
                             levels = c("strong trump", "trump", "swing", 
                                        "clinton", "strong clinton")),
         percent_yes = perc_tract_yes*100)

    
# Vote shares are now available at tract level
# Ready to merge with incidence data for incidence-vote analysis

#### add incidence data ----
all_impacts <- targets::tar_read(impacts_baseline_dhxgboost_alpha) %>% 
  uncount(final_weight) 

tract_summaries <- all_impacts %>% 
  mutate(per_adult_gain = netGain/adults) %>% 
  uncount(adults) %>% 
  group_by(GEOID) %>%
  summarize(median_pa_gain = median(per_adult_gain),
            p90_pa_gain = quantile(per_adult_gain,.9),
            p10_pa_gain = quantile(per_adult_gain, .1)) %>% 
  left_join(votes_tract) %>% 
  mutate(clinton_margin = perc_tract_clinton - perc_tract_trump,
         p90_delta = p90_pa_gain - median_pa_gain,
         p10_delta = median_pa_gain - p10_pa_gain)

colors <- c(rgb(0,.4,1), rgb(.25, .3, .75), rgb(.5,.2,.5), rgb(.75, .1, .25), rgb(1,0,0))

tract_type_key = "Tracts classified based on share of presidential votes cast for Clinton. Strong Trump = [0, .35], Trump = (.35, .45], Swing = (.45,.55], Clinton = (.55,.65], Strong Clinton = (.65, 1]"
ggplot(tract_summaries, aes(median_pa_gain, perc_tract_yes)) + 
  theme_bw() +
  geom_point(aes(color = tract_type,
                 fill = tract_type),
             alpha = .5) + 
    geom_smooth(method = "lm", 
                aes(color = tract_type),
                se = F) +
  scale_color_manual(values = rev(colors),
                     name = "Tract category") + 
  ylab("Share voting yes on I-732") + 
  xlab("Median adult incidence in tract ($/adult/yr)") +
  guides(color = "legend", fill = "none")

ggsave("figures/votes_v_impact.png", width = 6, height = 4)

ggplot(tract_summaries, aes(median_pa_gain, perc_tract_yes)) + 
  theme_bw() +
  geom_point(aes(color = tract_type,
                 fill = tract_type),
             alpha = .5) + 
  geom_smooth(method = "lm", 
              aes(color = tract_type),
              se = F) +
  scale_color_manual(values = rev(c("grey0", "grey15", "grey30", "grey45", "grey60")),
                     name = "Tract category") + 
  ylab("Share voting yes on I-732") + 
  xlab("Median adult incidence in tract ($/adult/yr)") +
  guides(color = "legend", fill = "none")

ggsave("figures/votes_v_impact_bw.png", width = 6, height = 4)

coef_names <- c("(Intercept)" = "Intercept",
               "median_pa_gain" = "Median per adult gain",
               "p90_delta" = "90th - median",
               "p10_delta" = "median - 10th")

tract_summaries %>%
  nest_by(tract_type) %>%
  mutate(models = list(lm(percent_yes ~ median_pa_gain, data))) %>%
  pull(models, name = tract_type) %>%
  modelsummary::modelsummary(stars = c('*' = .05, '**' = .01, "***" = .001),
                             gof_map = c("nobs", "r.squared"),
                             coef_rename = coef_names,
                             output = "latex") %>% 
  write_no_table_envir("tables/vote_model.tex")

quintile_summaries <- all_impacts %>% 
    mutate(pcGain = netGain/adults) %>%
    uncount(adults) %>%
    group_by(GEOID) %>% 
    mutate(quintile = ntile(pcGain,5),
           median_income = median(tot_income)) %>% 
    group_by(GEOID, quintile, median_income) %>% 
    summarize(quintile_mean = mean(netGain)) %>% 
    pivot_wider(names_from = quintile, 
                names_prefix = "q", 
                values_from = quintile_mean) %>% 
    left_join(votes_tract) %>% 
    mutate(clinton_margin = perc_tract_clinton - perc_tract_trump,
           q1q2delta = q2 - q1,
           q2q3delta = q3 - q2,
           q4q3delta = q4 - q3,
           q5q4delta = q5 - q4,
           b60 = (q1+q2+q3)/3)
  
  coef_names = c("(Intercept)" = "Intercept",
                 "q3" = "\\mu_3",
                 "q1q2delta" = "\\mu_2 - \\mu_1",
                 "q2q3delta" = "\\mu_3 - \\mu_2",
                 "q4q3delta" = "\\mu_4 - \\mu_3",
                 "q5q4delta" = "\\mu_5 - \\mu_4")
  
  quintile_summaries %>%
    ungroup() %>% 
    nest_by(tract_type) %>%
    mutate(models = list(lm(percent_yes ~ q1q2delta + q2q3delta + 
                              q3 + q4q3delta + q5q4delta, data))) %>%
    pull(models, name = tract_type) %>%
    modelsummary::modelsummary(stars = c('*' = .05, '**' = .01, "***" = .001),
                               gof_map = c("nobs", "r.squared"),
                               coef_map = coef_names,
                               escape = F,
                               output = "latex") %>% 
  write_no_table_envir("tables/vote_impact_quintile.tex")
  
  quintile_summaries %>%
    ungroup() %>% 
    nest_by(tract_type) %>%
    mutate(models = list(lm(percent_yes ~ b60 + q4 + q5, data))) %>%
    pull(models, name = tract_type) %>%
    modelsummary::modelsummary(stars = c('*' = .05, '**' = .01, "***" = .001),
                               gof_map = c("nobs", "r.squared"))
  
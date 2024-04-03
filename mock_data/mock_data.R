## ------------------------------------------ ##
#          Delta -- Infrastructure
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Purpose:
## Create a mock data set for our Shiny app

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Create necessary sub-folder(s)
dir.create(path = file.path("mock_data"), showWarnings = F)

## ------------------------------------------ ##
#              Data Creation -----
## ------------------------------------------ ##

prob_flood_df <- data.frame(island_tract = 1:139) %>%
  mutate(island_tract = paste0("island_", island_tract),
         high_flow = runif(139, min = 0, max = 1),
         earthquake = runif(139, min = 0, max = 1),
         levee_cond = runif(139, min = 0, max = 1),
         subsidence = runif(139, min = 0, max = 0.3),
         levee_invent = sample(1:20, 139, replace = T),
         prob_flood = high_flow*earthquake*levee_cond*subsidence*levee_invent)

impact_econ_df <- data.frame(island_tract = 1:139) %>%
  mutate(island_tract = paste0("island_", island_tract),
         loss_life = runif(139, min = 0, max = 4),
         prop_damage = runif(139, min = 0, max = 50),
         water_supply = runif(139, min = 0, max = 8),
         land_use = runif(139, min = 0, max = 500),
         ghg_seq = runif(139, min = 0, max = 300),
         impact_econ = loss_life*prop_damage*water_supply*land_use*ghg_seq)

impact_ecol_df <- data.frame(island_tract = 1:139) %>%
  mutate(island_tract = paste0("island_", island_tract),
         degrade_habit = runif(139, min = 0, max = 0.5),
         enhance_habit = runif(139, min = 0, max = 0.5),
         current_restore = sample(1:50, 139, replace = T),
         water_storage = runif(139, min = 0, max = 500),
         habit_acre = runif(139, min = 0, max = 1000),
         water_quality = runif(139, min = 0, max = 1),
         water_recharge = runif(139, min = 0, max = 1),
         carbon_stock = runif(139, min = 0, max = 500),
         climate_invest = sample(1:10, 139, replace = T),
         impact_ecol = degrade_habit*enhance_habit*current_restore*water_storage*habit_acre*
           water_quality*water_recharge*carbon_stock*climate_invest)

social_vul_df <- data.frame(island_tract = 1:139) %>%
  mutate(island_tract = paste0("island_", island_tract),
         water_loss = runif(139, min = 0, max = 0.5),
         income = sample(10000:100000, 139, replace = T),
         education = sample(1:5, 139, replace = T),
         race = sample(1:6, 139, replace = T),
         food_access = sample(1:8, 139, replace = T),
         tribal_bound = sample(1:2, 139, replace = T),
         age = sample(1:95, 139, replace = T),
         drink_contam = runif(139, min = 0, max = 0.5),
         pest_exposure = runif(139, min = 0, max = 0.5),
         traffic_impacts = sample(1:10, 139, replace = T),
         waste_sites = sample(1:10, 139, replace = T),
         impaired_water = runif(139, min = 0, max = 0.5),
         card_disease = runif(139, min = 0, max = 0.5),
         housing_burden = sample(1:10, 139, replace = T),
         agric_reliance = sample(1:10, 139, replace = T),
         social_vul = water_loss*income*education*race*food_access*
           tribal_bound*age*drink_contam*pest_exposure*traffic_impacts*
           waste_sites*impaired_water*card_disease*housing_burden*agric_reliance,
         social_vul_scaled = scales::rescale(social_vul, to = c(1,10))) %>%
  select(-social_vul) %>%
  rename(social_vul = social_vul_scaled)

risk_df <- list(prob_flood_df, impact_econ_df, impact_ecol_df, social_vul_df) %>%
  purrr::reduce(dplyr::left_join, by = 'island_tract') %>%
  mutate(risk = (prob_flood)*(impact_econ)*(impact_ecol)*(social_vul)) %>%
  mutate(risk_scaled_percent = scales::rescale(risk, to = c(0,100)))

write.csv(risk_df, file.path("mock_data", "mock_rand_data.csv"), row.names = F)

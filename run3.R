#dap for verification between repeat and independent.var
dap3 <- read.csv("input2/dap_jmcna3.csv", header = T, stringsAsFactors = F)

# list_of_results3 <-  from_analysisplan_map_to_output(data_jmcna,
#                                                     analysisplan = dap3,
#                                                     weighting = weight.function,
#                                                     cluster_variable_name = NULL,
#                                                     questionnaire = questionnaire, confidence_level = 0.90)
# list_of_results3 %>% saveRDS("output/list_of_results_jmcna3.RDS")
list_of_results3 <- readRDS("output/list_of_results_jmcna3.RDS")


big_table3 <- list_of_results3$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#test 3.1
#cannot have independent.variable with NAs or ""

#test 3.2
#select_one no SL
verif_breadwinner3 <- big_table3 %>%
  filter(dependent.var == "breadwinner") %>%
  select(dependent.var.value, repeat.var.value, numbers)

data_jmcna %>%
  group_by(district_idp, breadwinner) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(district_idp) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_breadwinner3, by = c("breadwinner" = "dependent.var.value", 
                                      "district_idp" = "repeat.var.value")) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers, 
         verif_prop = abs(prop-numbers)) %>%
  filter(verif == F) %>% 
  arrange(desc(verif_prop))
#not equal

# select_multiple no SL
verif_enough_water.drinking3 <- big_table3 %>%
  filter(dependent.var == "enough_water", 
         dependent.var.value == "drinking") %>%
  select(dependent.var.value, repeat.var.value, numbers)

data_jmcna %>%
  group_by(district_idp, enough_water.drinking) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(district_idp) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(enough_water.drinking == 1) %>%
  left_join(verif_enough_water.drinking3, 
            by = c("district_idp" = "repeat.var.value")) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers, 
         verif_prop = abs(prop-numbers)) %>%
  filter(verif == F) %>% 
  arrange(desc(verif_prop))
#not equal





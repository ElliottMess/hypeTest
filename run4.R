#dap for verification between repeat and independent.var
dap4 <- read.csv("input2/dap_jmcna4.csv", header = T, stringsAsFactors = F)

# list_of_results4 <-  from_analysisplan_map_to_output(data_jmcna,
#                                                      analysisplan = dap4,
#                                                      weighting = weight.function,
#                                                      cluster_variable_name = NULL,
#                                                      questionnaire = questionnaire, confidence_level = 0.90)
# list_of_results4 %>% saveRDS("output/list_of_results_jmcna4.RDS")
list_of_results4 <- readRDS("output/list_of_results_jmcna4.RDS")

big_table4 <- list_of_results4$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

big_table4 <- big_table4 %>% 
  mutate(methode = case_when(is.na(repeat.var.value) ~ "independent.var", 
                             is.na(independent.var.value) | independent.var.value == "NA" ~ "repeat.var"),
         independent_repeat.value = paste(independent.var.value, repeat.var.value), 
         independent_repeat.value = gsub("NA", "", independent_repeat.value),
         independent_repeat.value = str_trim(independent_repeat.value)) %>% 
  select(dependent.var, dependent.var.value, independent_repeat.value, numbers, se, min, max, methode ) %>%
  mutate(dependent.var.value = ifelse(is.na(dependent.var.value), "NA", as.character(dependent.var.value)))

split_bigtable4 <- big_table4 %>%
  split.data.frame(big_table4$methode)

#test 4.1.
wide_table4 <- split_bigtable4[[1]] %>%
  left_join(select(split_bigtable4[[2]],dependent.var, dependent.var.value, independent_repeat.value, numbers),
            by = c("dependent.var", "dependent.var.value", "independent_repeat.value")) %>% 
  select(-c("methode", "se", "min", "max")) %>%
  rename(independent_methode = numbers.x, 
         repeat_methode = numbers.y) %>%
  mutate(independent_methode = round(independent_methode, digits = 4),
         repeat_methode =round(repeat_methode, digits = 4),
         verif = independent_methode == repeat_methode, 
         difference = abs(independent_methode - repeat_methode)) %>%
  arrange(desc(difference))
#not equal

View(wide_table4)

#test 4.2.
#select_one no SL
verif_breadwinner4 <- wide_table4 %>%
  filter(dependent.var == "breadwinner") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(resp_gender, breadwinner) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(resp_gender) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_breadwinner4, by = c("breadwinner" = "dependent.var.value", 
                                       "resp_gender" = "independent_repeat.value")) %>%
  rename(numbers = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers, 
         verif_prop = abs(prop-numbers)) 
# equal

# select_multiple no SL
verif_enough_water.drinking4 <- wide_table4 %>%
  filter(dependent.var == "enough_water", 
         dependent.var.value == "drinking") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(resp_gender, enough_water.drinking) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(resp_gender) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(enough_water.drinking == 1) %>%
  left_join(verif_enough_water.drinking4, 
            by = c("resp_gender" = "independent_repeat.value")) %>%
  rename(numbers = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers, 
         verif_prop = abs(prop-numbers))
# equal

# select_one SL  
verif_child_thin4 <- wide_table4 %>%
  filter(dependent.var == "child_thin") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(resp_gender, child_thin) %>%
  filter(child_thin != "") %>%
  summarise(ww = sum(weightss)) %>%
  group_by(resp_gender) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_child_thin4, by = c("child_thin" = "dependent.var.value", 
                                     "resp_gender" = "independent_repeat.value")) %>%
  rename(numbers = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers)
# equal

# select_multiple SL 
verif_latrine_features.door4 <- wide_table4 %>%
  filter(dependent.var == "latrine_features", 
         dependent.var.value == "door") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(resp_gender, latrine_features.door) %>%
  filter(!is.na(latrine_features.door)) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(resp_gender) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(latrine_features.door == 1) %>%
  left_join(verif_latrine_features.door4, 
            by = c("resp_gender" = "independent_repeat.value")) %>%
  rename(numbers = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers, 
         diff_verif = abs(prop - numbers))
#not equal

# integer no SL 
verif_females_18_404 <- wide_table4 %>%
  filter(dependent.var == "females_18_40") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  select(resp_gender, females_18_40, weightss) %>%
  group_by(resp_gender) %>%
  mutate(females_18_40_w = females_18_40 * weightss) %>%
  summarise(ww = sum(females_18_40_w),
            sum_w = sum(weightss)) %>%
  mutate(mean = ww / sum_w)  %>%
  left_join(verif_females_18_404, 
            by = c("resp_gender" = "independent_repeat.value")) %>%
  rename(numbers = independent_methode) %>%
  mutate(mean = round(mean, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = mean == numbers)
# equal

# integer SL enrolled_boys_6_12 
verif_enrolled_boys_6_124 <- wide_table4 %>%
  filter(dependent.var == "enrolled_boys_6_12") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode) 

data_jmcna %>%
  select(resp_gender, enrolled_boys_6_12, weightss) %>%
  filter(!is.na(enrolled_boys_6_12)) %>%
  group_by(resp_gender) %>%
  mutate(enrolled_boys_6_12_w = enrolled_boys_6_12 * weightss) %>%
  summarise(ww = sum(enrolled_boys_6_12_w),
            sum_w = sum(weightss)) %>%
  mutate(mean = ww / sum_w) %>%
  left_join(verif_enrolled_boys_6_124, 
            by = c("resp_gender" = "independent_repeat.value")) %>%
  rename(numbers = independent_methode) %>%
  mutate(mean = round(mean, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = mean == numbers)
# equal

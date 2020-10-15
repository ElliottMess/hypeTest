library(hypegrammaR)
library(tidyverse)

### Import main dataset ###
data_jmcna <- read.csv("input2/2020_jmcna.csv", header = T, stringsAsFactors = F)

samplingframe <- read.csv("input2/sf.csv", stringsAsFactors = F)
samplingframe$stratum_id <- paste(samplingframe$district, samplingframe$idp_settlement, sep = "_")
data_jmcna$stratum_id <- paste(data_jmcna$district, data_jmcna$idp_settlement, sep = "_")

weight.function <- map_to_weighting(sampling.frame = samplingframe, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "Population",
                                    sampling.frame.stratum.column = "stratum_id",
                                    data = data_jmcna)

data_jmcna$weightss <- weight.function(data_jmcna)

kobochoices <- read.csv("input2/choices.csv", header = T, stringsAsFactors = F)

koboquestions <- read.csv("input2/questions.csv", header = T, stringsAsFactors = F)

questionnaire<-load_questionnaire(data = data_jmcna,
                                  questions = koboquestions,
                                  choices = kobochoices,
                                  choices.label.column.to.use = "label::English")
#warning is correct, I just took the variables of interest.

#dap for verification between repeat and independent.var
dap <- read.csv("input2/dap_jmcna.csv", header = T, stringsAsFactors = F)

# list_of_results <-  from_analysisplan_map_to_output(data_jmcna,
#                                                     analysisplan = dap,
#                                                     weighting = weight.function,
#                                                     cluster_variable_name = NULL,
#                                                     questionnaire = questionnaire, confidence_level = 0.90)
# list_of_results %>% saveRDS("output/list_of_results_jmcna.RDS")
list_of_results <- readRDS("output/list_of_results_jmcna.RDS")


big_table <- list_of_results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#adding which method of calculation was used
big_table <- big_table %>% 
  mutate(methode = case_when(is.na(repeat.var.value) ~ "independent.var", 
                             is.na(independent.var.value) | independent.var.value == "NA" ~ "repeat.var"),
         independent_repeat.value = paste(independent.var.value, repeat.var.value), 
         independent_repeat.value = gsub("NA", "", independent_repeat.value),
         independent_repeat.value = str_trim(independent_repeat.value)) %>% 
  select(dependent.var, dependent.var.value, independent_repeat.value, numbers, se, min, max, methode ) %>%
  mutate(dependent.var.value = ifelse(is.na(dependent.var.value), "NA", as.character(dependent.var.value)))

#split into 2 DF by method so I can bind them and compare results
split_bigtable <- big_table %>%
  split.data.frame(big_table$methode)

#test 2.1.1.a
wide_table <- split_bigtable[[1]] %>%
  left_join(select(split_bigtable[[2]],dependent.var, dependent.var.value, independent_repeat.value, numbers),
            by = c("dependent.var", "dependent.var.value", "independent_repeat.value")) %>% 
  select(-c("methode", "se", "min", "max")) %>%
  rename(independent_methode = numbers.x, 
         repeat_methode = numbers.y) %>%
  mutate(independent_methode = round(independent_methode, digits = 4),
         repeat_methode =round(repeat_methode, digits = 4),
    verif = independent_methode == repeat_methode) 

wide_table
# wide_table %>%
#   filter(verif == F) %>% nrow()
#	the results are same

#test 2.1.1.b
#select_one no SL
verif_breadwinner <- wide_table %>%
  filter(dependent.var == "breadwinner") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(state, breadwinner) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_breadwinner, by = c("breadwinner" = "dependent.var.value", 
                                      "state" = "independent_repeat.value")) %>%
  rename(hype_number = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         hype_number = round(hype_number,digits = 4), 
         verif = prop == hype_number) 
#equal

# select_multiple no SL
verif_enough_water.drinking <- wide_table %>%
  filter(dependent.var == "enough_water", 
         dependent.var.value == "drinking") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(state, enough_water.drinking) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(enough_water.drinking == 1) %>%
  left_join(verif_enough_water.drinking, 
            by = c("state" = "independent_repeat.value")) %>%
  rename(hype_number = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         hype_number = round(hype_number,digits = 4), 
         verif = prop == hype_number)
#equal

# select_one SL  
verif_child_thin <- wide_table %>%
  filter(dependent.var == "child_thin") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(state, child_thin) %>%
  filter(child_thin != "") %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_child_thin, by = c("child_thin" = "dependent.var.value", 
                                      "state" = "independent_repeat.value")) %>%
  rename(hype_number = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         hype_number = round(hype_number,digits = 4), 
         verif = prop == hype_number)
#equal

# select_multiple SL 
verif_latrine_features.door <- wide_table %>%
  filter(dependent.var == "latrine_features", 
         dependent.var.value == "door") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  group_by(state, latrine_features.door) %>%
  filter(!is.na(latrine_features.door)) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(latrine_features.door == 1) %>%
  left_join(verif_latrine_features.door, 
            by = c("state" = "independent_repeat.value")) %>%
  rename(hype_number = independent_methode) %>%
  mutate(prop = round(prop, digits = 4),
         hype_number = round(hype_number,digits = 4), 
         verif = prop == hype_number, 
         diff_verif = abs(prop - hype_number))
#not equal

# integer no SL 
verif_females_18_40 <- wide_table %>%
  filter(dependent.var == "females_18_40") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode)

data_jmcna %>%
  select(state, females_18_40, weightss) %>%
  group_by(state) %>%
  mutate(females_18_40_w = females_18_40 * weightss) %>%
  summarise(ww = sum(females_18_40_w),
            sum_w = sum(weightss)) %>%
  mutate(mean = ww / sum_w)  %>%
  left_join(verif_females_18_40, 
            by = c("state" = "independent_repeat.value")) %>%
  rename(hype_number = independent_methode) %>%
  mutate(mean = round(mean, digits = 4),
         hype_number = round(hype_number,digits = 4), 
         verif = mean == hype_number)
#equal

# integer SL enrolled_boys_6_12 
verif_enrolled_boys_6_12 <- wide_table %>%
  filter(dependent.var == "enrolled_boys_6_12") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode) 

data_jmcna %>%
  select(state, enrolled_boys_6_12, weightss) %>%
  filter(!is.na(enrolled_boys_6_12)) %>%
  group_by(state) %>%
  mutate(enrolled_boys_6_12_w = enrolled_boys_6_12 * weightss) %>%
  summarise(ww = sum(enrolled_boys_6_12_w),
            sum_w = sum(weightss)) %>%
  mutate(mean = ww / sum_w) %>%
  left_join(verif_enrolled_boys_6_12, 
            by = c("state" = "independent_repeat.value")) %>%
  rename(hype_number = independent_methode) %>%
  mutate(mean = round(mean, digits = 4),
         hype_number = round(hype_number,digits = 4), 
         verif = mean == hype_number)
#equal

#test 2.1.2
#for 2 level desegregation i.e. per district and per displacement population
dap2 <- read.csv("input2/dap_jmcna_2dessagregation.csv", header = T, stringsAsFactors = F)

# list_of_results2 <-  from_analysisplan_map_to_output(data_jmcna, 
#                                                     analysisplan = dap2,
#                                                     weighting = weight.function,
#                                                     cluster_variable_name = NULL,
#                                                     questionnaire = questionnaire, confidence_level = 0.90)
# 
# list_of_results2 %>% saveRDS("output/list_of_results_jmcna2.RDS")
list_of_results2 <- readRDS("output/list_of_results_jmcna2.RDS")

big_table2 <- list_of_results2$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

big_table2 <- big_table2 %>%
  mutate(numbers = round(numbers, digits = 4))

#select_one no SL
verif_breadwinner2 <- big_table2 %>%
  filter(dependent.var == "breadwinner") %>%
  select(dependent.var.value, independent.var.value ,repeat.var.value,  numbers )

data_jmcna %>%
  group_by(state, idp_settlement, breadwinner) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state, idp_settlement) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_breadwinner2, 
            by = c("state" = "repeat.var.value", 
                   "breadwinner" = "dependent.var.value", 
                   "idp_settlement" = "independent.var.value")) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers) 
#equal

# %>%
#   filter(verif == F) %>% nrow()

# select_multiple no SL
verif_enough_water.drinking2 <- big_table2 %>%
  filter(dependent.var == "enough_water", 
         dependent.var.value == "drinking") %>%
  select(dependent.var.value, independent.var.value ,repeat.var.value,  numbers )

data_jmcna %>%
  group_by(state, idp_settlement, enough_water.drinking) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state, idp_settlement) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(enough_water.drinking == 1) %>%
  left_join(verif_enough_water.drinking2, 
            by = c("state" = "repeat.var.value", 
                   "idp_settlement" = "independent.var.value")) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers) 
#equal

# select_one SL    
verif_child_thin2 <- big_table2 %>%
  filter(dependent.var == "child_thin") %>%
  select(dependent.var.value, independent.var.value ,repeat.var.value,  numbers )

data_jmcna %>%
  group_by(state, idp_settlement, child_thin) %>%
  filter(child_thin != "") %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state, idp_settlement) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_child_thin2, 
            by = c("state" = "repeat.var.value", 
                   "child_thin" = "dependent.var.value", 
                   "idp_settlement" = "independent.var.value")) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers) 
# %>%
#   filter(verif == F) %>% nrow()
#equal


# select_mutiple SL 
verif_latrine_features.door2 <- big_table2 %>%
  filter(dependent.var == "latrine_features", 
         dependent.var.value == "door") %>%
  select(dependent.var.value, independent.var.value ,repeat.var.value,  numbers )

data_jmcna %>%
  group_by(state, idp_settlement, latrine_features.door) %>%
  filter(!is.na(latrine_features.door)) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(state, idp_settlement) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(latrine_features.door == 1) %>%
  left_join(verif_latrine_features.door2, 
            by = c("state" = "repeat.var.value", 
                   "idp_settlement" = "independent.var.value")) %>%
  mutate(prop = round(prop, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = prop == numbers, 
         diff_verif = abs(prop - numbers))
#not equal

# integer no SL 
verif_females_18_402 <- big_table2 %>%
  filter(dependent.var == "females_18_40") %>%
  select(dependent.var.value, independent.var.value ,repeat.var.value,  numbers )

data_jmcna %>%
  select(state, idp_settlement, females_18_40, weightss) %>%
  group_by(state, idp_settlement) %>%
  mutate(females_18_40_w = females_18_40 * weightss) %>%
  summarise(ww = sum(females_18_40_w),
            sum_w = sum(weightss)) %>%
  mutate(mean = ww / sum_w) %>%
  left_join(verif_females_18_402, 
            by = c("state" = "repeat.var.value", 
                   "idp_settlement" = "independent.var.value")) %>%
  mutate(mean = round(mean, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = mean == numbers) 
#equal

# integer SL enrolled_boys_6_12 
verif_enrolled_boys_6_122 <- big_table2 %>%
  filter(dependent.var == "enrolled_boys_6_12") %>%
  select(dependent.var.value, independent.var.value ,repeat.var.value,  numbers )

data_jmcna %>%
  select(state, idp_settlement, enrolled_boys_6_12, weightss) %>%
  filter(!is.na(enrolled_boys_6_12)) %>%
  group_by(state, idp_settlement) %>%
  mutate(enrolled_boys_6_12_w = enrolled_boys_6_12 * weightss) %>%
  summarise(ww = sum(enrolled_boys_6_12_w),
            sum_w = sum(weightss)) %>%
  mutate(mean = ww / sum_w) %>%
  left_join(verif_enrolled_boys_6_122, 
            by = c("state" = "repeat.var.value", 
                   "idp_settlement" = "independent.var.value")) %>%
  mutate(mean = round(mean, digits = 4),
         numbers = round(numbers,digits = 4), 
         verif = mean == numbers) 
#equal

#test 2.1.2 (swapping idp_settlement and states between repeat and independent)
#dap for 2 level desagregation inversing between repeat and independent
dap2b <- read.csv("input2/dap_jmcna_2dessagregation2.csv", header = T, stringsAsFactors = F)

# list_of_results2b <-  from_analysisplan_map_to_output(data_jmcna,
#                                                     analysisplan = dap2b,
#                                                     weighting = weight.function,
#                                                     cluster_variable_name = NULL,
#                                                     questionnaire = questionnaire, confidence_level = 0.90)
# 
# list_of_results2b %>% saveRDS("output/list_of_results_jmcna2b.RDS")
list_of_results2b <- readRDS("output/list_of_results_jmcna2b.RDS")

big_table2b <- list_of_results2b$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

big_table2b <- big_table2b %>%
  mutate(numbers = round(numbers, digits = 4))

wide_table2 <- big_table2 %>%
  left_join(big_table2b,
            by = c("dependent.var" = "dependent.var", 
                   "dependent.var.value" = "dependent.var.value", 
                   "independent.var" = "repeat.var",
                   "independent.var.value" = "repeat.var.value", 
                   "repeat.var" = "independent.var",
                   "repeat.var.value" = "independent.var.value")) %>% 
  select(-c("se.x", "min.x", "max.x", "se.y", "min.y", "max.y", )) %>%
  # rename(independent_methode = numbers.x, 
  #        repeat_methode = numbers.y) %>%
  mutate(numbers.x = round(numbers.x, digits = 4),
         numbers.y =round(numbers.y, digits = 4),
         verif = numbers.x == numbers.y) 
wide_table2 
wide_table2 %>%
  filter(verif == F) %>% 
  nrow()
#equal

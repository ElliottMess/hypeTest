library(hypegrammaR)
library(tidyverse)

kobochoices <- read.csv("input/kobo_IntentionsChoices.csv", header = T, stringsAsFactors = F)

koboquestions <- read.csv("input/kobo_IntentionsSurvey.csv", header = T, stringsAsFactors = F)

### Import main dataset ###
data_intentions <- read.csv("input/intentions_data.csv", header = T, stringsAsFactors = F)

dap <- read.csv("input/dap_intentions_AoO2.csv", header = T, stringsAsFactors = F)

samplingframe <- load_samplingframe("input/sf_camps_nat_intentions.csv")
samplingframe$stratum_id <- paste(samplingframe$camp_name_label, samplingframe$governorate, sep = "_")
data_intentions$stratum_id <- paste(data_intentions$camp_name, data_intentions$governorate, sep = "_")


weight.function <- map_to_weighting(sampling.frame = samplingframe, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "population_list",
                                    sampling.frame.stratum.column = "stratum_id",
                                    data = data_intentions)
data_intentions$weightss <- weight.function(data_intentions)

questionnaire<-load_questionnaire(data = data_intentions,
                                  questions = koboquestions,
                                  choices = kobochoices,
                                  choices.label.column.to.use = "label::English")

# list_of_results <-  from_analysisplan_map_to_output(data_intentions,
#                                                     analysisplan = dap,
#                                                     weighting = weight.function,
#                                                     cluster_variable_name = NULL,
#                                                     questionnaire = questionnaire, confidence_level = 0.90)
# 
# list_of_results %>% saveRDS("output/list_of_results.RDS")
list_of_results <- readRDS("output/list_of_results.RDS")

big_table <- list_of_results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)

#adding which method of calculation was used
big_table <- big_table %>% 
  mutate(methode = case_when(is.na(repeat.var.value) ~ "independent.var", 
                             is.na(independent.var.value) ~ "repeat.var"), 
         independent_repeat.value = paste(independent.var.value, repeat.var.value), 
         independent_repeat.value = gsub("NA", "", independent_repeat.value),
         independent_repeat.value = str_trim(independent_repeat.value)) %>% 
  select(-c("repeat.var", "repeat.var.value", "independent.var", "independent.var.value")) %>%
  select(dependent.var, dependent.var.value, independent_repeat.value, numbers, se, min, max, methode )

#split into 2 DF by method so I can bind them and compare results
split_bigtable <- big_table %>%
  split.data.frame(big_table$methode)

#test 1.1
wide_table <- split_bigtable[[1]] %>%
  left_join(select(split_bigtable[[2]],dependent.var, dependent.var.value, independent_repeat.value, numbers),
            by = c("dependent.var", "dependent.var.value", "independent_repeat.value")) %>% 
  select(-c("methode", "se", "min", "max")) %>%
  rename(independent_methode = numbers.x, 
         repeat_methode = numbers.y) %>%
  mutate(independent_methode = round(independent_methode, digits = 4), 
         repeat_methode = round(repeat_methode, digits = 4),
         verif = independent_methode == repeat_methode,
         difference = abs(independent_methode - repeat_methode)) 
wide_table %>% View()
##the results are completely different, when using that dataset, up to 40 points of % difference. 

#test 1.2
# select_one no SL
# I filter the result table on the value of interest
verif_safe_in_aoo <- wide_table %>%
  filter(dependent.var == "safe_in_aoo") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode, repeat_methode) 

#I calculate the weighted proportion and then I bind the previous table to check results.
data_intentions %>%
  group_by(governorate_origin, safe_in_aoo) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(governorate_origin) %>%
  mutate(prop = ww / sum(ww)) %>% 
  left_join(verif_safe_in_aoo, 
            by = c("governorate_origin" = "independent_repeat.value",
                   "safe_in_aoo" = "dependent.var.value")) %>%
  mutate(prop = round(prop, digits = 4), 
         verif_inde = independent_methode == prop, 
         verif_repeat = repeat_methode == prop)

# select_one no SL
# I filter the result table on the value of interest
verif_assistance_provided_in_aoo <- wide_table %>%
  filter(dependent.var == "assistance_provided_in_aoo") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode, repeat_methode) 

#I calculate the weighted proportion and then I bind the previous table to check results.
data_intentions %>%
  group_by(governorate_origin, assistance_provided_in_aoo) %>%
  summarise(ww = sum(weightss)) %>%
  group_by(governorate_origin) %>%
  mutate(prop = ww / sum(ww))  %>%
  left_join(verif_assistance_provided_in_aoo, 
            by = c("governorate_origin" = "independent_repeat.value",
                   "assistance_provided_in_aoo" = "dependent.var.value")) %>%
  mutate(prop = round(prop, digits = 4), 
         verif_inde = independent_methode == prop, 
         verif_repeat = repeat_methode == prop)
#equal to independent_methode 
#not equal to repeat_methode 

#select multiple no SL
verif_needs_return <- wide_table %>%
  filter(dependent.var == "needs_return", 
         dependent.var.value == "healthcare_services") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode, repeat_methode) 

data_intentions %>%
  group_by(governorate_origin, needs_return.healthcare_services) %>% 
  summarise(ww = sum(weightss)) %>% 
  # filter(!is.na(why_not_safe_in_aoo.poor_infrastructure)) %>%
  group_by(governorate_origin) %>%
  mutate(prop = ww / sum(ww)) %>% 
  filter(needs_return.healthcare_services == 1)   %>%
  left_join(verif_needs_return, 
            by = c("governorate_origin" = "independent_repeat.value")) %>%
  mutate(prop = round(prop, digits = 4), 
         verif_inde = independent_methode == prop, 
         verif_repeat = repeat_methode == prop) %>%
  select(-dependent.var.value)
#equal to independent_methode 
#not equal to repeat_methode 

# select_one  SL
verif_when_move_1 <- wide_table %>%
  filter(dependent.var == "when_move_1") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode, repeat_methode) 

data_intentions %>%
  select(governorate_origin, when_move_1, weightss) %>%
  group_by(governorate_origin, when_move_1) %>% 
  summarise(ww = sum(weightss)) %>% 
  filter(when_move_1 != "") %>%
  group_by(governorate_origin) %>%
  mutate(prop = ww / sum(ww)) %>%
  left_join(verif_when_move_1, 
            by = c("governorate_origin" = "independent_repeat.value",
                   "when_move_1" = "dependent.var.value")) %>%
  mutate(prop = round(prop, digits = 4), 
         verif_inde = independent_methode == prop, 
         verif_repeat = repeat_methode == prop)
#equal to independent_methode 
#not equal to repeat_methode 

#select mutiple + SL
verif_why_not_safe_in_aoo.gender_based_violence <- wide_table %>%
  filter(dependent.var == "why_not_safe_in_aoo", 
         dependent.var.value == "gender_based_violence") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode, repeat_methode) 

data_intentions %>%
  group_by(governorate_origin, why_not_safe_in_aoo.gender_based_violence) %>% 
  filter(!is.na(why_not_safe_in_aoo.gender_based_violence)) %>%
  summarise(ww = sum(weightss)) %>% 
  group_by(governorate_origin) %>%
  mutate(prop = ww / sum(ww)) %>%
  filter(why_not_safe_in_aoo.gender_based_violence == 1) %>%
  left_join(verif_why_not_safe_in_aoo.gender_based_violence, 
            by = c("governorate_origin" = "independent_repeat.value")) %>%
  mutate(prop = round(prop, digits = 4), 
         verif_inde = independent_methode == prop, 
         verif_repeat = repeat_methode == prop) %>%
  select(-dependent.var.value)
#not equal to independent_methode 
#not equal to repeat_methode 


#select mutiple + SL
verif_why_not_safe_in_aoo.poor_infrastructure <- wide_table %>%
  filter(dependent.var == "why_not_safe_in_aoo", 
         dependent.var.value == "poor_infrastructure") %>%
  select(dependent.var.value, independent_repeat.value, independent_methode, repeat_methode) 

data_intentions %>%
  group_by(governorate_origin, why_not_safe_in_aoo.poor_infrastructure) %>% 
  summarise(ww = sum(weightss)) %>% 
  filter(!is.na(why_not_safe_in_aoo.poor_infrastructure)) %>%
  group_by(governorate_origin) %>%
  mutate(prop = ww / sum(ww)) %>% 
  filter(why_not_safe_in_aoo.poor_infrastructure == 1)   %>%
  left_join(verif_why_not_safe_in_aoo.gender_based_violence, 
            by = c("governorate_origin" = "independent_repeat.value")) %>%
  mutate(prop = round(prop, digits = 4), 
         verif_inde = independent_methode == prop, 
         verif_repeat = repeat_methode == prop) %>%
  select(-dependent.var.value)
#not equal to independent_methode 
#not equal to repeat_methode 





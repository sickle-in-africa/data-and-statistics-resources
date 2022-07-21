#
#  heart disease chi square test
#  =============================
#
#  we check the association between heart disease (presence/absence), and sex 
#  of a patient using a publicly available dataset. We found strong evidence
#  (p value < 0.001) of an association between sex and the presence or absence
#  of heart disease.
#
#  created by: Jack Morrice
#  created on: 21st July 2022
#
################################################################################

# 0. import libraries

library(tidyverse)

# 1. import data

read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data', 
         header=FALSE) %>% 
  as_tibble() -> heart_disease_data

# 2. inspect relevant columns

heart_disease_data %>% str()

heart_disease_data %>% select(V2)
heart_disease_data %>% select(V2) %>% as.list()
heart_disease_data %>% select(V2) %>% range()

heart_disease_data %>% select(V14)
heart_disease_data %>% select(V14) %>% as.list()
heart_disease_data %>% select(V14) %>% range()

# 3. clean up relevant columns

heart_disease_data %>%
  rename(sex=V2) %>%
  rename(num=V14) %>%
  mutate(sex_mf = as.factor(ifelse(sex==1, 'male', 'female'))) %>%
  mutate(heart_disease = as.factor(ifelse(num==0,'absent','present'))) ->
  heart_disease_data

# 4. summarize and visualize relevant columns

heart_disease_data %>% 
  group_by(sex_mf) %>% 
  summarise(count=n())

heart_disease_data %>% 
  ggplot(aes(x=sex_mf)) + 
    geom_bar()

heart_disease_data %>% 
  group_by(num) %>% 
  summarise(count=n())

heart_disease_data %>%
  ggplot(aes(x=num)) + 
    geom_bar()

heart_disease_data %>% 
  ggplot(aes(x=sex_mf, fill=heart_disease)) + 
    geom_bar()

heart_disease_data %>% 
  group_by(sex_mf, heart_disease) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = heart_disease, values_from = count) ->
  sex_heart_disease_2by2_table

sex_heart_disease_2by2_table %>% 
  ggplot(aes(x=sex_mf, y=absent/(absent+present))) + 
    geom_bar(stat = 'identity') + 
    ylim(0,1)

# 5. perform chi square test

chisq.test(
  x=heart_disease_data$sex, 
  heart_disease_data$heart_disease)

# 6. check, does the p value make sense given the plots you drew?

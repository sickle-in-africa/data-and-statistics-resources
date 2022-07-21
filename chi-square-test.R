library(tidyverse)

heart_disease_data %>% select(V2) %>% as.list()
heart_disease_data %>% select(V2) %>% range()

heart_disease_data %>% select(V14) %>% as.list()
heart_disease_data %>% select(V14) %>% range()

heart_disease_data %>% mutate(sex_mf = as.factor(ifelse(V2==1, 'male', 'female'))) -> heart_disease_data
heart_disease_data %>% mutate(heart_disease = as.factor(ifelse(V14==0,'absence','presence'))) -> heart_disease_data

heart_disease_data %>% group_by(sex_mf) %>% summarise(count=n())

heart_disease_data %>% ggplot(aes(x=sex_mf)) + geom_bar()

heart_disease_data %>% group_by(sex_mf, heart_disease) %>% summarise(count=n()) %>% pivot_wider(names_from = heart_disease, values_from = count)

heart_disease_data %>% group_by(sex_mf, heart_disease) %>% summarise(count=n()) %>% pivot_wider(names_from = heart_disease, values_from = count) %>% ggplot(aes(x=sex_mf, y=(presence/(presence+absence)))) + geom_bar(stat='identity') + ylim(0,1)

chisq.test(heart_disease_data$sex_mf, heart_disease_data$heart_disease)

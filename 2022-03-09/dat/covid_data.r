# packages
pacman::p_load(rio, tidyverse)

# import
dd <- import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2022/2022-03-09/Covid_data.csv")

# data w/ 1 record per person
dd_1 <- dd %>% 
   distinct(id, discontinue, age, sex, arm) %>% 
   mutate(arm = factor(arm, label = c('P','A')),
          sex = factor(sex, label = c('F','M'))) 

# data long w/ recoded OSCI
# WHO Ordinal Scale for Clinical Improvement
# https://loinc.org/LL5951-0/
dd_l <- dd %>% 
   select(id, day, OSCI_n = OSCI) %>% 
   mutate(OSCI_f8 = OSCI_n %>% 
             as_factor() %>% 
             fct_recode(
             `Death` =  "8",
             `Hospitalized, ventilation and additional organ support` = "7",
             `Hospitalized, intubation and mechanical ventilation` = "6",
             `Hospitalized, non-invasive ventilation or high-flow oxygen` = "5",
             `Hospitalized, oxygen by mask or nasal prongs` = "4", 
             `Hospitalized, no oxygen therapy` = "3",
             `Limitation of activities` = "2",
             `No limitation of activities` = "1",
             `No clinical or virological evidence of infection` = "0"),
          OSCI_f5 = OSCI_n %>% 
             as_factor() %>% 
             fct_recode(
                `Death`               = "8",
                `Hospitalized Severe` = "7",
                `Hospitalized Severe` = "6",
                `Hospitalized Severe` = "5",
                `Hospitalized Mild`   = "4", 
                `Hospitalized Mild`   = "3",
                `Ambulatory`          = "2",
                `Ambulatory`          = "1",
                `Uninfected`          = "0")
          )

# data wide w/ 1 record per person
dd_w <- dd_l %>% 
   select(id, day, OSCI_n) %>% 
   pivot_wider(id_cols = id,
               names_from = day,
               names_prefix = "d",
               values_from = OSCI_n) %>% 
   right_join(dd_1) %>% 
   relocate(age, discontinue, arm, sex, .after = id)

# Export
dd_1 %>% 
   export("./dat/covid_data_1.csv")

dd_l %>% 
   export("./dat/covid_data_l.csv")

dd_w %>% 
   export("./dat/covid_data_w.csv")

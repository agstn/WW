# packages
pacman::p_load(tidyverse, rio)
pacman::p_load(lubridate)
pacman::p_load(labelled)

# Import All
d_all <- import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2021/2021-03-10/AppData_FINAL.csv") %>% 
   mutate(across(starts_with("date"), ~dmy(.x))) %>% 
   arrange(AppID, date)

# Export
export(d_all, "C:/R/Wonderful-Wednesdays/2021-03-10/d_all.csv")

# Labels
var_label(d_all) <- list(AppID = 'Participant Identifier for App Data',
                         activated = 'User Activated?',
                         date = 'Date of Activity in the App',
                         time = 'Time of Activity in the App',
                         date1 = 'Baseline Visit ',
                         date2 = 'Month 1 Visit' ,
                         date3 = 'Month 2 Visit' ,
                         date4 = 'Month 3 Visit' ,
                         sitekey = 'Site',
                         severity = 'COPD Severity',
                         sex = 'Gender of participant',
                         age = 'Age',
                         durationofcopd = 'No. Years since COPD Diagnosis',
                         smokstatus = 'Smoking Status at Baseline',
                         generalV = 'COPD Education Video',
                         prV = 'COPD Pulmonary Rehab Video',
                         inhalerV = 'How to use Inhaler Video',
                         mindV = 'Mindfulness activity video',
                         smokeV = 'Smoking Cessation video' ,
                         symptomscore = 'Symptom Score Rating ',
                         catscore = 'COPD Assessment Test (CAT) Score',
                         catRisk = 'CAT Risk Group')

# 1 rec per participant variables
d_per <- d_all %>% 
   select(AppID, activated, date1:date4, sitekey, severity, sex, age, durationofcopd, smokstatus) %>% 
   distinct() %>% 
   rowwise() %>% 
   mutate(nvisit = 4-sum(is.na(date1) + is.na(date2) + is.na(date3) + is.na(date4)))

# 1 rec per participant per day in app usage (idea, amount of time the app was used)
d_vis <- d_all %>% 
   select(AppID, date, generalV:smokeV, symptomscore) %>% 
   distinct() %>% 
   mutate(across(c(generalV, prV, inhalerV, mindV, smokeV), ~ifelse(is.na(.x), 0, .x))) %>% 
   group_by(AppID, date) %>% 
   summarise(across(c(generalV:smokeV), ~max(.x, na.rm = TRUE)))

# 1 rec per participant summing all daily activities
d_vis_activity <- d_vis %>% 
   select(-date) %>% 
   group_by(AppID) %>% 
   summarise(across(c(generalV:smokeV), ~sum(.x, na.rm = TRUE)))

# 1 rec per participant per day in CAT
d_cat <- d_all %>% 
   select(AppID, date, catscore) %>%
   group_by(AppID, date) %>% 
   summarise(catscore = mean(catscore, na.rm = TRUE)) %>% 
   drop_na(catscore)

# 1 rec per participant per day in Symptoms
d_symp <- d_all %>% 
   select(AppID, date, symptomscore) %>% 
   group_by(AppID, date) %>% 
   summarise(symptomscore = mean(symptomscore, na.rm = TRUE)) %>% 
   drop_na(symptomscore)

# All Days
d_day <- d_per %>% 
   select(AppID, date1, date4) %>% 
   mutate(date4 = ifelse(is.na(date4), date1+90, date4),
          date4 = as_date(date4)) %>% 
   pivot_longer(cols = -1,
                values_to = 'date') %>% 
   group_by(AppID) %>% 
   complete(date = seq(from = min(date), to = max(date), by = 1)) %>% 
   mutate(day = 1:n()) %>% 
   ungroup() %>% 
   left_join(d_cat) %>% 
   left_join(d_symp)

# Create Visit (months) data
d_group <- tribble(~day, ~group,
        1,      'B',
        2:31,   'M1',
        32:62,  'M2',
        63:100, 'M3') %>% 
   unnest()

# Get Means by Visit (month)
d_group <- d_day %>% 
   left_join(d_group) %>% 
   group_by(AppID, group) %>% 
   summarise(across(c(catscore, symptomscore), ~mean(.x, na.rm = TRUE))) %>% 
   mutate(across(c(catscore, symptomscore), ~ifelse(is.nan(.x), NA, .x)))


# CHECK Imputed
d_group_i <- d_group %>% 
   group_by(AppID) %>%
   rename(cat  = catscore,
          symp = symptomscore) %>% 
   fill(cat, .direction = 'downup') %>% 
   mutate(cat_ch  = cat - cat[1],
          symp_ch = symp - symp[1]) %>% 
   filter(group != "Baseline") 

d_group_i <- d_group_i %>% 
   pivot_wider(id_cols = AppID,
               names_from = group,
               values_from = c(cat_ch, symp_ch))
   
# Merge and export
d_1rec <- d_per %>% 
   left_join(d_vis_activity) %>% 
   left_join(d_group_i) %>%
   ungroup() %>% 
   arrange(match(AppID, c(18,16,6,3,10,4,20,5,19,13,2,11,8,14,15,1,12,7,9,17,21,35,36,37,45,46,47,50,57))) %>% 
   mutate(severity = str_to_title(severity) %>% as.factor(),
          sitekey = as.factor(sitekey),
          sex = str_sub(sex,1,1) %>% as.factor(),
          smokstatus = str_to_title(smokstatus) %>% as.factor(),
          activated = as.factor(activated),
          age = as.numeric(age),
          activated = factor(activated, labels = c("N","Y")),
          AppID  = str_glue("ID-{AppID}")
          ) %>%
   select(-starts_with("date"), -nvisit) %>% 
   rename(site = sitekey,
          sev  = severity,
          dx_copd = durationofcopd,
          smoke = smokstatus) %>%
   rename(cat_B = cat_ch_B,
          cat_1 = cat_ch_M1,
          cat_2 = cat_ch_M2,
          cat_3 = cat_ch_M3,
          symp_B = symp_ch_B,
          symp_1 = symp_ch_M1,
          symp_2 = symp_ch_M2,
          symp_3 = symp_ch_M3) %>% 
   mutate(across(c(dx_copd,cat_B,cat_1,cat_2,cat_3,symp_B,symp_1,symp_2,symp_3), 
                 ~round(.x, digits = 2) %>% 
                    as.character() %>% 
                    as.numeric())) %>% 
   as.data.frame()

# Export
export(d_1rec, "C:/R/Wonderful-Wednesdays/2021-03-10/d_1rec.rds")
export(d_1rec, "C:/R/Wonderful-Wednesdays/2021-03-10/d_1rec.csv")

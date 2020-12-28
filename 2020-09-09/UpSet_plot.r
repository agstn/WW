# packages
pacman::p_load(tidyverse, rio)
pacman::p_load(labelled)
# devtools::install_github("krassowski/complex-upset")
library(ComplexUpset)

# import & recode ID & sort
ae <- import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2020/2020-09-09/2020-09-09-fake-data_aes.csv") %>% 
   mutate(id = str_remove(Subject,"FAKE-DATA-") %>% 
             as.numeric(), 
          .after = Subject) %>% 
   arrange(id)

# 1 Duplicate
ae   %>% filter(id == 10084) %>% filter(`Adverse Event` == "Diarrhea")

# Remove Duplicate
ae <- ae %>% 
   distinct()

# export fixed file
ae %>% 
   export("C:/R/Wonderful-Wednesdays/2020-09-09/2020-09-09-fake-data_aes.csv.rds")

# extend to all days between adverse events
ae_l <- ae %>% 
   select(-Subject) %>% 
   pivot_longer(cols = 2:3,
                values_to = "day") %>% 
   group_by(id, `Adverse Event`, `Treatment`, System, Severity, Sex ) %>% 
   complete(day = seq(min(day), max(day), by = 1)) %>% 
   select(-name) %>% 
   ungroup() %>% 
   arrange(id, day)

# Add variables to wide data
ae_w2 <- ae_l %>% 
   select(id, day, Sex, Treatment, Severity, `Adverse Event`) %>%
   mutate(value = 1) %>%
   pivot_wider(id_cols = c("id","day","Treatment","Sex"),
               names_from = "Adverse Event",
               values_from = "value",
               values_fill	= 0) %>% 
   mutate(Treatment = factor(Treatment, labels = c("A","B")))
# You cannot do the analysis using severity because at each day there can be diverse severerities for
# different AE's overalapping

# Figure 2.1
upset(
   data = ae_w2,
   intersect = names(ae_w2)[-c(1:4)],
   min_size = 20,
   width_ratio = 0.25,
   height_ratio = 1,
   name  = NULL,
   queries=list(
      upset_query(
         intersect=c("Nausea","Vomiting"),
         color=  "#E41A1C",
         fill  =  "#E41A1C",
         
         only_components=c('intersections_matrix', 'Intersection size')
      )
   ),
   annotations = list(
      'Intersection Size\n(Treatment %)' = list(
         aes = aes(x=intersection, fill=Treatment ),
         geom = list(
            geom_bar(stat='count', position='fill'),
            geom_hline(yintercept = 0.5, col='gray90'),
            scale_y_continuous(labels=scales::percent_format()),
            scale_fill_brewer(palette = "Dark2")
         )
      ),
      'Intersection Size\n(Sex %)' = list(
         aes = aes(x=intersection, fill=Sex),
         geom = list(
            geom_bar(stat='count', position='fill'),
            geom_hline(yintercept = 0.5, col='gray90'),
            scale_y_continuous(labels=scales::percent_format()),
            scale_fill_manual(values = c("#E78AC3","#8DA0CB"))
         )
      )
   ),
   themes=upset_modify_themes(
      list('overall_sizes'= theme(axis.ticks.x = element_line()))
   )
) +
   labs(title = 'Co-Occurence of Daily AE Symptoms',
        caption = 'Symptoms with more than 20 days by Frequency: Total pool is 148 individuals for 2575 Daily AE Symptoms \n Lex, Alexander, et al. "UpSet: visualization of intersecting sets." IEEE transactions on visualization and computer graphics 20.12 (2014): 1983-1992.') +
   ggsave("C:/R/Wonderful-Wednesdays/2020-09-09/UpSet_plot.png",
          width = 11, height =10, units = "in")

# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(treemapify)

# import
dd_l <- import('./dat/covid_data_l.csv') %>% 
   mutate(OSCI_n = ifelse(is.na(OSCI_n), 9, OSCI_n)) %>% 
   filter(day == 35) %>% 
   left_join( import('./dat/covid_data_1.csv') %>% 
                 select(id, arm))

cols <- c('9' = "gray85", 
          '8' = "#A50026",
          '7' = "#CE2726",
          '6' = "#EA5839",
          '5' = "#F88D51",
          '4' = "#FDBE70",
          '3' = "#FEE597",
          '2' = "#8FC3DD",
          '1' = "#649AC7",
          '0' = "#416AAE")

dd_c <- dd_l %>% 
   group_by(arm, OSCI_n, OSCI_f5) %>% 
   count() %>%
   ungroup() %>% 
   mutate(OSCI_f8 = factor(OSCI_n,
                           labels  = c("No clinical or virological evidence of infection",    
                                       "No limitation of activities",                          
                                       "Limitation of activities",                             
                                       "No oxygen therapy",                             
                                       "Oxygen by mask or nasal prongs",                
                                       "Non-invasive ventilation or high-flow oxygen",
                                       "Intubation and mechanical ventilation",       
                                       "Ventilation and additional organ support",    
                                       "",
                                       "Lost to follow-up"))
   ) %>% 
   mutate(OSCI_f5 = factor(OSCI_f5) %>% 
             fct_reorder(OSCI_n)
          ) %>% 
   mutate(arm = factor(arm, label = c('Active','Placebo')) %>% fct_rev()) 


f_tree <- ggplot(data = dd_c,
       aes(area = n, label = OSCI_f8, fill = factor(OSCI_n), subgroup = OSCI_f5) ) +
   geom_treemap(layout = 'fixed', start = 'topleft', 
                show.legend = FALSE, size = 5, color = '#ffffff') + 
   geom_treemap_text(layout = 'fixed', start = 'topleft', 
                     grow = FALSE, reflow = TRUE) +
   geom_treemap_subgroup_border(layout = 'fixed', start = 'topleft') +
   geom_treemap_subgroup_text(layout = 'fixed', start = 'topleft',
                              place = 'center', 
                              grow = FALSE, reflow = TRUE,
                              alpha = 0.30, colour ="black", fontface = "italic") +
   facet_wrap( ~ arm, ncol = 1) +
   scale_fill_manual(values = cols) +
   theme_light() +
   theme(panel.grid   = element_blank(),
         strip.text   = element_text(size = 12, face = 'bold'),
         panel.spacing = unit(0, "lines"),
         legend.position="none")

ggsave('./fig/covid_tree.pdf', plot = f_tree, width = 4, height = 8)


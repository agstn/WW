# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(RColorBrewer, ggtext)
pacman::p_load(ggmosaic)
pacman::p_load(TraMineR)

# import
dd_w <- import('./dat/covid_data_w.csv') %>% 
   mutate(across(c(d0:d35), ~ifelse(is.na(.x), 9, .x)))

# create Modal data
seq.p <- seqdef(dd_w %>% filter(arm == 'P') %>% select(d0:d35))
seq.a <- seqdef(dd_w %>% filter(arm == 'A') %>% select(d0:d35))

p <- seqmeant(seq.p)
a <- seqmeant(seq.a)

dd_p <- data.frame(arm = 'P',
                   mean   = data.frame(p)  ) %>% 
   rownames_to_column(var = 'OSCI_n') 

dd_a <- data.frame(arm = 'A',
                   mean   = data.frame(a)  ) %>% 
   rownames_to_column(var = 'OSCI_n')

dd_m <- bind_rows(dd_p,
                  dd_a) %>% 
   mutate(arm = factor(arm, label = c('Active','Placebo')) %>% fct_rev() ) 

rm(seq.p, seq.a, p, a, dd_p, dd_a)

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

f_m <- ggplot(data = dd_m)+
   geom_mosaic(aes(x = product(OSCI_n), fill=OSCI_n, weight = Mean), 
               divider = "vspine", offset = 0) +
   facet_wrap(~arm) +
   scale_fill_manual(values = cols)  +
   scale_x_productlist(name = '',
                       labels = NULL,
                       breaks = NULL,
                       expand = c(0.005,0.005)) +
   scale_y_productlist(name = '',
                       expand = c(0.00,0.00),
                       labels = c('<span style = "color:#bfbfbf;">**Lost to Follow-Up [NA]**</span>
                               <br>',
                               
                               '<span style = "color:#A50026;">**Death [8]**</span>
                               <br>',
                               
                               '<span style = "color:#CE2726;">**Hospitalized Severe [7]**</span>
                                <br><span style="font-size:10pt">Ventilation and additional organ support',
                               
                               '<span style = "color:#EA5839;">**Hospitalized Severe [6]**</span>
                                <br><span style="font-size:10pt">Intubation and mechanical ventilation',
                               
                               '<span style = "color:#F88D51;">**Hospitalized Severe [5]**</span>
                                <br><span style="font-size:10pt">Non-invasive ventilation or high-flow oxygen',
                               
                               '<span style = "color:#FDBE70;">**Hospitalized Mild [4]**</span>
                                <br><span style="font-size:10pt">Oxygen by mask or nasal prongs', 
                               
                               '<span style = "color:#FEE597;">**Hospitalized Mild [3]**</span>
                                <br><span style="font-size:10pt">No oxygen therapy',
                               
                               '<span style = "color:#8FC3DD;">**Ambulatory [2]**</span>
                                <br><span style="font-size:10pt">Limitation of activities',
                               
                               '<span style = "color:#649AC7;">**Ambulatory [1]**</span>
                                <br><span style="font-size:10pt">No limitation of activities',
                               
                               '<span style = "color:#416AAE">**Uninfected [0]**</span>
                                <br><span style="font-size:10pt">No clinical or virological evidence of infection
                               </span>') %>% 
                          rev() ) +
   theme_light() +
   theme(axis.text.y = element_markdown(size = 13),
         axis.text.y.right  = element_markdown(),
         axis.ticks.length.y = unit(0,"lines"),
         panel.grid   = element_blank(),
         strip.text   = element_text(size = 12, face = 'bold'),
         panel.spacing = unit(0, "lines"),
         legend.position="none")

ggsave('./fig/covid_mosaic.pdf', plot = f_m, width = 5, height = 7)
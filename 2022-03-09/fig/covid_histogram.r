# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(RColorBrewer, ggtext)

# import
dd_l <- import("./dat/covid_data_l.csv")
dd_1 <- import("./dat/covid_data_1.csv")

dd_l <- left_join(dd_1,
                  dd_l %>% select(id, day, OSCI_n)
                  ) %>%
   mutate(OSCI_n = factor(OSCI_n) %>%
             fct_explicit_na(na_level = '9') %>%
             fct_rev(),
          arm = factor(arm, label = c('Active','Placebo')) %>% fct_rev() )

# plot
f_h <- ggplot(data = dd_l, 
       aes(day, fill = OSCI_n )) +
   geom_histogram(binwidth = 1, position = 'fill') +
   scale_fill_manual(values = c('9' = "gray85", 
                                '8' = "#A50026",
                                '7' = "#CE2726",
                                '6' = "#EA5839",
                                '5' = "#F88D51",
                                '4' = "#FDBE70",
                                '3' = "#FEE597",
                                '2' = "#8FC3DD",
                                '1' = "#649AC7",
                                '0' = "#416AAE")) +
   scale_x_continuous(name = 'Day',
                      breaks = seq(0, 35, 5),
                      expand = c(0, 0)) +
   scale_y_continuous(name = '**B. Frequency Plot**',
                      breaks = seq(0, 1, 0.25),
                      labels = c('0','.25','.50','.75','1'),
                      expand = c(0, 0) ) +
   facet_wrap(~arm) +
   theme_light() +
   theme(panel.grid   = element_blank(),
         strip.text   = element_text(size = 12, face = 'bold'),
         legend.position="none",
         axis.title.y = element_markdown(size = 12))

ggsave('./fig/covid_histogram.pdf', plot = f_h, width = 8, height = 4)

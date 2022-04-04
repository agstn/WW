# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(ggtext)
pacman::p_load(RColorBrewer)

# import
dd_l <- import('./dat/covid_data_l.csv')
dd_1 <- import('./dat/covid_data_1.csv')

dd_l <- left_join(dd_1,
                  dd_l %>% select(id, day, OSCI_n)) %>%
   mutate(OSCI_n = replace_na(OSCI_n, 9) ,
          arm = factor(arm, label = c('Active','Placebo')) %>% fct_rev() ) %>%
   arrange(desc(arm), id)

dd_w <- dd_l %>%
   select(id, arm, day, OSCI_n) %>%
   pivot_wider(values_from = OSCI_n,
               names_from  = day,
               names_prefix  = 'd')

dd_w_o <- data.frame(id    = dd_w$id,
                     arm   = dd_w$arm) %>%
   left_join(dd_w) %>% 
   arrange(arm, 
           d35, d34, d33, d32, d31, d30, d29, d28, d27, d26, 
           d25, d24, d23, d22, d21, d20, d19, d18, d17, d16, 
           d15, d14, d13, d12, d11, d10, d9, d8, d7, d6, 
           d5, d4, d3, d2) %>% 
   mutate(id_order = 1:n())

dd_l_o <- dd_w_o %>% 
   pivot_longer(cols = starts_with("d"),
                names_to = "day",
                values_to = "OSCI_n",
                names_prefix = "d") %>% 
   mutate(day    = as.integer(day),
          OSCI_n = as.factor(OSCI_n) %>% fct_rev())

f_t <- ggplot(data = dd_l_o, 
       aes(x = day, y = id_order, fill = OSCI_n )) +
   geom_tile() +
   facet_wrap(~arm, scales = "free") +
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
   scale_y_discrete(name = '**A. Full Index Plot**', 
                    labels = NULL, 
                    breaks = NULL) +
   scale_x_continuous(name = 'Day',
                      breaks = seq(0, 35, 5),
                      expand = c(0, 0)) +
   theme_light() +
   theme(panel.grid   = element_blank(),
         strip.text   = element_text(size = 12, face = 'bold'),
         legend.position="none",
         axis.title.y = element_markdown(size = 12))

ggsave('./fig/covid_tile_order.pdf', plot = f_t, width = 8, height = 5)

                    
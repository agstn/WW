# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(RColorBrewer, ggtext)
pacman::p_load(TraMineR)

# import
dd_w <- import('./dat/covid_data_w.csv')

# create Modal data
seq.p <- seqdef(dd_w %>% filter(arm == 'P') %>% select(d0:d35))
seq.a <- seqdef(dd_w %>% filter(arm == 'A') %>% select(d0:d35))

p <- seqmodst(seq.p)
a <- seqmodst(seq.a)

dd_p <- data.frame(arm = 'P',
                   freq   = attr(p, "Frequencies") %>% as.vector(),
                   OSCI_n = data.frame(p) %>% as.vector() %>% t() ) %>% 
   rownames_to_column(var = 'day') %>% 
   mutate(day = parse_number(day)) 

dd_a <- data.frame(arm = 'A',
                   freq   = attr(a, "Frequencies") %>% as.vector(),
                   OSCI_n = data.frame(a) %>% as.vector() %>% t() ) %>% 
   rownames_to_column(var = 'day') %>% 
   mutate(day = parse_number(day)) 

dd_m <- bind_rows(dd_p,
                  dd_a) %>% 
   mutate(arm = factor(arm, label = c('Active','Placebo')) %>% fct_rev() )

rm(seq.p, seq.a, p, a, dd_p, dd_a)

f_mo <- ggplot(data = dd_m,
       aes(x = day, y = freq, fill = OSCI_n)) +
   geom_col() +
   geom_hline(yintercept = c(0.25, 0.50, 0.75), color = 'white', size = 0.25) +
   facet_wrap(~arm) +
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
   scale_y_continuous(name = '**C. Modal Plot**',
                      breaks = seq(0, 1, 0.25),
                      labels = c('0','.25','.50','.75','1'),
                      expand = c(0, 0) ) +
   theme_light() +
   theme(panel.grid   = element_blank(),
         strip.text   = element_text(size = 12, face = 'bold'),
         legend.position="none",
         axis.title.y = element_markdown(size = 12))

ggsave('./fig/covid_modal.pdf', plot = f_mo, width = 8, height = 3)

# PACKAGES
pacman::p_load(tidyverse, rio)
pacman::p_load(dabestr)
pacman::p_load(ggdist, patchwork)
pacman::p_load(ggpp, ggtext)

# IMPORT
d1 <- import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2021/2021-10-13/WWW_example_minimal_clinical_improvement.csv") %>% 
   mutate(id = row_number(), .before = everything()) %>% 
   janitor::clean_names()

# EXPORT
d1 %>% export("C:/R/Wonderful-Wednesdays/2021-10-13/ww_mci.csv")

# RESHAPE & CALC
d2 <- d1 %>% 
   pivot_longer(cols = c(total_score_baseline, total_score_follow_up)) %>% 
   mutate(time = ifelse(name == "total_score_baseline","B","F"),
          cgi_f = factor(cgi_i, labels = c('Very much improved [1]',
                                           'Much improved [2]',
                                           'Minimally improved [3]',
                                           'No change [4]',
                                           'Minimally worse [5]',
                                           'Much worse [6]',
                                           'Very much worse [7]'))) %>% 
   select(id, cgi_i, cgi_f, time, value) %>% 
   nest_by(cgi_i, cgi_f) %>% 
   mutate(dabest   = list(dabest(data, 
                                 time, value,
                                 idx = c("B","F"),
                                 paired = TRUE, id.col = id) )) %>% 
   mutate(mean_diff = list( mean_diff(dabest) ) ) 

# EXTRACT
d3 <- d2 %>% 
   select(cgi_i, cgi_f, mean_diff) %>% 
   mutate(change = mean_diff$data %>% list(),
          boot   = mean_diff$result$bootstraps) %>% 
   select(-mean_diff) 

# N's and nested graphs
n <- d2 %>% 
   mutate(n = nrow(data)/2) %>%
   select(cgi_i, cgi_f, n) %>%
   ungroup() %>%
   mutate(n_sum = cumsum(n))

gg_p <- function(i){
   
   cols         <- as.vector(rep('gray90',7))
   cols[i]      <- 'gray25'
   
   ggplot(data = n,
          aes(x = n, y = '1', fill = fct_rev(cgi_f)   ) ) +
      geom_bar(stat = 'identity') +
      geom_label(data = n %>% slice(i),
                 aes(   x = n_sum,
                        y = '1',
                        label = n),
                 nudge_y = 0.40,
                 size = 3, fill = 'white', 
                 label.padding = unit(0.10, "lines"), label.size = 0.0) +
      scale_fill_manual(values = rev(cols)) +
      guides(fill = 'none') +
      theme_void() 
}

n <- n %>%
   rowwise() %>%
   mutate(gg = list( gg_p(cgi_i))) 

# CHANGE
change <- d3 %>% 
   select(cgi_i, cgi_f, change) %>% 
   unnest(c(change))

f1 <- ggplot(data = change,
             aes(x = time, y = value, group = id)) +
   geom_plot_npc(data = n,
                 vp.width = 0.95, vp.height = 0.08,
                 aes(npcx = 0.01, npcy = 0.99,  label = gg)) +
   geom_line(col = 'gray25', alpha = 0.5, size = 0.5) +
   stat_summary(aes(group = 1), 
                fun = mean, colour = 'black', geom='line', size = 1,
                position = position_nudge(x = c(-0.05, 0.05))) +
   stat_halfeye(data = . %>% filter(time == "B"),
                aes(group = 1),
                point_interval = mean_qi,
                side = 'left',
                justification = 1.1,
                adjust = 0.95,
                position = position_nudge(x = -0.1)) +
   stat_halfeye(data = . %>% filter(time == "F"),
                aes(group = 1),
                point_interval = mean_qi,
                side = 'right',
                justification = -0.1,
                adjust = 0.95,
                position = position_nudge(x = 0.1)) +
   scale_y_continuous(name = 'Better <----- Total Score -----> Worst',
                      expand = c(0,0),
                      limits = c(12, 80),
                      sec.axis = sec_axis(~., name = '')) +
   scale_x_discrete(name = '',
                    expand = c(0.05, 0.05),
                    labels = c("Baseline         ", "         Follow-up")) +
   facet_wrap(.~cgi_f, ncol = 7) +
   theme_bw() +
   theme(panel.grid.minor = element_blank(),
         panel.spacing    = unit(0, "lines"),
         plot.margin      = margin(b = 0))

# BOOSTRAP
boot <- d3 %>% 
   select(cgi_i, cgi_f, boot) %>% 
   unnest(c(boot))

m <- boot %>%
   group_by(cgi_f) %>%
   summarise(m = mean(boot) %>% round(1))

f2 <- ggplot(data = boot,
             aes(y = boot, x = 'Ch', fill = stat(abs(y) > 10) ) ) +
   stat_halfeye(point_interval = mean_qi,
                side = 'right',
                justification = -0.05,
                adjust = 0.8,
                position = position_nudge(x = 0.05)) +
   geom_hline(yintercept  = c(-10, 10),
              color = 'gray75') +
   geom_hline(yintercept = 0,
              color = 'gray50') +
   scale_fill_manual(values = c("gray80", "skyblue")) +
   scale_y_continuous(name = 'Paired Mean Difference \n (5K Bootstrap)',
                      limits = c(-20, 20),
                      expand = c(0, 0),
                      sec.axis = sec_axis(~., name = '')) +
   scale_x_discrete(name = '',
                    expand = c(0.15, 0),
                    labels = "F minus B") +
   facet_wrap(.~cgi_f, ncol = 7) +
   geom_text(data = m,
             aes(x = 'Ch', y = m, label = m),
             size = 3, nudge_x = 0.2
   ) +
   theme_bw() +
   theme(panel.grid.minor   = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.spacing      = unit(0, "lines"),
         plot.margin        = margin(t = 0, b = 0), 
         strip.text         = element_blank()) +
   guides(fill = 'none')

f1/f2 +
   plot_layout(heights = c(1.5,1)) +
   plot_annotation(
      title   = 
      "<b>Baseline, Follow-up and Change assessment of the total score by Clinical Assessment of Improvement (CGI-I)</b> 
      <br>
      <span style = 'font-size:10pt'><b>Top panel</b> gray lines represent a participant score, while 
      the slab and point interval (mean, 95% and 99% CI) characterize the marginal observations with 
      top annotations for the total number of pairs. 
      <br>
      <b>Bottom panel</b> paired mean Change derived though nonparametric bootstrap resampling, mean difference 
      <span>&#177;</span> than 10 are <b><span style = 'color:skyblue;'>color-coded</span></b>.
     </span>",
     theme = theme(
        plot.title = element_textbox_simple(
           size = 13,
           lineheight = 1,
           r = unit(5, "pt"),
           padding = margin(4, 4, 4, 4),
           margin  = margin(1, 20, 5, 1),
           fill = "gray90"
        )
     )
   )

ggsave("C:/R/Wonderful-Wednesdays/2021-10-13/ww_mci.png",
       dpi = 600,
       width = 14,
       height = 7)
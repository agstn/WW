pacman::p_load(tidyverse, rio)
pacman::p_load(ggh4x)
pacman::p_load(gganimate)

vas_per <- import("C:/R/Wonderful-Wednesdays/2021-05-12/vas_per.rds")
vas_vis <- import("C:/R/Wonderful-Wednesdays/2021-05-12/vas_vis.rds") %>% 
   filter(ady <= 420) 


mysqrt_trans <- function() {
   scales::trans_new("mysqrt", 
                     transform = base::sqrt,
                     inverse = function(x) ifelse(x<0, 0, x^2),
                     domain = c(0, Inf))
}

axs_df <- crossing(name = c('sym','dose','cumrem','cumrel'),
                   txt = c('a','b','c','d') ) %>% 
   as.data.frame() %>% 
   mutate(name  = as.factor(name) %>% 
             fct_relevel('sym','dose','cumrem','cumrel')) %>% 
   filter(name  == 'sym')

for (s in unique(vas_per$subject)) {
   
   v <- vas_vis %>%
      filter(subject == s) %>%
      select(subject, trt01pc, ady, sym, dose, rem, rel) %>%
      mutate(cumrem = cumsum(rem),
             cumrel = cumsum(rel)) %>% 
      select(-rem, -rel) %>% 
      pivot_longer(cols = c(sym, dose, cumrem, cumrel)) %>% 
      mutate(name = factor(name) %>% 
                fct_relevel('sym','dose','cumrem','cumrel'))
   
   v2 <- v %>% 
      rename(ady_2 = ady)
   
   g1 <- ggplot(data = v,
                aes(x = ady, y = value, group = subject)) +
      
      geom_segment(data = axs_df,
                   x  = 1, xend = 365, y = 33, yend = 33,
                   arrow = arrow(length = unit(0.05,  "native"), type = "closed"),
                   inherit.aes = FALSE) +
      geom_label(data = axs_df,
                 x = 168, label = "On Treatment",
                 y = 33,
                 size = 2,
                 inherit.aes = FALSE) +
      geom_segment(data = axs_df,
                   x  = 366, xend = 425, y = 33, yend = 33,
                   arrow = arrow(length = unit(0.05,  "native"), type = "closed"),
                   inherit.aes = FALSE) +
      geom_label(data = axs_df,
                 x = 391, label = "Off Treatment",
                 y = 33,
                 size = 2,
                 inherit.aes = FALSE) +
      coord_cartesian(clip = "off") +
      
      geom_vline(xintercept = c(168, 365), col = 'gray65') +
      geom_line(data = v2,
                aes(x = ady_2, y = value, group = 1),
                size = 1, 
                alpha = 0.5,
                color = ifelse(v2$trt01pc == 'T', '#1b9e77', '#d95f02')) +
      geom_line(size = 1, color = ifelse(v$trt01pc == 'T', '#1b9e77', '#d95f02')) +
      geom_point(         color = ifelse(v$trt01pc == 'T', '#1b9e77', '#d95f02')) + 
      facet_wrap(~name,
                 strip.position = 'left',
                 ncol = 1,
                 scales = 'free_y',
                 labeller = as_labeller(c(sym    = "Vasculitis symptom score \n(sqrt)", 
                                          dose   = "Oral Corticosteroid dose \n(log+1)", 
                                          cumrem = "Remission Event \n(cumulative)", 
                                          cumrel = "Relapse Event\n(cumulative sqrt)"))
      ) +
      facetted_pos_scales(y = list(scale_y_continuous(limits = c(0, 28),
                                                      trans = 'mysqrt',
                                                      breaks = c( 0,  1,  4,  10,  20),
                                                      labels = c('0','1','4','10','20'),
                                                      minor_breaks = NULL,
                                                      expand = c(0.03, 0.01)),
                                   scale_y_continuous(limits = c(0, 1280),
                                                      breaks = c( 0,  2,  7.5,  25,  75,  250,  750), # seq(0, 35, by = 5)^2,
                                                      labels = c('0','2','7.5','25','75','250','750'),
                                                      expand = c(0.03, 0.01),
                                                      trans = "log1p",
                                                      minor_breaks = NULL),
                                   scale_y_continuous(limits  = c(0,350),
                                                      breaks = c(0, 100, 200, 300),
                                                      expand = c(0.03, 0.01),
                                                      minor_breaks = NULL),
                                   scale_y_continuous(limits  = c(0,11),
                                                      trans = 'mysqrt',
                                                      breaks = c(0, 1, 4, 9),
                                                      expand = c(0.03, 0.01),
                                                      minor_breaks = NULL))
      )  +
      scale_x_continuous(name = "Study Day",
                         limits = c(0, 420),
                         breaks = c(1, 168, 365, 420),
                         labels = c("1", "168 days\n 24 wks", "365 days\n 52 wks", "420 days\n 60 wks"),
                         expand = c(0.01, 0.01),
                         minor_breaks = NULL) + 
      force_panelsizes(rows = c(1,1,0.5,0.5),
                       respect = FALSE) +
      labs(y = NULL) +
      theme_bw(base_size = 10) +
      theme(plot.margin = unit(c(1.5, 0.5, 0.5, 0.5), units="lines"),
            strip.placement = "outside",
            axis.text.x = element_text(hjust = 1)) +
      transition_reveal(ady, range=c(1L, 420L)) 
   
   a_g1 <- animate(g1,
                   nframes = 100,
                   width  = 6.0,
                   height = 7.0,
                   units = 'in',
                   res = 96,
                   end_pause = 25)
   
   anim_save(filename = str_glue("subject_{s}.gif"),
             a_g1,
             path = "C:/R/Wonderful-Wednesdays/2021-05-12/vas-subject-gif/")
}

pacman::p_load(tidyverse, rio)
pacman::p_load(ggh4x, ggtext)
pacman::p_load(patchwork, magick)

for (d in c(seq(1, 420, 2), 420) ) {
   
   vas_glm <- import('C:/R/Wonderful-Wednesdays/2021-05-12/vas_glm.rds') %>% 
      filter(ady <= d)
   
   ref_df <- crossing(name = c('sym_d','dose_d','cumrem_d','cumrel_d'),
                      name2 = c('mean','diff'),
                      time = c('a','b','c'),
                      ref  = 1) %>% 
      as.data.frame() %>% 
      mutate(name  = as.factor(name) %>% fct_relevel('sym_d','dose_d','cumrem_d','cumrel_d'),
             name2 = as.factor(name2) %>% fct_relevel('mean','diff')) %>% 
      filter(name2 != 'mean')
   
   axs_df <- crossing(name = c('sym_d','dose_d','cumrem_d','cumrel_d'),
                      name2 = c('mean','diff'),
                      nesting( ady = c(1, 168, 365, 420),
                               txt = c('a','b','c','d') )) %>% 
      as.data.frame() %>% 
      mutate(name  = as.factor(name) %>% fct_relevel('sym_d','dose_d','cumrem_d','cumrel_d'),
             name2 = as.factor(name2) %>% fct_relevel('mean','diff')) %>% 
      filter(name2 == 'mean') %>% 
      filter(name  == 'sym_d')
   
   vas_l_l <- vas_glm %>% 
      group_by(name, name2, trt01pc) %>% 
      slice(n()) %>% 
      select(ady, name, name2, trt01pc, rate) %>% 
      mutate(color = case_when(trt01pc == 'T'     ~ '#1b9e77',
                               trt01pc == 'P'     ~ '#d95f02',
                               trt01pc == 'T / P' ~ '#7f7f7f'))
   
   vas_l_l_s  <- vas_l_l %>% filter(name == 'sym_d', trt01pc %in% c('T','P')) 
   vas_l_l_s2 <- vas_l_l %>% filter(name == 'sym_d', trt01pc %in% c('T / P')) 
   
   vas_l_l_d  <- vas_l_l %>% filter(name == 'dose_d', trt01pc %in% c('T','P')) 
   vas_l_l_d2 <- vas_l_l %>% filter(name == 'dose_d', trt01pc %in% c('T / P')) 
   
   vas_l_l_c  <- vas_l_l %>% filter(name == 'cumrem_d', trt01pc %in% c('T','P')) 
   vas_l_l_c2 <- vas_l_l %>% filter(name == 'cumrem_d', trt01pc %in% c('T / P')) 
   
   vas_l_l_cl  <- vas_l_l %>% filter(name == 'cumrel_d', trt01pc %in% c('T','P')) 
   vas_l_l_cl2 <- vas_l_l %>% filter(name == 'cumrel_d', trt01pc %in% c('T / P'))  
   
   f1 <- ggplot(data = vas_glm,
                aes(x = ady, y =rate, color = trt01pc, fill = trt01pc)) +
      geom_hline(data = ref_df,
                 aes(yintercept = ref),
                 col = 'gray25') +
      geom_vline(xintercept = d, col = 'gray50') +
      geom_vline(xintercept = c(168, 365), col = 'gray65') +
      geom_line() +
      geom_point(data = vas_glm %>% filter(ady == d)) +
      geom_ribbon(data = . %>% filter(name2 == 'mean'),
                  aes(ymin=rate-SE, ymax=rate+SE),
                  alpha=0.5, colour = NA) +
      geom_ribbon(data = . %>% filter(name2 == 'diff'),
                  aes(ymin=asymp.LCL, ymax=asymp.UCL),
                  alpha=0.5, colour = NA) +
      
      geom_segment(data = axs_df,
                   x  = 1, xend = 365, y = 5.0, yend = 5.0,
                   arrow = arrow(length = unit(0.08,  "native"), type = "closed"),
                   inherit.aes = FALSE) +
      geom_label(data = axs_df,
                 x = 168, label = "On Treatment",
                 y = 5.4,
                 size = 3,
                 inherit.aes = FALSE) +
      geom_segment(data = axs_df,
                   x  = 366, xend = 420, y = 5.0, yend = 5.0,
                   arrow = arrow(length = unit(0.08,  "native"), type = "closed"),
                   inherit.aes = FALSE) +
      geom_label(data = axs_df,
                 x = 390, label = "Off Treatment",
                 y = 5.4,
                 size = 3,
                 inherit.aes = FALSE) +
      coord_cartesian(clip = "off") +
      facet_nested_wrap(vars(name, name2),
                        dir = 'v',
                        strip.position = "left",
                        ncol = 1,
                        scales = 'free_y',
                        labeller = as_labeller(c(sym_d    = "Vasculitis symptom score",
                                                 dose_d   = "Oral Corticosteroid dose",
                                                 cumrem_d = "Remission Event (cumulative)",
                                                 cumrel_d = "Relapse Event (cumulative)",
                                                 mean = 'Means',
                                                 diff = 'Ratio'))
      ) +
      facetted_pos_scales(y =list(
         scale_y_continuous(limits = c(0.5, 4.5),
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_s %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s$color}'>{vas_l_l_s$trt01pc  }</b>"),
                               name = NULL
                            )),
         scale_y_continuous(limits = c(0.25, 4.5),
                            trans = "log10",
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_s2 %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s2$color}'>{vas_l_l_s2$trt01pc  }</b>"),
                               name = NULL
                            )),
         
         scale_y_continuous(limits = c(3.5, 15.2),
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_d %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s$color}'>{vas_l_l_s$trt01pc  }</b>"),
                               name = NULL
                            )),
         scale_y_continuous(limits = c(0.30, 2.0),
                            trans = "log10",
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_d2 %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s2$color}'>{vas_l_l_s2$trt01pc  }</b>"),
                               name = NULL
                            )),
         
         scale_y_continuous(limits = c(0, 205),
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_c %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s$color}'>{vas_l_l_s$trt01pc  }</b>"),
                               name = NULL
                            )),
         scale_y_continuous(limits = c(0.2, 11),
                            trans = "log10",
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_c2 %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s2$color}'>{vas_l_l_s2$trt01pc  }</b>"),
                               name = NULL
                            )),
         
         scale_y_continuous(limits = c(0, 3.0),
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_cl %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s$color}'>{vas_l_l_s$trt01pc  }</b>"),
                               name = NULL
                            )),
         scale_y_continuous(limits = c(0.10, 5.0),
                            trans = "log10",
                            minor_breaks = NULL,
                            sec.axis = dup_axis(
                               breaks = vas_l_l_cl2 %>% pull(rate),
                               labels = str_glue("<b style='color:{vas_l_l_s2$color}'>{vas_l_l_s2$trt01pc  }</b>"),
                               name = NULL
                            )))
      )+
      scale_color_manual(values = c('T' = '#1b9e77',
                                    'P' = '#d95f02',
                                    'T / P' = '#7f7f7f'),
                         guide = FALSE) +
      scale_fill_manual(values = c('T' = '#1b9e77',
                                   'P' = '#d95f02',
                                   'T / P' = '#7f7f7f')) +
      scale_x_continuous(name = str_glue("Study Day: {d}"),
                         limits = c(0, 420),
                         breaks = c(1, 168, 365, 420),
                         labels = c("1", "168 days\n 24 wks", "365 days\n 52 wks", "420 days\n 60 wks"),
                         expand = c(0.01, 0.01),
                         minor_breaks = NULL) + 
      labs(y = NULL) +
      theme_bw(base_size = 10) +
      theme(
         plot.margin = unit(c(1.5, 0.5, 0.5, 0.5), units="lines"),
         legend.position = "none",
         strip.placement = "outside",
         axis.text.x = element_text(hjust = 1),
         axis.text.y.right= element_markdown()
      ) 
   
   
   f2 <- ggplot(data = vas_glm ,
                aes(y = rate, fill = trt01pc)) +
      geom_hline(data = ref_df,
                 aes(yintercept = ref),
                 col = 'gray25') +
      geom_boxplot(varwidth = TRUE, alpha = 0.5, outlier.shape = NA) +
      facet_nested(rows = vars(name,name2),
                   cols = vars(time),
                   scales = 'free_y',
                   labeller = as_labeller(c(sym_d    = "Vasculitis symptom score",
                                            dose_d   = "Oral Corticosteroid dose",
                                            cumrem_d = "Remission Event (cumulative)",
                                            cumrel_d = "Relapse Event (cumulative)",
                                            mean = 'Means',
                                            diff = 'Ratio',
                                            a = '0-24 wks',
                                            b = '24-52 wks',
                                            c = '52-60 wks'))
      ) +
      facetted_pos_scales(y =list(
         scale_y_continuous(limits = c(0.5, 4.5),
                            minor_breaks = NULL),
         scale_y_continuous(limits = c(0.25, 4.5),
                            trans = "log10",
                            minor_breaks = NULL),
         
         scale_y_continuous(limits = c(3.5, 15.2),
                            minor_breaks = NULL),
         scale_y_continuous(limits = c(0.30, 2.0),
                            trans = "log10",
                            minor_breaks = NULL),
         
         scale_y_continuous(limits = c(0, 205),
                            minor_breaks = NULL),
         scale_y_continuous(limits = c(0.2, 11),
                            trans = "log10",
                            minor_breaks = NULL),
         
         scale_y_continuous(limits = c(0, 3.0),
                            minor_breaks = NULL),
         scale_y_continuous(limits = c(0.10, 5.0),
                            trans = "log10",
                            minor_breaks = NULL))
      ) +
      scale_color_manual(values = c('T' = '#1b9e77',
                                    'P' = '#d95f02',
                                    'T / P' = '#7f7f7f'),
                         guide = FALSE) +
      scale_fill_manual(values = c('T' = '#1b9e77',
                                   'P' = '#d95f02',
                                   'T / P' = '#7f7f7f'),
                        labels = c("P",
                                   "T",
                                   "Ratio")) +
      labs(y = NULL,
           fill = NULL) +
      theme_bw(base_size = 10) +
      theme(
         legend.position = "none",
         strip.placement = "outside",
         axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank()
      )
   
   
   f1 + f2 +
      plot_layout(widths = c(4, 1.5))
   
   ggsave(
      str_glue("C:/R/Wonderful-Wednesdays/2021-05-12/vas-fig-gif/fig-gif_{str_pad(d, 3, pad='0')}.png"),
      width = 14,
      height = 8)
   
}

# unlink("C:/R/Wonderful-Wednesdays/2021-05-12/fig-gif/*", recursive = T, force = T)
pacman::p_load(gifski)

lf <- list.files (path = "C:/R/Wonderful-Wednesdays/2021-05-12/vas-fig-gif",
                  pattern = c(".png"),
                  full.names = TRUE)

gifski(png_files = lf,
       gif_file = "C:/R/Wonderful-Wednesdays/2021-05-12/vas-fig-gif.gif",
       width = 4200,
       height = 2400,
       delay = 0.15 )



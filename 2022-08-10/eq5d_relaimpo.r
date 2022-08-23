# packages
pacman::p_load(rio, tidyverse, scales)
pacman::p_load(relaimpo)
pacman::p_load(ggh4x, ggtext)
pacman::p_load(patchwork, cowplot, magick)

# import
dd <- rio::import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2022/2022-08-10/EQ5D.csv") %>% 
   mutate(id = 1:n(), .before = 1) %>% 
   rename_with(tolower) %>% 
   mutate(mar.status = factor(mar.status, labels = c('single','married')),
          gender = factor(gender, labels = c('female','male')),
          bmi = factor(bmi, labels = c('<30', '>=30')),
          edu = factor(edu, labels = c('<=9 yrs', '>9 yrs'))) %>% 
   rename(sex = gender,
          marsta = mar.status,
          ncond = n.cond)

# export
dd %>% export('ed5d_data.rds')

# REG SELECTION
mm <- lm(eq5d ~ (. - id)^2, data = dd) %>% 
   MASS::stepAIC()

# REG MODEL
mm <- lm(eq5d ~ age + sex + marsta + edu + ncond + bmi +
            sex:marsta + 
            age:bmi + 
            age:marsta  +  
            marsta:bmi,
         data = dd) 

# RELAIMP
rr <- lm(eq5d ~ age + sex + marsta + edu + ncond + bmi +
            sex:marsta + 
            age:bmi + 
            age:marsta  +  
            marsta:bmi,
         data = dd) %>% 
   calc.relimp(type = 'lmg')

# DATA DERIVE
lmg <- data.frame(lmg  = rr$lmg,
                  rank = rr$lmg.rank) %>% 
   rownames_to_column(var = 'name') %>% 
   mutate(name = fct_reorder(name, .x = rank),
          inter = ifelse(str_detect(name, ':'), 1, 0)) %>% 
   arrange(inter, rank) %>% 
   mutate(rank2 = 1:n(), .after = rank) %>% 
   mutate(lmg_cum = cumsum(lmg),
          lmg_start = lmg_cum - lmg,
          lmg_end   = lmg_cum) 

tot <- lmg %>% 
   slice_max(lmg_cum) %>% 
   mutate(name = 'Total R2', 
          inter = 2,
          lmg_start = 0,
          lmg = lmg_cum)

lmg <- bind_rows(lmg,
                 tot)

# FIGURE IND
f_ind <- lmg %>% 
   ggplot(
      aes( y    = fct_reorder(name, -rank2),
           yend = fct_reorder(name, -rank2) ) ) + 
   geom_segment(
      aes(#y    = name,
         #yend = name,
         x    = lmg_start,
         xend = lmg_end),
      color = 'gray50',
      size = 15, lineend = 'butt'
   ) +
   geom_segment(
      aes(#y    = name,
         #yend = name,
         x    = lmg_start,
         xend = lmg_end),
      position = position_nudge(y = 0.4),
      arrow = arrow(type = "closed",
                    length = unit(0.10, "inches")
      ),
      color = 'gray50',
      size = 0.5
   ) +
   geom_text(aes(y =  name,
                 x = lmg_cum,
                 label = str_glue("+{label_percent(accuracy = 0.1)(lmg)}") ),
             hjust= -0.1,
             size = 3.5) +
   facet_grid(rows = vars(inter %>%
                             factor(labels = c('Main Effect','Two−way Interactions','Total')) ),
              scales = 'free_y',
              space  = 'free_y') +
   scale_x_continuous(name = 'Relative Importance (%) for EQ-5D (LMG metric)',
                      labels = percent, 
                      expand = expansion(add = c(0.01, 0.035) ) ) +
   scale_y_discrete(name = NULL,
                    labels = c('A','B','C','D','E','F')) +
   facetted_pos_scales( y  = list( 
      scale_y_discrete(name = NULL,
                       labels = c('<img src="./grViz/_age.png"    width="125">',
                                  '<img src="./grViz/_marsta.png" width="125">',
                                  '<img src="./grViz/_sex.png"    width="125">',
                                  '<img src="./grViz/_edu.png"    width="125">',
                                  '<img src="./grViz/_bmi.png"    width="125">',
                                  '<img src="./grViz/_ncond.png"  width="125">')),
      scale_y_discrete(name = NULL,
                       labels = c('<img src="./grViz/_age_bmi.png"    width="120">',
                                  '<img src="./grViz/_marsta_bmi.png" width="120">',
                                  '<img src="./grViz/_age_marsta.png" width="120">',
                                  '<img src="./grViz/_sex_marsta.png" width="120">')),
      scale_y_discrete(labels = 'Total R<sup>2</sup>')
   )
   ) +
   labs(y = NULL) +
   theme_bw(base_size = 12) +
   theme(panel.grid.major.x = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.spacing = unit(0.25, "lines"),
         axis.text.y = element_markdown(color = "black", size = 12),
         axis.ticks.length.y = unit(0,'cm') 
   ) 

# FIG ALL
f_all <- ggdraw() +  
   draw_image("grViz/_all.png")


# FIG LEGEND
df <- data.frame(
   x = 0.00,
   y = 1.0,
   label = '
   **Bottom:** Graphical depiction of the regression model 
   automatically selected based on MASS::stepAIC which sequentially searches 
   through all possible models (both forward selection and backward elimination) 
   for the one that minimizes AIC. The model selection routine considered all 
   main effects (solid) and two-way interactions (dashed). <br> 
   **Right:** Relative importance (%) derived from the previously selected regression model
   using the LMG metric via the R-package 
   relaimpo to estimate the relative importance of each predictor. LMG estimates 
   the importance by splitting the total R<sup>2</sup> into one non-negative R<sup>2</sup> share per regressor, 
   all of which sum to the total explained R<sup>2</sup>.'
)

f_leg <- ggplot() +
   geom_textbox(
      data = df,
      aes(x, y, label = label),
      color = "black", fill = "gray85", box.color = "gray85",
      width = grid::unit(1.0, "npc"), # 73% of plot panel width
      hjust = 0, vjust = 1,
      size = 4.0
   ) +
   scale_x_continuous(expand  = c(0,0),
                      limits = c(0,1))+
   scale_y_continuous(expand  = c(0,0),
                      limits = c(0,1)) +
   theme_void() 

# FIG COMBINE
layout <- "
AACCC
AACCC
BBCCC
BBCCC
BBCCC"

f_leg + f_all + f_ind + 
   plot_layout( design = layout ) 

# FIG SAVE
ggsave('ed5d_relaimpo.pdf', height = 7.5, width = 8.5, dpi = 600, scale = 1.25)
ggsave('ed5d_relaimpo.png', height = 7.5, width = 8.5, dpi = 600, scale = 1.25)

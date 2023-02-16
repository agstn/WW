# PACKAGES
pacman::p_load(tidyverse, rio)
pacman::p_load(gtsummary, gt, gtExtras)
pacman::p_load(unglue)
pacman::p_load(msm)
pacman::p_load(scales, ggdist)

# options
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(123)

# DATA MANUSCRIPT
dlqi <- import('DLQI_for_AA_by_study.xlsx') %>% 
   janitor::clean_names(case = 'old_janitor') %>% 
   mutate(unglue_data(mean_sd, "{mean_m} ({sd_m})", convert = TRUE) ) %>%
   mutate(study_number = factor(study_number) %>% fct_reorder(mean_m)) %>% 
   rowwise() %>% 
   mutate(sim = data.frame(
      rtnorm = rtnorm(n, mean_m, sd_m, lower = 0, upper = 30),
      rnorm  = rnorm(n, mean_m, sd_m)) %>% list(),
      mean_s = mean(sim$rnorm),
      sd_s   = sd(sim$rnorm)) %>% 
   arrange(study_number)

# Nomenclature
# _f figure
# _m manuscript
# _s simulated

# COLOR
# RColorBrewer::brewer.pal(9,'Reds')
# col_numeric("Reds", domain = 2:11) (dlqi_gt$mean_m) %>% show_col()
#     col_bin("Reds", domain = 2:11, bins = 4) (dlqi_gt$mean_m) %>% show_col()
# col_quantile("Reds", domain = 2:11, n = 4) (dlqi_gt$mean_m) %>% show_col()

# GENERATE GGPLOT
dlqi_gt <- dlqi %>% 
   mutate(
      data = data.frame( 
         mean_f = mean_f, sd_f   = sd_f,
         mean_m = mean_m, sd_m   = sd_m,
         mean_s = mean_s, sd_s   = sd_s) %>% list() ) %>% 
   ungroup() %>%
   mutate(mean_col = col_bin("Reds", domain = 2:11, bins = 4) (.$mean_m) ) %>%
   rowwise() %>%
   mutate(gg_mean = list(
      
      ggplot(data = data,
             aes(x = mean_m, y = '1' )) +
         
         geom_point(size = 40) +
         
         geom_segment(aes(xend = mean_m + sd_m,
                          yend = '1'),
                      linewidth = 10,
                      arrow = arrow( type = 'closed',
                                     length = unit(0.8, "inches") ) ) +
         
         geom_pointrange(data = sim,
                         aes(x = rtnorm, y = '1'),
                         stat = "summary",
                         fun.min = function(z) {quantile(z,0.25)},
                         fun.max = function(z) {quantile(z,0.75)},
                         fun = median,
                         shape = 15,
                         linewidth = 10,
                         size = 10,
                         color = 'gray50',
                         position = position_nudge(y = 0.60) ) +
         
         geom_text(size = 40,
                   nudge_y = -.5,
                   #hjust = 0.05,
                   aes(label = style_number(mean_m, digits = 1)) ) +
         
         geom_text(size = 40,
                   nudge_y = -.5,
                   hjust = 0.7,
                   aes(x = mean_m + sd_m,
                       label = str_glue("\u00B1 {style_number(sd_m, digits = 1)}") ) ) +
         
         coord_cartesian(xlim = c(1, 17)) + 
         
         scale_y_discrete(expand = expansion(add = c(-1, -1) ) ) +
         
         theme_void() +
         
         theme(aspect.ratio = 0.25,
               plot.margin = unit(c(0, 0, 0, 0), 'cm'),
               panel.background = element_rect(fill = mean_col)
         )
   ) ,
   ggplot_mean = NA
   ) %>% 
   mutate(gg_dist = list(
      ggplot( data = sim ,
              aes(y = '1',
                  x = rtnorm      
              ) ) +
         
         geom_vline(xintercept = mean_m,
                    linewidth = 3,
                    color = mean_col) +
         
         geom_dots(smooth = "bounded", layout = "weave",
                   slab_linewidth = 0,
                   slab_colour = 'transparent',
                   binwidth = 0.15,
                   slab_fill = 'gray25',
                   na.rm = TRUE) +
         
         geom_vline(xintercept = c(0, 5, 10, 15, 20, 25, 30),
                    color = 'gray15') +
         
         geom_vline(xintercept = c(0, 5, 10, 15, 20, 25, 30),
                    color = 'gray15') +
         
         scale_x_continuous(breaks = c(0, 6, 10, 20, 30),
                            limits = c(0, 30)) +
         
         scale_y_discrete(expand = expansion(add = c(-30, -2) ) ) +
         
         theme_void() +
         
         labs(x = NULL,
              y = NULL) +
         
         theme(panel.grid.minor.x = element_blank(),
               panel.grid.major.y = element_blank(),
               legend.position="none",
               aspect.ratio = 0.20)
   )
   ) %>%
   mutate(ggplot_dist = NA)


# TABLE 
dlqi_gt %>% 
   select(-sim,
          -whiskers,
          -scale,
          -mean_m,
          -mean_col,
          -mean_sd,
          -ends_with("_f"),
          -ends_with("_s")) %>%
   
   gt() %>% 
   
   gt_add_divider(study_number) %>% 
   
   gt_merge_stack(col1 = study_number, 
                  col2 = reference,
                  font_size = c("14px", "16px")) %>%
   
   data_color(n,      
              colors = scales::col_quantile(
                 palette = "Blues",
                 domain = c(50, 100, 250, 500, 750, 1500),
                 n = 5
              ) ) %>% 
   
   sub_missing() %>% 
   
   fmt_number(columns = age, decimals = 0) %>%
   
   fmt_percent(columns = females, decimals = 1, scale_values = FALSE) %>% 
   
   text_transform(
      locations = cells_body(columns = ggplot_mean),
      fn = function(x) {
         dlqi_gt$gg_mean %>%
            ggplot_image(height = 50, aspect_ratio = 5)
      }
   ) %>%
   text_transform(
      locations = cells_body(columns = ggplot_dist),
      fn = function(x) {
         dlqi_gt$gg_dist %>%
            ggplot_image(height = 50, aspect_ratio = 5)
      }
   ) %>%
   
   cols_align(columns = study_number, align = 'left') %>% 
   
   cols_hide(columns = c(data, sd_m, gg_mean, gg_dist)) %>% 
   
   cols_move(columns = c(age, females), after = study_number) %>% 
   
   cols_label(study_number = md('Study Number<br>Reference'),
              age     = md("Age<br>(yr)"),
              females = md("Female<br>(%)"),
              n       = md("Sample<br>Size"),
              ggplot_mean = html('5-number summary<br>Q1-&#9632;Median-Q3 Mean&rarr;SD'),
              ggplot_dist = md('Density Dot Plot (Simulated)<br>
                               <b>0&emsp;&ensp;
                               5&emsp;
                               10&emsp;
                               15&emsp;
                               20&emsp;
                               25&ensp;
                               30</b>')) %>%
   
   cols_width( study_number ~ px(200),
                    age     ~ px(68),
                    females ~ px(68),
                    n       ~ px(68),
               ggplot_mean ~ px(250),
               ggplot_dist ~ px(250)) %>% 
   tab_spanner(label = "Demographics", columns = c(age, females) ) %>%
   tab_spanner(label = "Statistics",   columns = c(n)) %>%
   tab_spanner(label = md("**DLQI** Displays"), columns = c(ggplot_mean, ggplot_dist)) %>% 
   tab_header(
      title  = md("**Table: Dermatology Life Quality Index (DLQI)** measures from 12 alopecia areata studies"),
      subtitle = md("DLQI is measured from 0-30 with the latter indicating more severe impairment")
   ) %>% 
   tab_footnote(
      footnote =  html(
         "<a href='https://onlinelibrary.wiley.com/doi/abs/10.1111/jdv.18926'> Muntyanu, Anastasiya, et al. <b>The burden of alopecia areata: A scoping review focusing on quality of life, mental health and work productivity.</b>
         Journal of the European Academy of Dermatology and Venereology (2023).</a>"),
      locations = cells_title(groups = "title")
   ) %>% 
   tab_source_note(
      source_note =  html(
         "<details> <summary>References</summary>
<b>94.</b> Andersen YMF, Nymand L, DeLozier AM, et al. Patient characteristics and 
disease burden of alopecia areata in the Danish Skin Cohort. BMJ Open 2022; 12: e053137.
<br>
<b>95.</b> Ferentinos P, Kalogeropoulou E, Pappa G, et al. Assessing the role of 
stressful life events in the induction and recurrence of alopecia areata: 
A case-control study. J Am Acad Dermatol. Epub ahead of print 24 March 2022. 
DOI: 10.1016/j.jaad.2022.03.036.
<br>
<b>22.</b> Ito T, Kamei K, Yuasa A, et al. Health-related quality of life in 
patients with alopecia areata: Results of a Japanese survey with norm-based 
comparisons. J Dermatol 2022; 49:584-593.
<br>
<b>20.</b> Qi S, Xu F, Sheng Y, et al. Assessing quality of life in Alopecia 
areata patients in China. Psychol Health Med 2015; 20: 97-102.
<br>
<b>13.</b> Vélez-Muñiz R d. C, Peralta-Pedrero ML, Jurado-Santa Cruz F, et al. 
Psychological Profile and Quality of Life of Patients with Alopecia Areata. 
Skin Appendage Disorders 2019; 5:293-298.
<br>
<b>100.</b> Ghajarzadeh M, Ghiasi M, Kheirkhah S. Associations between skin 
diseases and quality of life: a comparison of psoriasis, vitiligo, and alopecia 
areata. Acta Med Iran 2012; 50:511-515.
<br>
<b>29.</b> Shi Q, Duvic M, Osei JS, et al. Health-Related Quality of Life (HRQoL) 
in Alopecia Areata Patients-A Secondary Analysis of the National Alopecia Areata 
Registry Data. J Investig Dermatol Symp Proc 2013; 16: S49-S50.
<br>
<b>32.</b> Yu N-L, Tan H, Song Z-Q, et al. Illness perception in patients with 
androgenetic alopecia and alopecia areata in China. Journal of Psychosomatic 
Research 2016; 86: 1-6.
<br>
<b>27.</b> Liu LY, Craiglow BG, King BA. Successful treatment of moderate-to-severe 
alopecia areata improves health-related quality of life. J Am Acad Dermatol 
2018; 78: 597-599.e2.
<br>
<b>19.</b> Abedini R, Hallaji Z, Lajevardi V, et al. Quality of life in mild 
nd severe alopecia areata patients. Int J Womens Dermatol 2018; 4: 91-94.
<br>
<b>16.</b> Zhang M, Zhang N. Quality of life assessment in patients with alopecia 
areata and androgenetic alopecia in the People's Republic of China. Patient 
Prefer Adherence 2017;11: 151-155.
<br>
<b>90.</b> Nasimi M, Ghandi N, Torabzade L, et al. Alopecia Areata-Quality of 
Life Index Questionnaire (Reliability and Validity of the Persian Version) in 
Comparison to Dermatology Life Quality Index. Int J Trichology 2020; 12: 227-233.
</a></details>")
   ) %>% 
   tab_options( data_row.padding = px(0)) %>% 
   opt_align_table_header(align = 'left') %>% 
   gtsave(filename = "DLQI_for_AA_by_study.html")
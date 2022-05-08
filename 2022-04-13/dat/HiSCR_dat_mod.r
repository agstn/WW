# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(gtsummary)

# import
dd_var <- import('dat/HiSCR_dat_w.rds') %>% 
   ungroup() %>% 
   select(id, TRT, an.count_pch, abscesses_ch, drain.fist_ch) %>% 
   as.data.frame() %>% 
   nest(data = everything() )

# Crossing & Models
dd_mod <- crossing( an.count   = seq(0, -75, by = -25),
                    abscesses  = seq(0, -3,  by = -1),
                    drain.fist = seq(0, -3,  by = -1),
                    data = dd_var$data[[1]]  )  %>% 
   nest_by(an.count, abscesses, drain.fist) %>% 
   rowwise() %>% 
   mutate(data = list(data$data %>% 
                         mutate(HiSCR = case_when(
                            an.count_pch  <= an.count & 
                            abscesses_ch  <= abscesses & 
                            drain.fist_ch <= drain.fist   ~ 1,
                            TRUE                          ~ 0))
                      ),
          n = list( table(data$HiSCR))) %>% 
   filter(length(n)>1) %>% 
   mutate(n_y = n[[2]]) %>%
   mutate(table = list( tbl_summary(data    = data,
                                    by      = TRT,
                                    include = HiSCR,
                                    label = list(HiSCR ~ "HiSCR (Yes)"),
                                    statistic = all_categorical(dichotomous = TRUE) ~ "{p}% ({n})") %>% 
                           add_difference(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% 
                           modify_cols_merge(pattern = "{estimate} ({ci})") %>% 
                           modify_header(estimate  = "Difference (95% CI)",
                                         label     = "Recoding",
                                         p.value   = "p-value") %>%
                           modify_footnote(update = everything() ~ NA)
                        ) 
          ) %>% 
   mutate(p_a = inline_text(table, variable = 1, column = 'stat_1', pattern = "{p}%"),
          p_p = inline_text(table, variable = 1, column = 'stat_2', pattern = "{p}%"),
          n_a = inline_text(table, variable = 1, column = 'stat_1', pattern = "({n})"),
          n_p = inline_text(table, variable = 1, column = 'stat_2', pattern = "({n})"),
          d_e = inline_text(table, variable = 1, column = 'estimate'),
          d_l = inline_text(table, variable = 1, column = 'conf.low'),
          d_u = inline_text(table, variable = 1, column = 'conf.high'),
          d_c = inline_text(table, variable = 1, column = 'ci', pattern = '({conf.low}, {conf.high})'),
          d_f = inline_text(table, variable = 1, column = 'p.value', 
                            pvalue_fun = function(x) style_pvalue(x, digits = 3)),
          
          d_n = inline_text(table, variable  = 'HiSCR', column  = 'estimate') %>% 
                 str_remove("%") %>% as.numeric(),
          p_n = inline_text(table, variable  = 'HiSCR', column  = 'p.value', 
                            pvalue_fun = function(x) as.numeric(x)),
          p_l10 = -log10(p_n) )

# Export
dd_mod %>% 
   rio::export('dat/HiSCR_dat_mod.rds')


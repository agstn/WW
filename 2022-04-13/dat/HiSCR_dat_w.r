# packages
pacman::p_load(rio, tidyverse)
pacman::p_load(labelled)  

# import
dd <- rio::import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2022/2022-04-13/HiSCR_dat.csv") %>% 
   mutate(id = 1:n(), .before = 1)

dd %>% rio::export('dat/HiSCR_dat.csv')

# rename
dd_w <- dd %>% 
   mutate(TRT   = factor(TRT, labels = c('Active','Placebo')),
          HiSCR = factor(HiSCR)) %>% 
   rename(abscesses_base  = abscesses.base,
          drain.fist_base = drain.fist.base,
          infl.nod_base	  = infl.nod.base,	 
          abscesses_w16	  = abscesses.w16,	 
          drain.fist_w16  = drain.fist.w16, 
          infl.nod_w16    = infl.nod.w16   ) %>%
   rowwise() %>% 
   mutate(an.count_base   = abscesses_base + infl.nod_base,
          an.count_w16    = abscesses_w16  + infl.nod_w16) %>% 
   mutate(abscesses_ch  = abscesses_w16 - abscesses_base,
          drain.fist_ch = drain.fist_w16 - drain.fist_base,
          infl.nod_ch   = infl.nod_w16   - infl.nod_base,
          an.count_ch   = an.count_w16 - an.count_base,
          an.count_pch  = 100*(an.count_ch/an.count_base) ) %>% 
   mutate(HiSCR_manual = case_when(
      an.count_pch <= -50 & abscesses_ch <= 0 & drain.fist_ch <= 0 ~ 1,
      TRUE                                                        ~ 0
   ))

# label
var_label(dd_w) <- list(abscesses_base  = '# abscess [baseline]',
                        drain.fist_base = '# draining fistulae [baseline]',
                        infl.nod_base	 = '# inflammatory nodules [baseline]',
                        
                        abscesses_w16	 = '# abscess [week 16]',
                        drain.fist_w16  = '# draining fistulae [week 16]',
                        infl.nod_w16    = '# inflammatory nodules [week 16]',
                        
                        abscesses_ch	 = '# abscess [change]',
                        drain.fist_ch   = '# draining fistulae [change]',
                        infl.nod_ch     = '# inflammatory nodules [change]',
                        
                        an.count_base   = '# abscess + inflammatory nodules [baseline]',
                        an.count_w16    = '# abscess + inflammatory nodules [week 16]',
                        an.count_ch     = '# abscess + inflammatory nodules [change]',
                        an.count_pch     = '# abscess + inflammatory nodules [%change]',
                        
                        TRT	          = 'Treatment arm: active or placebo',
                        HiSCR           = 'HiSCR response')

# model
dd_w %>% rio::export('dat/HiSCR_dat_w.csv')
dd_w %>% rio::export('dat/HiSCR_dat_w.rds')

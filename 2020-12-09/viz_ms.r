# LINKS
# modelStudio github: https://github.com/ModelOriented/modelStudio
# modelStudio package: https://modelstudio.drwhy.ai/index.html
# book: Explanatory Model Analysis Explore, Explain, and Examine Predictive Models: http://ema.drwhy.ai/
# website: https://modeloriented.github.io/DrWhy/

# location
rstudioapi::getSourceEditorContext()$path %>% 
  dirname() %>% 
  setwd()

# Import & Reshape
pacman::p_load(tidyverse)

d1 <- read.csv("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2020/2020-12-09/Reexcision.csv") %>% 
  mutate( hist         = factor(hist, labels = c("Others", "Invasive-duct./ductal-lob.")) %>% 
            fct_rev(),
          mult.foc     = factor(mult.foc, labels = c("No","Yes")),
          acc.in.situ  = factor(acc.in.situ, labels = c("Others","DCIS & LCIS")),
          lymph.inv    = factor(lymph.inv, labels = c("No","Yes")) %>% 
            fct_rev(),
          estr.rec     = factor(estr.rec, labels = c("No","Yes")),
          prog.rec     = factor(prog.rec, labels = c("No","Yes")) ) %>% 
  rename(`Residual_Tumor` = RE, 
         `Age` = age,
         `Tumor_Size` = tumor.size,
         `Histology` = hist,
         `Multifocality` = mult.foc,
         `Accomp_in_situ` = acc.in.situ,
         `Lymphovascular` = lymph.inv,
         `Estrogen_receptor` = estr.rec,
         `Progesterone_receptor` = prog.rec) 

# Logistic Regression w/ Interactions
f2 <- glm(Residual_Tumor ~ Age + Tumor_Size + Histology + 
            Multifocality + Accomp_in_situ + Lymphovascular +              
            Age*Lymphovascular + Accomp_in_situ*Lymphovascular, 
          data = d1, family = "binomial")

# Model Studio
pacman::p_load(modelStudio)
pacman::p_load(DALEX)

# Explain
explainer <- explain(f2,
                     data = d1,
                     y = d1$Residual_Tumor,
                     type = 'classification',
                     verbose = FALSE,
                     precalculate = FALSE)

# Interactive
modelStudio(explainer,
            facet_dim = c(1,1),
            new_observation = d1[140:150,],
            eda       = FALSE,
            show_info = FALSE,
            options = ms_options(w = 500, h = 400,
                                 margin_left = 200,
                                 show_boxplot = FALSE,
                                 show_subtitle = TRUE,
                                 ms_title    = "Interactive Model Studio",
                                 ms_subtitle = "Predictors of Residual Tumor in Breast-Conserving Therapy",
                                 positive_color = '#1a9641',
                                 negative_color = '#d7191c',
                                 default_color  = '#404040',
                                 bd_title     = 'Break-down Plot for Logistic Regression model',
                                 bd_subtitle = 'Shows contributions of every variable to a final prediction',
                                 fi_subtitle = '')) %>% 
  r2d3::save_d3_html(file = "viz_ms.html",
                     selfcontained = TRUE)

# Upload to RPubs
# markdown::rpubsUpload(title = "Interactive Model Studio", 
#                       htmlFile = "viz_ms_edited.html")


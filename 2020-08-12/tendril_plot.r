# packages
pacman::p_load(tidyverse, rio)
pacman::p_load(lubridate)
pacman::p_load(labelled)
pacman::p_load(Tendril)
pacman::p_load(ggtext)
pacman::p_load(colorspace)

# import
ae <- import("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2020/2020-08-12/2020-08-12_ae_example_dataset.csv") %>% 
  mutate(rando_date = ymd(rando_date),
         aestdat = ymd(aestdat),
         aeeddat = ymd(aeeddat)) %>% 
  mutate(day = as.numeric(aestdat - rando_date)) %>% 
  as.data.frame()

# Fixed mistake in subject 2011
ae[ae$usubjid == "2011", "arm"] <- "Intervention"

# add labels
var_label(ae) <- list(
  usubjid = "unique subject identifier",
  arm = "treatment assignment name",
  armn = "treatment assignment numeric (0: placebo; 1:intervention)",
  rando_date = "date of randomisation (yyyymmdd)",
  repeatnum = "unique event identifier within usubjid",
  aept = "adverse event code at preferred term/lower level",
  aebodsys = "adverse event code at body system/higher level",
  aesev = "adverse event severity grade (mild, moderate, severe)",
  aesevn = "adverse event severity grade number (1: mild, 2: moderate, 3: severe)",
  aeser = "serious adverse event (no, yes)",
  aesern = "serious adverse event (0: no, 1: yes)",
  aestdat = "adverse event start date (yyyymmdd)",
  aeeddat = "adverse event end date (yyyymmdd)",
  dur = "adverse event duration (days)")

# export
setwd("C:/R/Wonderful-Wednesdays/2020-08-12")
export(ae, "2020-08-12_ae_example_dataset.rds")

# Tendril
subj <- ae %>%
  count(usubjid, arm) %>% 
  #add_row(usubjid = 3000, arm = "Intervention", n = 0) %>% 
  #add_row(usubjid = 3001, arm = "Placebo",      n = 0) %>%
  select(-n) %>% 
  as.data.frame()

pt <- Tendril(mydata = ae,
              rotations = rep(3, nrow(ae)),
              AEfreqThreshold = 5,
              Tag = "Comment",
              Treatments = c("Intervention", "Placebo"),
              Unique.Subject.Identifier = "usubjid",
              Terms = "aebodsys",
              #Terms = "aept",
              Treat = "arm",
              StartDay = "day",
              # SubjList = subj,
              # SubjList.subject = "usubjid",
              # SubjList.treatment = "arm",
              # filter_double_events = TRUE
)

# working with terms 
Terms <- pt$data %>% 
  group_by(Terms) %>% 
  summarise(n = n(),
            x = x[n]) %>% 
  ungroup() %>% 
  arrange(x) %>% 
  mutate(text = str_glue("{Terms} (n={n})")) 

# Reorder guides
pt$data$Terms <- fct_relevel(pt$data$Terms,
                             Terms %>% 
                               pull(Terms) %>% 
                               as.vector()
)

levels(pt$data$Terms) <- Terms %>% pull(text) %>% as.vector()

# plot Results
plot(pt) +
  geom_point(alpha = 0.25) +
  geom_path(alpha = 0.25) +
  geom_vline(xintercept = 0, color = 'gray50', linetype = "dashed") +
  geom_hline(yintercept = 0, color = 'gray50', linetype = "dashed") +
  #scale_color_brewer(type = "qual", palette = "Set1") +
  scale_color_discrete_qualitative(palette = "Dark3")+
  scale_x_continuous(limits = c(-100, 200)) +
  labs(title = "**Tendril Plot** of System Organ Class Adverse Events (AE) having at least 5 incidences",
       # caption ="The Tendril Plot: a novel visual summary of the incidence, significance and temporal aspects of AE in clinical trials (JAMIA 2018; 25(8): 1069-1073)"
       caption = 
       "Each MedDRA adverse event code at body system/higher level is
       represented by a line (tendril) and each point is an event. Since time runs
       along each tendril, it is the shape that carries the important information,
       rather than the x and y coordinates. An event on the Intervention treatment
       arm will tilt tendril direction to the *right*, and an event on the placebo arm will
       tilt tendril direction to the *left*.
       <br>
       *The Tendril Plot: a novel visual summary of the incidence, significance and temporal aspects of AE in clinical trials (JAMIA 2018; 25(8): 1069-1073)*"
       ) +
  guides(color = guide_legend(title = "AE at Body System")) +
  theme(aspect.ratio = 0.70,
        plot.title = element_markdown(),
        plot.caption = element_textbox_simple(
          size = 8,
          lineheight = 1,
          hjust = 0, vjust = 1,
          padding = margin(1, 1, 1, 1),
          margin = margin(1, 1, 1, 1)
        ),
        plot.caption.position = "plot",
        legend.position       = c(0.99,0.92),
        legend.justification  = c(1,1),
        legend.background     = element_rect(fill  = 'gray90'),
        legend.key            = element_rect(fill  = 'gray90'))

ggsave("C:/R/Wonderful-Wednesdays/2020-08-12/tendril_plot.png",
       width = 7.5, height = 6, units = "in")

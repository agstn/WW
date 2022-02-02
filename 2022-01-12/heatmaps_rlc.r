# packages
library(tidyverse)
library(rlc)

# data
d1 <- read.csv("https://raw.githubusercontent.com/agstn/WW/main/2022-01-12/data_sample.csv") %>% 
   select(AccrualTime, FollowUpTime, HazardRatio, HazardRate = hazard_rate, Power, NTotal) %>% 
   mutate(TotalTime = AccrualTime + FollowUpTime, .after = FollowUpTime)

d2 <- read.csv("https://raw.githubusercontent.com/agstn/WW/main/2022-01-12/data_power.csv") %>% 
   select(AccrualTime, FollowUpTime, HazardRatio, HazardRate = hazard_rate, Power, NTotal) %>% 
   mutate(TotalTime = AccrualTime + FollowUpTime, .after = FollowUpTime,
          Power = round(100*Power,0))

d_all <- left_join(
   d1    %>% select(-Power),
   d2    %>% select(-NTotal)) %>% 
   group_nest(AccrualTime, FollowUpTime, TotalTime) %>%
   rowwise() %>% 
   mutate(data_ss = list( data %>% 
                             select(-Power) %>% 
                             pivot_wider(names_from  = HazardRate,
                                         values_from = NTotal) %>% 
                             select(HazardRatio, `4`:`6`) %>% 
                             column_to_rownames(var = 'HazardRatio')),
          data_pw = list( data %>% 
                             select(-NTotal) %>% 
                             pivot_wider(names_from  = HazardRate,
                                         values_from = Power) %>% 
                             select(HazardRatio, `4`:`6`) %>% 
                             column_to_rownames(var = 'HazardRatio'))
   )

# HEATMAP MATRIX
d_time <- d_all %>% 
   select(contains("Time")) %>% 
   distinct() %>%
   pivot_wider(names_from  = FollowUpTime,
               values_from = TotalTime) %>%
   column_to_rownames(var = 'AccrualTime')


# LAYOUT
openPage(useViewer=FALSE, layout="table1x3" )

AT <- 24
FT <- 6
SS <- NA
PW <- NA

# FIG 1
lc_heatmap(
   dat(
      value = d_time,
      title = str_glue("Total Time:{AT + FT} [Accural:{AT} & Follow-up:{FT}] (months)"),
      on_click = function(k) { 
         AT <<- rownames(d_time)[k[1]] %>% as.numeric()          
         FT <<- colnames(d_time)[k[2]] %>% as.numeric() 
         updateCharts("Fig1") # Title
         updateCharts("Fig2") # Sample Size
         updateCharts("Fig3") # Power
      }
   ),
   chartId = "Fig1",
   rowTitle = "Accural Time",
   colTitle = "Follow-up Time",
   palette = rev(RColorBrewer::brewer.pal(11, "RdYlGn")),
   colourDomain = c(21, 39),
   paddings = list(top = 50, left = 30, bottom = 30, right = 30),
   showPanel  = FALSE,
   showLegend = FALSE,
   place = "A1"
)

lc_colourSlider(chart = "Fig1",
                chartId = "Fig_S1",
                place = "A1",
                width = 450,
                title = "Total Time",
                titleSize = 15, titleY = 12,
                paddings = list(top = 35, left = 35, bottom = 10, right = 20))

# FIG 2
lc_heatmap(
   dat(value = d_all %>% 
          filter(AccrualTime == AT, FollowUpTime == FT) %>% 
          pull(data_ss) %>% 
          .[[1]],
       on_click = function(k) {
          data_ss <- d_all %>% 
             filter(AccrualTime == AT, FollowUpTime == FT) %>% 
             pull(data_ss) %>% 
             .[[1]]
          
          SS     <<- data_ss[k[1],k[2]]
          updateCharts("Fig2")
       },
       title = str_glue("Sample Size {ifelse(is.na(SS),'[click a cell]',SS)} for 90% power")
   ),
   chartId = "Fig2",
   rowTitle = "Hazard Ratio",
   colTitle = "Hazard Rate (%)",
   palette = RColorBrewer::brewer.pal(9, "Blues"),
   colourDomain = c(4800, 24000),
   paddings = list(top = 50, left = 30, bottom = 30, right = 30),
   showValue = FALSE,
   showPanel  = FALSE,
   showLegend = FALSE,
   place = "A2"
)

lc_colourSlider(chart = "Fig2",
                chartId = "Fig_S2",
                place = "A2",
                width = 450,
                title = "Sample Size",
                titleSize = 15, titleY = 12,
                paddings = list(top = 35, left = 35, bottom = 10, right = 20))

# FIG 3
lc_heatmap(
   dat(value = d_all %>%
          filter(AccrualTime == AT, FollowUpTime == FT) %>%
          pull(data_pw) %>%
          .[[1]],
       on_click = function(k) {
          data_pw <- d_all %>%
             filter(AccrualTime == AT, FollowUpTime == FT) %>%
             pull(data_pw) %>%
             .[[1]]

          PW     <<- data_pw[k[1],k[2]]
          updateCharts("Fig3")
       },
       title = str_glue("Power {ifelse(is.na(PW),'[click a cell]',PW)}% for 9726 participants")
   ),
   chartId = "Fig3",
   rowTitle = "Hazard Ratio",
   colTitle = "Hazard Rate (%)",
   palette = RColorBrewer::brewer.pal(9, "Oranges"),
   colourDomain = c(50, 100),
   paddings = list(top = 50, left = 30, bottom = 30, right = 30),
   showValue = FALSE,
   showPanel  = FALSE,
   showLegend = FALSE,
   place = "A3"
)

lc_colourSlider(chart = "Fig3",
                chartId = "Fig_S3",
                place = "A3",
                width = 450,
                title = "Power",
                titleSize = 15, titleY = 12,
                paddings = list(top = 35, left = 35, bottom = 10, right = 20))

lc_html(content = 
           "<b>Heatmaps:<a href='https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2022/2022-01-12'>
                       Uncertainty in study planning </a></b>
            <br> 
            <i>Click on a scenario from the first heatmap to investiage Sample Size and Power under different Hazard Ratios and Hazard Rates</i>",
        chartId = 'title')



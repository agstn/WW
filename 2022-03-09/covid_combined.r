# packages
pacman::p_load(rio, tidyverse) 
pacman::p_load(RColorBrewer, ggtext)
pacman::p_load(patchwork)

# individual plots
source('./fig/covid_tile_order.r')
source('./fig/covid_histogram.r')
source('./fig/covid_modal.r')
source('./fig/covid_mean.r')

# layout
design <- "11114
           11114
           11114
           2222#
           2222#
           3333#"

# combine
(f_t + theme(plot.margin = unit(c(0, 0, 0, 0), "lines") ) ) +
   
   (f_h + theme(strip.background = element_blank(), 
                strip.text = element_blank(),
                axis.title.x=element_blank(),
                plot.margin = unit(c(0, 0, 0, 0), "lines") ) ) +
   
   (f_mo + theme(strip.background = element_blank(), 
                 strip.text = element_blank(),
                 plot.margin = unit(c(0, 0, 0, 0), "lines") ) ) +
   (f_me) +
   
   
   plot_layout(design = design) +
   plot_annotation(
      title    = 'COVID-19 study comparing **Active vs Placebo** on Ordinal Scale for Clinical Improvement (OSCI) daily measures (35 days)',
      subtitle =
      '**A. Full Index Plot** representing all individual OSCI sequences sorted accordingly to the finished order. 
      **B. Frequency Plot** representing OSCI sequences distributions 
      **C. Sequences of modals** summary derived from the OSCI sequences distributions 
      **D. Mean time spent in each OSCI** summary of the mean number of times each OSCI is observed in a sequence. 
      This characterizes the overall OSCI distribution',
      #caption  = '**Data from Wonderful-Wednesdays** <span style="font-size:8pt">github.com/VIS-SIG[2022−03−09]</span>',
      theme = theme(plot.title.position = "plot",
                    plot.title = element_textbox_simple(
                       size = 16,
                       lineheight = 1,
                       r = unit(5, "pt"),
                       padding = margin(10, 10, 7, 10),
                       margin = margin(2, 6, 2, 6),
                       fill = "gray85"
                    ),
                    #plot.subtitle.position = "plot",
                    plot.subtitle = element_textbox_simple(
                       size = 14,
                       lineheight = 1,
                       r = unit(5, "pt"),
                       padding = margin(5, 5, 5, 5),
                       margin = margin(2, 6, 2, 6),
                       fill = "gray95"
                    ),
                    plot.caption  = element_markdown()) )

#save
ggsave('./covid_combined.pdf', width = 13, height = 10)
ggsave('./covid_combined.png', width = 13, height = 10, dpi = 600)

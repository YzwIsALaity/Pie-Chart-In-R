# Pie chart
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

Dt.Plot <- read.csv('Pie Chart.csv')

# Percentage label
Dt.Plot$'Percentage.Label' <- paste0(Dt.Plot$Percentage, '%')

# Factor for Origin and Type
Dt.Plot$Origin <- factor(Dt.Plot$Origin, levels = c("Mexican", 'White, NH', 'Black, NH'))
Dt.Plot$Type <- factor(Dt.Plot$Type, levels = c("Comprehensive Community Cancer Program",
                                                "Academic/Research Program", 
                                                "Community Cancer Program", 
                                                "Integrated Network Cancer Program", 
                                                "Missing"))

# Pie chart
Dt.PieChart <- 
  Dt.Plot %>% 
  arrange(Origin, desc(Type)) %>%  # arrange it with Origin and decreasing order of Type
  group_by(Origin) %>%             # group by Origin
  mutate(text_y = cumsum(Percentage) - Percentage/2) # the middle of each rectangle

head(Dt.PieChart)

# Single pie chart
Dt.Mexican <- Dt.PieChart[which(Dt.PieChart$Origin == 'Mexican'), ]

ggplot(Dt.Mexican, 
       aes(x = '',               # We don't want to show x axis
           y = Percentage,       # y axis is numerical percentage
           fill = Type)) +       # color for each facility type
  geom_col(width = 1) +          # create a bar chart with width = 1
  coord_polar(theta = "y") +     # transfer y axis into polar coordinate
  scale_fill_brewer() +          # color 
  geom_text_repel(aes(y = text_y, 
                      label = Percentage.Label),  # label text
                  size = 4,                       # size of text
                  show.legend = FALSE,            # remove legend
                  nudge_x = 1.5) +                # the distance between text and figure
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_text(size = 13, face = 'bold'), 
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13, face = 'bold'),      
        strip.background = element_blank())

# Multiple pie charts
ggplot(Dt.PieChart, 
       aes(x = '',               # We don't want to show x axis
           y = Percentage,       # y axis is numerical percentage
           fill = Type)) +       # color for each facility type
  geom_col(width = 1) +          # create a bar chart with width = 1
  coord_polar(theta = "y") +     # transfer y axis into polar coordinate
  scale_fill_brewer() +          # color 
  geom_text_repel(aes(y = text_y, 
                      label = Percentage.Label),  # label text
                  size = 4,                       # size of text
                  show.legend = FALSE,            # remove legend
                  nudge_x = 1.5) +                # the distance between text and figure
  facet_grid(cols = vars(Origin),   # faceted by Origin variable
             scales = 'fixed') + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_text(size = 13, face = 'bold'), 
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13, face = 'bold'),      
        strip.background = element_blank())
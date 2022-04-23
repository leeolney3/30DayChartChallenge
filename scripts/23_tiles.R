# 30DayChartChallenge (timeseries) 23. tiles
# Food price inflation in the Caribbean
# Data source: FAO, available at: https://www.fao.org/faostat/en/#data/CP

# Libraries
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Sans Condensed")
f1="IBM Plex Sans Condensed"

# Data
data = read_csv("data/23_FAOSTAT.csv") %>% janitor::clean_names()

# Wrangle
data1 = data %>%
  filter(year>=2015) %>%
  mutate(mth = match(months, month.name)) %>%
  mutate(date= zoo::as.yearmon(paste(year, mth), "%Y %m"))

lev =data1 %>% filter(date==max(date)) %>%
  select(area,value, date) %>% 
  arrange(value) %>%
  pull(area)
  
# Plot
data1 %>%
  ggplot(aes(x=date, y=factor(area, levels=lev), fill=value)) +
  geom_tile(height=.7) +
  geom_text(data=lab, aes(x=date+.8, label=scales::percent(value, scale=1, accuracy=.01)), 
            size=3, family=f1, hjust=1) +
  annotate(geom="text", size=3, family=f1, hjust=.8, x=2022.3, y=21.8, label="Sep 2021") + 
  scico::scale_fill_scico(palette="oleron", midpoint = 0, labels=scales::percent_format(scale=1)) +
  scale_x_continuous(position="top", breaks=seq(2015,2021,1),expand = expansion(mult = c(0, .007))) +
  scale_y_discrete(expand = expansion(mult = c(0.04, 0))) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_vgrid(12) +
  theme(text=element_text(family=f1),
        panel.grid.major.x=element_line(color="grey70", size=.3),
        legend.position = "top",
        axis.title=element_blank(),
        axis.ticks.y.left = element_blank(),
        legend.title=element_blank(),
        plot.margin=margin(.5,.75,.3,.5,unit="cm"),
        plot.title.position = "plot",
        legend.margin=margin(l=-40),
        plot.title=element_text(hjust=.5, size=14),
        plot.subtitle = element_text(hjust=.5, size=10),
        plot.caption=element_text(margin=margin(t=15), size=9, color="grey20"),
        axis.text.x.top = element_text(size=9),
        axis.text.y = element_text(size=10),
        legend.text=element_text(size=10),
        axis.line.y = element_blank()
        ) +
  guides(fill=guide_colorbar(barwidth = unit(18, "lines"), 
                             barheight = unit(.5, "lines")
                             )) +
  labs(title="Food price inflation in the Caribbean",
       subtitle="Monthly, from Jan 2015 to Sep 2021. Areas arranged in descending order of food price inflation in Sep 2021",
       caption="#30DayChartChallenge 23. tiles  |  Source: FAO")
       
ggsave("23_tiles.png", bg="white", height=7, width=7)

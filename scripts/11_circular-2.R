# 30DayChartChallenge 11 Circular
# Belgium gas storage from from January 1, 2011 to March 31, 2022
# Data source: Gas Infrastructure Europe, by way of Data Is Plural
# Dataset link: https://agsi.gie.eu/#/graphs/BE

library(tidyverse)
library(lubridate)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Roboto")
f1 = "Roboto"

df = read_csv("data/gas_storage_bel.csv") %>% janitor::clean_names()
df = df %>% mutate(year=year(gas_day_started_on)) %>%
  filter(year<2022)
  
df %>%
  select(gas_day_started_on,gas_in_storage_t_wh) %>%
  mutate(mth = month(gas_day_started_on)) %>%
  ggplot(aes(x=mth, y=gas_in_storage_t_wh,group=mth)) +
  #geom_point(aes(color=gas_in_storage_t_wh),alpha=.5, shape=21) +
  ggforce::geom_sina(aes(color=gas_in_storage_t_wh), alpha=.5, shape=21) +
  geom_text(data=tibble(x=6.5, y=seq(2,8,2), lab=c("2","4","6","8TWh")),
            aes(x=x, y=y, label=y),
            inherit.aes = FALSE, family=f1) +
  scico::scale_color_scico(palette="imola", direction=-1, labels=scales::label_number(suffix="TWh")) +
  scale_x_continuous(breaks=c(seq(1,12,1)), labels=month.abb[1:12]) +
  scale_y_continuous(breaks=c(seq(0,8,2))) +
  coord_polar() +
  cowplot::theme_minimal_grid(12, line_size = .3) +
  theme(text=element_text(family=f1),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=9.5),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        plot.margin=margin(.5,0,.3,0, unit="cm"),
        legend.justification = "center",
        legend.margin=margin(b=-15),
        plot.title=element_text(hjust=.5, size=14),
        plot.subtitle=element_text(hjust=.5, size=9),
        plot.caption=element_text(size=8, hjust=.5, margin=margin(t=-10))
        ) +
  guides(color=guide_colorbar(barwidth = unit(18,"lines"),
                              barheight = unit(.4,"lines"))) +
  labs(title="Belgium Gas Storage",
       subtitle="Country-level daily amount of gas storage (TWh), from January 1, 2011 to December 31, 2021",
       caption="#30DayChartChallenge (Distributions) 11 Circular | Source: Gas Infrastructure Europe") 
       
ggsave("distribution_circular.png",bg="white")
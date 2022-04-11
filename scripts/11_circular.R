# 30DayChartChallenge 10 Experimental
# Alternative fuel transit buses by fuel type
# Data source: AFDC, by way of Data Is Plural (https://afdc.energy.gov/data/categories/vehicles--2)
# Plot inspired by Cédric Scherer @CedScherer (https://www.behance.net/gallery/100683383/Travelling-to-Outer-Space) and Blake Robert Mills @BlakeRobMills (https://twitter.com/BlakeRobMills/status/1453207974646849537)

library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Karla")
f1="Karla"

# Data 
data = tibble::tribble(
     ~Year, ~Diesel, ~Gasoline, ~Natural.Gas, ~Hybrid, ~`Biodiesel*`, ~Other, ~Total,
    "2007",   52069,       391,        10179,    1501,           "-",   1109,  65249,
    "2008",   46687,       333,        12304,    2527,       "4,389",    266,  66506,
    "2009",   44669,       454,        11864,    3177,       "4,149",    519,  64832,
    "2010",   43585,       464,        12320,    4637,       "5,100",    132,  66239,
    "2011",   43926,       553,        12867,    6087,       "5,465",    277,  69175,
  "2012**",   42779,       667,        13546,    7721,       "5,229",    246,  70187,
    "2013",   40051,       711,        11951,   12734,       "5,478",    213,  71139,
    "2014",   42468,       650,        11031,   11692,       "5,030",    195,  71066,
    "2015",   39140,       773,        15391,   11415,       "5,223",    132,  72075,
    "2016",   36091,      1131,        18219,   11392,       "4,991",    133,  71956,
    "2017",   32718,      1383,        20800,   10558,       "6,662",    266,  72387,
    "2018",   32671,      1178,        19151,   13872,       "4,275",    597,  71743,
    "2019",   29989,      1166,        21486,   12696,       "5,850",    557,  71744
  ) %>% janitor::clean_names()

# Wrangle
df = data %>% 
  select(-total) %>%
  mutate(biodiesel= parse_number(biodiesel)) %>%
  pivot_longer(2:7) %>%
  mutate(year=parse_number(year))

lev = df %>% group_by(name) %>% tally(value) %>% arrange(n) %>%
  pull(name)
  
# Plot
df %>%
  mutate(name=factor(name, levels=rev(lev)),
         value=replace_na(value,0)) %>%
  arrange(year,name) %>%
  group_by(year) %>%
  mutate(val = cumsum(value)) %>%
  ungroup() %>%
  ggplot() + 
  annotate(geom="segment",x=rep(2005.5,7), xend=rep(2021,7), 
           y=seq(10000,70000,10000), yend=seq(10000,70000,10000), size=.3, color="grey80") +
  ggforce::geom_link(aes(x=year, xend=year, y=0, yend=val, 
                         color=fct_rev(name)), size=2) +
  geom_text(data = df %>% filter(year %in% c(2007, 2013,2019), name=="other"),
            aes(label=year, x=year, y=0), vjust=2, size=3, family=f1) +
  geom_text(data = df %>% filter(year %in% c(2007, 2013,2019), name=="other"),
            aes(label="|", x=year, y=0), vjust=1.5, size=1.5, familiy=f1) +
  scale_color_manual(values=rev(c("#D55E00","#0073B2","#F1E542","#009E73","#E79F00","black")),
                     labels=c("Other","Gasoline","Biodiesel","Hybrid","Natural Gas","Diesel")) +
  #rcartocolor::scale_color_carto_d(palette="Vivid") +
  coord_polar(theta = "y", clip="off", start = 4.71)+
  scale_x_continuous(expand = expansion(mult = c(1, -0.12))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), 
                     breaks=seq(10000,70000,10000), labels=scales::comma_format()) +
  theme_void() +
  theme(text=element_text(family=f1),
        axis.text.x=element_text(color="grey50", size=8),
        legend.position=c(.5,.5),
        legend.title=element_blank(),
        plot.title=element_text(size=15,face="bold"),
        plot.subtitle = element_text(size=8.5),
        plot.margin=margin(.3,.3,.3,.3, unit="cm"),
        plot.caption=element_text(hjust=0, size=8, margin=margin(t=-8))) +
  labs(caption="Notes: Natural Gas includes compressed and liquefied forms. Other up to 2007 included propane, bio/soy fuel, and biodiesel. After\n2007, Other included battery-electric, hydrogen, and propane.\n\n#30DayChartChallenge 11 circular | Data from US Department of Energy’s Alternative Fuels Data Center, by way of Data is Plural",
       title="Alternative Fuels Transit Buses, 2017-2019",
       subtitle="Number of transit buses in use in the United States, categorized by fuel type, from 2007 to 2019. In all years shown, diesel\nbuses represent the largest portion of total buses, with natural gas buses a distant second. Hybrid buses are the fastest\ngrowing category as they have increased more than eight times from 2007 to 2019. The increase in both natural gas and\nhybrid buses is largely a result of favorable economics and clean air benefits in the transit bus application.") +
  guides(color=guide_legend(reverse=T))
  
ggsave("11_circular.png", height=8, width=6.8, bg="white")
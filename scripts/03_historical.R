# 30DayChartChallenge 03 historical
# Young self-employed, 20-29 year-old men / 20-29 year-old women, % of total employed, 1990 – 2020
# Data source: OECD, available at https://data.oecd.org/entrepreneur/young-self-employed.htm

# Libraries
library(tidyverse)
library(ggtext)
library(countrycode)
library(showtext)

# Font
font_add_google("Fira Sans Condensed")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import data
data = read_csv("data/young-self-employed.csv") %>% janitor::clean_names()

# Wrangle
selected =data %>% count(location, sort=T) %>% filter(n>37) %>% pull(location)
data1 = data %>% filter(location %in% selected) %>%
  mutate(name = countrycode(location, origin="iso3c", destination="country.name")) 
  
labdf = data1 %>% select(name, subject, time, value) %>%
  group_by(name, time) %>%
  summarise(diff=diff(value)) %>%
  ungroup() %>%
  group_by(name) %>%
  filter(time==max(time)) %>%
  mutate(lab= glue::glue("**{name}**<br>∆ {round(diff,2)}% in {time}")) %>%
  arrange(diff) %>%
  select(name, lab)
  
lev = labdf %>% pull(lab) #country levels

# Plot
data1 %>%
  left_join(labdf, by="name") %>%
  ggplot() +
  geom_point(aes(time, value, color=subject, group=subject), size=.8) +
  scale_color_manual(values=c("#659E9D","#E37000")) +
  scale_y_continuous(limits=c(0,20), labels=scales::percent_format(scale=1, accuracy=1)) +
  facet_wrap(~factor(lab, levels=lev)) +
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Fira Sans Condensed"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size=.3),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title = element_markdown(size= 15),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text=element_markdown(lineheight = 1.2, size=9.5),
        plot.subtitle=element_markdown(size=9, lineheight = 1.2, color="grey20"),
        plot.caption=element_text(color="grey20"),
        axis.title=element_blank()) +
  labs(title="**Young self-employed <span style='color:#659E9D'>men</span> and <span style='color:#E37000'>women</span>** (1990 - 2020)",
       subtitle="Share of self-employed aged 20-29 among all employed workers aged 20-29 for each group (men/women), of 16 selected<br> countries (with the most records in the time period). Arranged in ascending order of the difference in the most recent year<br>with records of both groups, that is labeled below the country name.",
       caption="\n#30DayChartChallenge 03 Historial  |  Data source: data.oecd.org")
       
# Save plot
ggsave("03_historical.png", bg="white")


# Facet dot plot comparing the share of self-employed aged 20-29 among all employed workers aged 20-29 for each group (men/women) from 1990 to 2020. Year on x-axis, percentage on y-axis, dot color by group (men/women), and facet by country. The plot includes 16 countries with the most records (men and women) in the time period. The selected countries are arranged in ascending order of the difference between men and women, of the most recent year with records of both group. The plot shows that among the 16 countries with the most records, Greece (2020) has the largest difference (-8.08%) while Hungary (2020) has the smallest difference (-1%). Data from OECD, available at available at https://data.oecd.org/entrepreneur/young-self-employed.htm
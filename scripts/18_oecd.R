# 30DayChartChallenge 18 data day: OECD
# Municipal Waste (Total, Kilograms/capita, 2000 – 2020)
# Data source: https://data.oecd.org/waste/municipal-waste.htm

library(tidyverse)
library(ggtext)
library(countrycode)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Lato")
f1="Lato"

data = read_csv("data/DP_LIVE_31032022124539396.csv") %>% janitor::clean_names()

# Wrangle
data1 = data %>%
  select(location, time, value) %>%
  mutate(name = countrycode(location, origin="iso3c", destination="country.name"),
         name = case_when(location=="OECD"~"**OCED - Total**",
                          location=="OECDE"~"**OECD - Europe**",
                          TRUE~name))

selected = data1 %>% filter(time %in% c(2000,2010,2020)) %>%
  count(location, sort=T) %>%
  filter(n==3) %>% pull(location)

data2 = data1 %>% 
  filter(time %in% c(2000,2010,2020), location %in% selected) 
  
lev = data2 %>% filter(time==2020) %>%
  arrange(desc(value)) %>%
  mutate(name=fct_inorder(name))

# Plot
  ggplot(aes(y=factor(name, levels=lev$name), x=value)) +
  geom_line(aes(group=name), color="#868EA0") +
  geom_point(aes(fill=factor(time), shape=factor(time), color=factor(time)), size=3, alpha=.7) +
  scale_x_continuous(labels=scales::label_number(suffix="kg/cap"), limits=c(200,900), expand=c(0,0)) +
  scale_shape_manual(values=c(23,22,21)) +
  scale_fill_manual(values=c("#008F91","#204AD4","#CC2929")) +
  scale_color_manual(values=c("#008F91","#204AD4","#CC2929")) +
  theme_minimal() +
  theme(text=element_text(size=12, family=f1),
        panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_markdown(color="black"),
        axis.text.x=element_text(color="black"),
        legend.position="top",
        legend.title=element_blank(),
        panel.grid.major.y=element_line(linetype = "dotted", color="grey85"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(.5,.7,.5,.5, unit="cm"),
        plot.title.position = "plot",
        legend.justification = "left",
        plot.title=element_markdown(),
        plot.subtitle = element_text(size=10),
        plot.caption=element_text(color="#4a4a4a", size=9),
        legend.margin = margin(b=-5),
        plot.background=element_rect(fill="#fafafa", color=NA)
        ) +
  labs(title="**Total Municipal Waste**, Kilograms/capita",
       subtitle="Countries arranged in ascending order of 2020 total municipal waste",
       caption="\n#30DayChartChallenge 18 data day: OECD")
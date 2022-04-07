# 30DayChartChallenge 07 physical
# Self-Reported Physical Inactivity Among US Adults
# Data source: CDC available at: https://www.cdc.gov/physicalactivity/data/inactivity-prevalence-maps/index.html

# Libraries
library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(usmap)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Saira Semi Condensed")
f1= "Saira Semi Condensed"

# Data
df_overall = read_csv("physical_inactivity/1-self-reported.csv") %>% janitor::clean_names() %>%
  select(state, Overall=prevalence)

df1 = read_csv("physical_inactivity/6-non-hispanic-aian-adults.csv") %>% janitor::clean_names() %>%
  select(state, "Non-Hispanic American Indian/Alaska Native"=prevalence)

df2 = read_csv("physical_inactivity/5-non-hispanic-asian-adults.csv") %>% janitor::clean_names() %>%
  select(state, "Non-Hispanic Asian"=prevalence)

df3 = read_csv("physical_inactivity/3-non-hispanic-black-adults.csv") %>% janitor::clean_names() %>%
  select(state, "Non-Hispanic Black"=prevalence)

df4 = read_csv("physical_inactivity/2-non-hispanic-white-adults.csv") %>% janitor::clean_names() %>%
  select(state, "Non-Hispanic White"=prevalence)

df5 = read_csv("physical_inactivity/4-hispanic-adults.csv") %>% janitor::clean_names() %>%
  select(state, "Hispanic"=prevalence)
  
combined = df_overall %>%
  left_join(df1, by="state") %>%
  left_join(df2, by="state") %>%
  left_join(df3, by="state") %>%
  left_join(df4, by="state") %>%
  left_join(df5, by="state") 
  
# Wrangle
combined2 = combined %>%
  pivot_longer(2:7) %>%
  mutate(value=parse_number(value)) %>%
  filter(!is.na(value)) %>%
  mutate(name = factor(name, levels=c("Non-Hispanic American Indian/Alaska Native","Hispanic","Non-Hispanic Black","Non-Hispanic White","Non-Hispanic Asian","Overall"))) %>%
  mutate(st = state.abb[match(state,state.name)],
         st = case_when(state=="District of Columbia"~"DC",
                        TRUE~st),
         region=case_when(st %in% .northeast_region~"Northeast",
                          st %in% .midwest_region ~"Midwest",
                          st %in% .south_region ~"South",
                          st %in% .west_region ~"West"
                          )) %>%
  filter(!is.na(region))
  
med = combined2 %>%
  group_by(name) %>%
  summarise(value=median(value))
  
# Plot
# ggbeeswarm::geom_quasirandom reference: https://z3tt.github.io/beyond-bar-and-box-plots/

combined2 %>%
  ggplot(aes(x=name, y=value)) +
  geom_quasirandom(aes( color=region),
    size = 2.5, width = .33, alpha = .8) + 
  geom_quasirandom(aes( color=region),
    size = 2.5, width = .33, shape = 1, color = "black", stroke = .2) +
  geom_point(data= med, shape="|", size=11, color="#495057") +
  geom_text(data=med %>% filter(name!="Overall"), aes(label=scales::percent(value, scale=1)), size=3.2, vjust=-3.7, family=f1) +
  geom_text(data=med %>% filter(name=="Overall"), aes(label=glue::glue("Median: {scales::percent(value,scale=1)}")), size=3.2, vjust=-3.7, family=f1) +
  scale_x_discrete(labels=scales::wrap_format(18)) +
  scale_y_continuous(labels=scales::percent_format(scale=1, accuracy = 1)) +
  MetBrewer::scale_color_met_d("Egypt") +
  coord_flip() +
  theme_minimal(13.5) +
  theme(text=element_text(family=f1),
        plot.title.position="plot",
        plot.subtitle = element_text(size=11),
        legend.position="top",
        legend.justification = "left",
        legend.margin=margin(l=-67,t=-4, b=-6),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=11),
        axis.text.y=element_text(color="black"),
        axis.text.x=element_text(size=10),
        legend.text=element_text(size=11),
        legend.title=element_text(size=11),
        plot.caption.position = "plot",
        plot.caption=element_text(color="grey20", size=9, hjust=0, margin=margin(t=13)),
        plot.margin=margin(.5,.7,.3,.5, unit="cm"),
        panel.grid=element_line(size=.4),
        panel.grid.major.y=element_line(linetype="dotted", size=.7),
        ) +
  labs(title="Self-Reported Physical Inactivity Among US Adults",
       y="Prevalence of inactivity",
       subtitle="Prevalence of inactivity (%) by state and race/ethnicity, using combined data from 2017 through 2020",
       caption="#30DayChartChallenge 07 physical | Data source: CDC Behavioral Risk Factor Surveillance System (BRFSS)",
       color="Region")  
       
ggsave("07_physical.png", bg="white")
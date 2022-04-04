# 30DayChartChallenge 01 part-to-whole
# College sports participants (2015-2019), coed men/coed women/men/women
# Data source: Equity in Athletics from #TidyTuesday Week 13, available at https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-29/readme.md

# Libraries 
library(tidyverse)
library(ggtext)
library(showtext)

# Font
font_add_google("Dosis","dosis")
f1 = "dosis"
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

# Wrangle
selected = c("Archery", "Rifle", "Rodeo", "Sailing") 

df1 = sports %>% group_by(sports) %>%
  summarize(partic_men= sum(partic_men, na.rm=T),
            partic_women=sum(partic_women, na.rm=T),
            coed_men= sum(partic_coed_men, na.rm=T),
            coed_women = sum(partic_coed_women, na.rm=T)) %>%
  ungroup() %>%
  filter(sports %in% selected) %>%
  pivot_longer(2:5) %>%
  group_by(sports) %>%
  mutate(pct = value/sum(value)*100) %>%
  mutate(id=case_when(name=="partic_men"~1,
                      name=="partic_women"~2,
                      name=="coed_men"~3,
                      name=="coed_women"~4)) %>%
  mutate(name=fct_inorder(name),
         sports=fct_inorder(sports)) %>%
  ungroup()
  
lab =df1 %>% group_by(sports) %>% tally(value) %>%
  mutate(lab=glue::glue("**{sports}** (n= {scales::comma(n)})")) %>%
  select(sports,lab) # strip text labels 
  
df2 = df1 %>%
  group_by(sports) %>%
  mutate(xmin = 0, ymin=0,
         xmax=case_when(id==1|id==3 ~ -1*(sqrt(pct)), TRUE~sqrt(pct)),
         ymax=case_when(id==1|id==2 ~ -1*(sqrt(pct)), TRUE~sqrt(pct))
         ) %>%
  mutate(x=case_when(id==1|id==3~-0.5,TRUE~0.15),
         y=case_when(id==1|id==2~-0.15, TRUE~0.15),
         x2=case_when(id==1|id==3~-.9,TRUE~0.05),
         y2 = case_when(id==1|id==2~-0.8, TRUE~0.7)) %>%
  left_join(lab, by="sports" )

# Plot  
df2 %>%ggplot() +
  geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax/4, ymax=ymax/4, fill=factor(id))) +
  geom_text(data= df2 %>% filter(sports=="Archery", id==1),
            aes(x=-.6, y=-1.05, label="Men"), size=4, color="white", family=f1) +
  geom_text(data= df2 %>% filter(sports=="Archery", id==1),
            aes(x=-.6, y=1, label="Coed men"), size=4, color="white", family=f1) +
  geom_text(data= df2 %>% filter(sports=="Archery", id==2),
            aes(x=.6, y=-1.05, label="Women"), size=4, color="white", family=f1) +
  geom_text(data= df2 %>% filter(sports=="Archery", id==4),
            aes(x=.6, y=1, label="Coed women"), size=4, color="white", family=f1) +
  geom_text(aes(x=x,y=y, label=scales::percent(pct, scale=1, accuracy=.1), hjust=ifelse(id==1|id==3,0,0)), 
            size=3.8, lineheight=1,family=f1, show.legend=F, color="white") +
  scale_color_identity() +
  facet_wrap(~lab, ncol=2) +
  scale_fill_manual(values=c("#4C869F","#BC8400","#0C3660","#854E0D"),
                    labels=c("men","women", "coed_men", "coed_women")) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed() +
  cowplot::theme_map(12) +
  theme(text=element_text(family=f1),
        legend.position="none",
        plot.margin=margin(.5,0,.5,0,unit="cm"),
        strip.text=element_markdown(size=12),
        plot.caption=element_text(size=9, hjust=0, margin=margin(t=13)),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.title=element_text(size=16),
        plot.subtitle = element_markdown(size=11.2, margin=margin(b=8)),
        ) +
  labs(title="College sports participants (2015-2019)",
       subtitle="Proportion of <span style='color:#0C3660'>**coed men**</span>, <span style='color:#854E0D'>**coed women**</span>, <span style='color:#4C869F'>**men**</span> and <span style='color:#BC8400'>**women**</span> participants, by selected college sport.",
       caption="#30DayChartChallenge 01 part-to-whole  |  Data: Equity in Athletics from #TidyTuesday Week 13")


# Save plot
ggsave("30chart_04.png", bg="white", height=7, width=6)
  

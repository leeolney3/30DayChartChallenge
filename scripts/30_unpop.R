# 30DayChartChallenge 30 data day: UN Population
# Net Reproduction Rate, Estimates
# Data: https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Fertility/WPP2019_FERT_F05_NET_REPRODUCTION_RATE.xlsx

# Libraries
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Sans")
f2 = "IBM Plex Sans"

# Data
raw = read_csv("data/30_npr.csv") 

df = raw %>%
  rename(area=3)  %>%
  select(-Index, -Variant, -Notes, -Type, -`Parent code`) %>%
  pivot_longer(!1:2)

df$name = sub("\\s+$", "", gsub('(.{5})', '\\1 ', df$name))

# Plot 1, country-level
df %>% 
  ggplot(aes(name, value)) +
  #ggdist::stat_halfeye(fill=NA) +
  ggdist::stat_interval(.width = c(.25, .5, .95, 1), size = 1.5, 
                        position=position_nudge(x=-.05, y=0)) +
  ggdist::stat_dots() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5),
                   expand=c(0,0)) +
  scale_y_continuous(expand=c(0.02,0.02)) +
  scale_color_manual(values=MetBrewer::met.brewer("Egypt"),
                     labels = function(x) paste0(as.numeric(x)*100, "%")) +
  cowplot::theme_minimal_hgrid(14) +
  theme(text=element_text(family=f2),
        legend.position = "top",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.subtitle = element_text(lineheight = .9, size=11.2),
        plot.caption=element_text(size=10, hjust=0, color="grey30", margin=margin(t=15)),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=12),
        legend.margin=margin(b=-5, t=5),
        axis.title = element_blank(),
        axis.text.x=element_text(size=10.5),
        plot.margin=margin(.5,.5,.2,.3, unit="cm"),
        plot.background = element_rect(fill="#fafafa")
        ) +
  labs(caption="#30DayChartChallenge 30 data day: UN Population",
       color="Level:",
       title="Net Reproduction Rate, Estimates",
       subtitle="The average number of daughters a hypothetical cohort of women would have at the end of their reproductive\nperiod if they were subject during their whole lives to the fertility rates and the mortality rates of a given period.\nIt is expressed as number of daughters per woman.") +
  annotate(geom="richtext",label="**Niger** has the highest estimate<br>net reproduction rate at **2.942**<br>in the period 2015-2020", x=10.5, y=3.5, 
           family=f2, size=3.5, hjust=0,fill = NA, label.color = NA,) +
  annotate(geom="curve", x=13.4, xend=14.05, y=3.36, yend=2.98, curvature=-.3,  arrow=arrow(length=unit(0.20,"cm")))

ggsave("30_unpop_p1.png", height=6.5, width=8.5, unit="in")

# Plot 2, region/sub-region/country-level for Sub-Saharan Africa
font_add_google("IBM Plex Sans Condensed")
f1 = "IBM Plex Sans Condensed"

raw = read_csv("data/30_npr_ssa.csv") 
#colnames(raw)
df = raw %>% pivot_longer(8:21)
df$name = sub("\\s+$", "", gsub('(.{5})', '\\1 ', df$name))

df %>%
  ggplot() +
  geom_hline(yintercept=.5) +
  geom_point(data= df %>% filter(level=="c1"),aes(name, value), color="#D81A61", 
             alpha=.3, shape=95, size=10) +
  geom_point(data= df %>% filter(level=="c2"),aes(name, value), color="#1F88E5", 
             alpha=.3, shape=95, size=10) +
  geom_point(data= df %>% filter(level=="c3"),aes(name, value), color="#117733", 
             alpha=.3, shape=95, size=10) +
  geom_point(data= df %>% filter(level=="c4"),aes(name, value), color="#FFC108", 
             alpha=.3, shape=95, size=10) +
  geom_line(data= df %>% filter(level %in% c("b")), aes(name, value, group=group, color=group),
            size=1.5, alpha=.8) +
  scale_color_manual(values=c("#D81A61","#1F88E5","#117733","#FFC108")) +
  ggnewscale::new_scale_color() +
  geom_line(data= df %>% filter(level %in% c("a")), aes(name, value, group=group, color="Sub-Saharan Africa"), size=3, alpha=.7) +
  scale_color_manual(values="grey30") +
  scale_y_continuous(breaks=seq(0.5,3.5,.5), limits=c(.5,3.7), expand=c(0,0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  cowplot::theme_minimal_hgrid(13) +
  theme(legend.position = "top",
        plot.title.position = "plot",
        plot.subtitle=element_text(size=11, lineheight = .9),
        plot.title=element_text(face="plain"),
        plot.caption.position = "plot",
        text=element_text(family=f2),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_text(size=11),
        axis.text.x=element_text(family=f1, size=11),
        plot.margin=margin(.5,.5,.2,.3, unit="cm"),
        plot.background = element_rect(fill="#fafafa"),
        legend.text=element_text(family=f1,size=12),
        axis.ticks.length.x=unit(.3,"lines"),
        plot.caption=element_text(color="grey30",size=10, margin=margin(t=10)),
        legend.title=element_blank()) +
  guides(color=guide_legend(order=1)) +
  labs(title="Net Reproduction Rate, Estimates",
       subtitle="The average number of daughters a hypothetical cohort of women would have at the end of their reproductive\nperiod if they were subject during their whole lives to the fertility rates and the mortality rates of a given period.\nIt is expressed as number of daughters per woman.",
       caption="#30DayChartChallenge 30 data day: UN Population")
       
ggsave("30_unpop_p2.png",height=6, width=8, unit="in")
# 30DayChartChallenge 24 theme day: Financial Times
# Spider news
# Source: Stefano Mammola et al. by way of Data is Plural
# Dataset available at: https://figshare.com/articles/dataset/Global_Spider_News_Database/14822301

library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Noto Sans")
f1="Noto Sans"

# Data
df = read_csv("data/spider_news_global.csv") 

df %>% count(Country_search, sort=T)

# Wrangle
df1 = df %>% filter(yr>=2016, Country_search %in% c("UK","USA","Italy")) %>%
  select(yr, m, d, Country_search, Expert_arachnologist, Expert_doctor, Expert_others) %>%
  drop_na() %>%
  mutate(date= zoo::as.yearmon(paste(yr, m), "%Y %m")) %>%
  count(date, Country_search) %>%
  mutate(grp="Spider news")
  
df2 = df %>% filter(yr>=2016, Country_search %in% c("UK","USA","Italy")) %>%
  select(yr, m, d, Country_search, Expert_arachnologist, Expert_doctor, Expert_others) %>%
  drop_na() %>%
  mutate(grp=  Expert_arachnologist+ Expert_doctor+ Expert_others,
         grp= case_when(grp>=1 ~"Spider news containing expert-opinion", TRUE ~"Expert-opinion absent")) %>%
  mutate(date= zoo::as.yearmon(paste(yr, m), "%Y %m")) %>%
  count(Country_search,grp, date) %>%
  filter(grp=="Spider news containing expert-opinion") %>%
  select(date, Country_search, n, grp)
  
df3 = rbind(df1,df2) %>% filter(date<2020)

# Plot
p = df3 %>% 
  ggplot(aes(x=date, y=n)) +
  geom_line(aes(color=grp, size=grp)) +
  scale_size_manual(values=c(.7,.8)) +
  scale_x_continuous(labels=c("2016","17","18","19","20"), expand=c(0,0), limits=c(2016,2020)) +
  scale_y_continuous(expand=c(0.0,0.0), limits=c(0,51), breaks=seq(0,50,10)) +
  scale_color_manual(values=c("#EF6690","#0E5499")) +
  facet_wrap(~Country_search) +
  cowplot::theme_minimal_hgrid(12, color="#D4CABD") +
  theme(text=element_text(family=f1, color="#37332F"),
        legend.position = "none",
        legend.title=element_blank(),
        strip.text=element_text(face="bold"),
        plot.caption = element_text(size=8, hjust=0, lineheight = 1.1),
        plot.margin=margin(.75,.5,.5,.3,unit="cm"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.spacing = unit(2, "lines"),
        plot.background = element_rect(fill="#FEF1E5", color=NA),
        plot.title = element_markdown(margin=margin(b=9)),
        plot.subtitle = element_text(lineheight = 1.1),
        axis.text=element_text(color="#595048"),
        axis.line.x = element_line(color="#9E9489"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="#9E9489"),
        axis.ticks.length=unit(.2, "cm"),
        axis.title=element_blank()) +
  labs(title="Number of <span style='color:#EF6690'>Spider News</span> and <span style='color:#0E5499'>Spider news containing expert-opinion</span>",
       subtitle="Online newspaper reports published in Italy, UK and USA, from 2016 to 2020.\nExpert-opinion from arachnologist, medical professional or other experts e.g., pest controllers.",
       caption="\nSource: Mammola, S., Malumbres-Olarte, J., Arabesky, V. et al. An expert-curated global database of online newspaper articles on spiders and spider\nbites. Sci Data 9, 109 (2022). https://doi.org/10.1038/s41597-022-01197-6\n#30DayChartChallenge 24. theme day: Financial Times")
       
# Add tag (similar to method used in 12 theme day: The Economist)
png("24_financial-times.png", width=8, height=5,unit='in',res=300)

# function from https://stackoverflow.com/questions/64656234/how-does-the-economist-make-these-lines-near-the-title-using-using-ggplot
annotate_npc <- function(x, y, height, width, ...) {
  grid::grid.draw(grid::rectGrob(
    x = unit(x, "npc"), y = unit(y, "npc"), height = unit(height, "npc"), width = unit(width, "npc"),
    gp = grid::gpar(...)
  ))
}

p
annotate_npc(x = 0.067, y = 0.98, height = 0.009, width = 0.1, fill = "#000000", col = NA)

dev.off()
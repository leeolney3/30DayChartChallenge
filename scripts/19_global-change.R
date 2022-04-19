# 30DayChartChallenge (Timeseries) 19 global change
# WHO outbreak alerts by year
# Source: Colin J. Carlson et al. Github by way of Data is Plural
# Link to data: https://github.com/cghss/dons, https://www.data-is-plural.com/archive/2022-03-30-edition/
# Plot inspired by: The New York Times, by way of DataWrapper Data Vis Dispatch (https://blog.datawrapper.de/wp-content/uploads/2022/03/data-viz-dispatch-visualization44.png)

library(tidyverse)
library(magrittr)
library(lubridate)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Saira Semi Condensed")
f2 = "Saira Semi Condensed"

# Data
data = read_csv("https://raw.githubusercontent.com/cghss/dons/master/Data/DONdatabase.csv")

# Wrangle
# reference: https://github.com/cghss/dons/blob/master/Scripts/Visualization%2001%20-%20Over%20time.R
df1 = data %>% 
  mutate(YearEvent = year(mdy(ReportDate))) %>%
  group_by(DiseaseLevel1, YearEvent) %>%
  count() %>% ungroup %>% complete(DiseaseLevel1, YearEvent, fill = list(n = 0))
  
df2 = df1 %>%
  mutate(fill = case_when(DiseaseLevel1=="Influenza A"~"#F25F1A",
                          DiseaseLevel1=="Ebola virus"~"#7F62F1",
                          DiseaseLevel1=="MERS-CoV"~"#208583",
                          DiseaseLevel1=="Cholera"~"#EB3A8F",
                          DiseaseLevel1=="Meningococcal disease"~"#293CBF", 
                          DiseaseLevel1=="Yellow fever"~"#FFBD44",
                           TRUE~"grey90"))

lab = tribble(
  ~DiseaseLevel1,~YearEvent,~n,~color,
  "Influenza A",2008,55,"white",
  "Ebola\nvirus",2014,200,"white",
  "MERS-CoV",2014,60,"white",
  "Cholera",1998,100,"white",
  "Meningococcal\ndisease", 2000.5,37,"white",
  "Yellow fever",2001, 5,"black"
)

# Plot
df2 %>%
  ggplot(aes(x=YearEvent, y=n, group=DiseaseLevel1)) +
  geom_segment(
    data = tibble(y = seq(0, 300, by = 100), x1 = c(1995.5,1995.5,2002,2013), x2 = 2020.5),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",size = .4, family=f2) +
  geom_text(
    data = tibble(y = seq(5, 305, by = 100), x = rep(2020, 4), label=seq(0, 300, by = 100)),
    aes(x = x, y=y, label=label),
    inherit.aes = FALSE,
    color = "grey30", size=3.2, hjust=1, family=f2) +
  annotate(geom="text", x=2020, y=295, label="reports", color = "grey30", size=3.2, hjust=1,
           family=f2) +
  geom_area(aes(fill=fill), color="grey60",size=.1) +
  geom_text(data=lab, aes(label=DiseaseLevel1, color=color,), 
            size=3, lineheight=.85, family=f2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(breaks=c(1996,2019,2000,2005,2010,2015)) +
  coord_cartesian(expand=F) +
  theme_minimal(11) +
  theme(panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_line(color="grey91", size=.4),
        axis.ticks.length=unit(.25, "cm"),
        text=element_text(family=f2),
        plot.margin=margin(.5,.3,.3,.1,unit="cm"),
        plot.caption=element_text(hjust=0.03, size=8.5, margin=margin(t=15))
        ) +
  annotate(geom="text", x=1996, y=333, hjust=0, vjust=1,family=f2, 
           label="Number of WHO outbreak alerts", size=5.5) +
  annotate(geom="text", x=1996, y=315, hjust=0, vjust=1, family=f2, size=3.2,
           label="by year, includes 2,700+ reports published between January 1996\nand December 2019. There were 335 reports in 2014, of which 175\n(52%) are Ebola virus reports.") +
  labs(caption="#30DayChartChallenge (Timeseries) 19 global change  |  Data source: Colin J. Carlson et al. Github, by way of Data is Plural")
  
ggsave("19_global-change.png", bg="white", height=6, width=8)
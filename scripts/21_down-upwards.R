# 30DayChartChallenge 21 down/upwards
# Data source: U.S. Department of Energy, by way of Data is Plural, available at: https://afdc.energy.gov/data/10581

# Libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Serif")
f1="IBM Plex Serif"

# Data: paste using {datapasta}
df = tibble::tribble(
                ~alt, ~`2004`, ~`2005`, ~`2006`, ~`2007`, ~`2008`, ~`2009`, ~`2010`, ~`2011`, ~`2012`, ~`2013`, ~`2014`, ~`2015`,   ~`2016`, ~`2017`, ~`2018`,   ~`2019`,   ~`2020`,
               "E85",   47643,   72899,  221834,  385671,  524169,  642520,  403981,  479706,  197187,  259337,  275508,  271996,    457817,  327865,  263967,    275216,    268047,
              "HEVs",   10674,   18553,   43886,   81987,  101954,  107585,   30613,   50503,  141406,   83339,   82834,   94330,    129827,  178011,  178733,    144875,    157540,
         "Biodiesel",   31922,   52275,   91584,   98785,   17222,    2814,   88726,   99347,  103106,   98028,  130540,  189823,    160763,  150695,  153380,    163366,    203839,
               "CNG",   76257,   49271,   57458,   55021,   51121,   44317,   42911,   48157,   59521,   79616,   68479,  107283,     98388,   97271,   82266,    100938,     87392,
           "Propane",   31338,   21117,   25543,   23628,   22260,    7937,   13196,   18793,   16501,   35554,   17404,   22762,     23648,   34753,   25395,     31985,     30156,
               "LNG",       0,    1873,    2271,    1731,    2053,    2038,    3410,    4315,    3411,    3645,    2992,    3974,      4924,    5070,    5100,      4917,      4510,
          "Hydrogen",      23,      42,      72,      86,      75,      74,      62,     118,      50,      46,      49,      95,        97,     116,     402,       691,       413,
              "PEVs",    9241,    7464,    9481,    5895,   14135,    4568,    9223,   10541,   20455,   32187,   65042,   96896,    171011,  162811,  227854,    263543,    234277,
               "RNG",       0,       0,       0,       0,       0,       0,       0,       0,       0,       0,     313,     366,      1157,    1734,    1677,      4922,      7229,
  "Renewable Diesel",       0,       0,       0,       0,       0,       0,       0,       0,       0,       0,       0,    3183,     71066,    7819,   11174,     13109,     11807,
             "Total",  207098,  223494,  452129,  652804,  732989,  811853,  592122,  711480,  541637,  591752,  643161,  790708,   1118698,  966145,  949948,   1003562,   1005210
  )

# Wrangle
df1 = df %>% 
  filter(alt!="Total") %>% 
  pivot_longer(2:18) %>%
  filter(alt %in% c("E85","HEVs","PEVs","Propane")) %>%
  group_by(alt) %>%
  mutate(diff = (value/lag(value) - 1)) %>%
  filter(!is.na(diff)) %>%
  mutate(col=case_when(diff<0~"Decrease", diff>0~"Increase"),
         name=parse_number(name))
         
# Plot
ggplot(aes(x=name, y=diff)) +
  geom_hline(yintercept=0, size=.3) +
  geom_segment(aes(y=0, yend=diff, x=name, xend=name, color=col), 
               arrow = arrow(type = "closed", length = unit(1.5, "mm")), 
               size=1, key_glyph = draw_key_rect) +
  scale_y_continuous(breaks=seq(-.5,2,.5), labels=scales::percent_format()) +
  scale_color_manual(values=c("#D68E24","#139E56")) +
  facet_wrap(~alt, scales="free_x") +
  cowplot::theme_minimal_hgrid(11) +
  theme(legend.position="top",
        text=element_text(family=f1),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, size=14.5),
        plot.subtitle=element_text(hjust=.5, size=10),
        legend.title=element_text(size=10.5),
        legend.text=element_text(size=10.5),
        legend.justification = "center",
        strip.text=element_text(size=11.5, face="bold"),
        axis.title=element_blank(),
        plot.margin=margin(.5,.75,.5,.5,unit="cm"),
        plot.background = element_rect(fill="#fafafa", color=NA),
        axis.ticks.length=unit(.25, "cm"),
        ) +
  labs(caption="\n#30DayChartChallenge |  Data: U.S. Department of Energy, by way of Data is Plural",
       title="Change in green cities alternative fuel vehicles inventory",
       subtitle="E85, HEVs, PEVs and Propane alternative fuel vehicles across U.S., from 2005 to 2020",
       color="% change from previous year") +
  guides(color=guide_legend(reverse=T))
  
ggsave("21_down-upwards.png", height=7, width=7)

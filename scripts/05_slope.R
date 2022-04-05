# 30DayChartChallenge 05 slope
# Malaria tests in sub-Saharan Africa, November 2019 to March 2021
# Data source: World Health Organization (World malaria report 2021 pp.19) available at https://apps.who.int/iris/rest/bitstreams/1398397/retrieveiris/rest/bitstreams/1398397/retrieve

# Libraries 
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Lato")
font_add_google("Dosis","dosis")

# Data
data = tibble::tribble(
                            ~country,    ~a1,    ~a2,     ~b1,     ~b2,
              "Burkina Faso Burundi", 58129L, 77081L,  40498L,  42598L,
                           "Burundi", 25486L, 23380L,  37637L,  31019L,
                          "Cameroon",  6653L,  6841L,   7213L,   7733L,
          "Central African Republic", 46150L, 62067L,  59096L,  50458L,
                              "Chad", 29484L, 24467L,  21777L,  13869L,
  "Democratic Republic of the Congo", 28317L, 30816L,  29531L,  28041L,
                    "Côte d'Ivoire", 38000L, 28081L,  26534L,  32427L,
                          "Ethiopia",  4581L,  2052L,   2211L,   1479L,
                             "Ghana", 20520L, 19790L,  10292L,  11970L,
                            "Guinea", 53694L, 47020L,  41308L,  30357L,
                             "Kenya", 10126L, 18002L,  12938L,   9018L,
                        "Madagascar",  2052L,  1676L,   3955L,   2964L,
                            "Malawi", 75235L, 85694L, 117184L, 126472L,
                              "Mali", 46299L, 35636L,  21913L,  20812L,
                        "Mozambique", 41444L, 21229L,  35751L,  56465L,
                             "Niger", 33929L, 22438L,  10956L,  13295L,
                           "Nigeria", 36495L, 27277L,  31157L,  20092L,
                            "Rwanda", 31293L, 21778L,  38086L,  20086L,
                      "Sierra Leone", 12178L, 13817L,  10606L,  14934L,
                              "Togo", 15536L, 12057L,  11615L,   9340L,
       "United Republic of Tanzania", 43011L, 43032L,  47659L,  40748L,
                            "Zambia", 10253L, 15147L,  29406L,  15996L,
                          "Zimbabwe",  2767L,  1958L,   5495L,   1614L
  )

# Wrangle
d1 = data %>% 
  select(country, `2019`=a1, `2020`=a2) %>%
  mutate(col=case_when(`2020`>`2019`~"Increase",
                       TRUE~"Decrease")) %>%
  pivot_longer(`2019`:`2020`) %>%
  mutate(grp = "November-December")

d2 = data %>% 
  select(country, `2020`=b1, `2021`=b2) %>%
  mutate(col=case_when(`2021`>`2020`~"Increase",
                       TRUE~"Decrease")) %>%
  pivot_longer(`2020`:`2021`) %>%
  mutate(grp = "January-March")

d3 = bind_rows(d1,d2)  
  
# Plot  
d3 %>%
  ggplot(aes(x=name, y=value, group=country)) +
  geom_line(aes(color=col),size=.55, key_glyph = draw_key_rect) +
  geom_text_repel(data=d3 %>% filter(grp=="November-December", name==2020, col=="Increase"),
            aes(label=country), size=3, family="dosis",color="#5D3A9B",box.padding = 0.1,
            direction="y", xlim = c(2.03, NA),segment.alpha = .5,segment.linetype = "dotted") +
  geom_text_repel(data=d3 %>% filter(grp=="January-March", name==2021, col=="Increase"),
            aes(label=country), size=3, family="dosis",color="#5D3A9B",box.padding = 0.1,
            direction="y", xlim = c(2.02, NA),segment.alpha = .5,segment.linetype = "dotted") +
  facet_wrap(~fct_rev(grp), ncol=2, scales="free_x") +
  scale_y_continuous(labels=scales::label_number_si(),
                     limits=c(0,127000),
                     breaks=seq(0,127000,25000)
                     ) +
  scale_color_manual(values=c("#E66100","#5D3A9B"),
                     labels=c("Decrease from previous year","Increase from previous year"),
                     guide=guide_legend(reverse=T, keyheight = unit(0.01, "lines"))) +
  scale_x_discrete(expand = expansion(mult = c(.15, 1))) +
  theme_minimal(13) +
  theme(text=element_text(family="dosis"),
        plot.margin=margin(.5,.2,.3,.5, unit="cm"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(linetype="dotted", color="grey", size=.3),
        plot.title.position = "plot",
        plot.title=element_text(family="Lato", face="bold"),
        plot.subtitle=element_text(size=11.5),
        axis.title=element_blank(),
        legend.position="top",
        legend.title=element_blank(),
        legend.margin=margin(b=-5, l=-30),
        legend.justification = "left",
        strip.text = element_text(face="bold", size=11, hjust=.17),
        plot.caption=element_text(size=8, color="grey30", hjust=0, family="Lato", margin=margin(t=12)),
        plot.caption.position="plot",
        axis.text.x=element_text(face="bold")
        ) +
  labs(title="Malaria tests in sub-Saharan Africa",subtitle="Differences in the number of malaria tests performed across similar periods using data from selected health facilities in\n23 malaria endemic countries in sub-Saharan Africa, November 2019 to March 2021.",
       caption="#30DayChartChallenge 05 Slope  |  Source: World Health Organization")
       
# Save plot
ggsave("05_slope.png", height=6.4, width=8, bg="white")
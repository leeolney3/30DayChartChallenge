# 30DayChartChallenge (relationship) 13. correlation
# Spurious correlation example from https://www.tylervigen.com/spurious-correlations
# Inspired by: @KittJonathan, @DigestData and @pablo_alvrez

library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Barlow Semi Condensed")
f1 = "Barlow Semi Condensed"

font_add_google("Roboto")
f2 = "Roboto"

# Data
df= tibble(
       x=seq(2000,2009,1), 
       "**Per capita consumption of mozzarella cheese**<br>(lbs)"=c(9.3,9.7,9.7,9.7,9.9,10.2,10.5,11,10.6,10.6),
       "**Civil engineering doctorates awarded**<br>(degrees)"=c(480,501,540,552,547,622,655,701,712,708) 
       ) %>%
   pivot_longer(2:3) 


# Plot
df %>% 
  ggplot(aes(x, value, color=name)) +
  geom_line(size=.8,show.legend=F) +
  geom_point(size=2,show.legend=F) +
  facet_wrap(~fct_rev(name), scales="free") +
  scale_color_manual(values=MetBrewer::met.brewer("Kandinsky",2)) +
  scale_y_continuous(expand=c(.1,.1)) +
  scale_x_continuous(breaks=seq(2000,2009,1),
                     labels=c("2000","","","2003","","","2006","","","2009")) +
  cowplot::theme_minimal_hgrid(14) +
  theme(text=element_text(family=f1),
        axis.line.x = element_line(color="#212529"),
        axis.ticks.x=element_line(color="#212529"),
        axis.ticks.length.x = unit(.25, "cm"),
        strip.text=element_markdown(size=12.9, lineheight = 1.1),
        axis.title=element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_markdown(family=f2),
        plot.subtitle = element_text(family=f2, margin=margin(b=15), lineheight = 1.2, size=10.5),
        plot.caption=element_text(family=f2, margin=margin(t=15), size=10, color="#37363A")
        ) +
  labs(title="Spurious correlation: <span style='color:#DB9125'>Mozzarella cheese</span> and <span style='color:#0A7C6F'>civil engineering doctorates</span>",
       subtitle="Example from: https://www.tylervigen.com/spurious-correlations\nCorrelation: 95.86% (r=0.958648)",
       caption="#30DayChartChallenge (relationship) 13. correlation")
       
ggsave("13_correlation.png", height=6, width=8, bg="white")
# 30DayChartChallenge 12 theme day: The Economist
# Teenagers on TV
# Data: Amber Thomas by way of Data is Plural, available at https://data.world/amberthomas/age-of-characters-and-actors-in-teen-tv-shows

library(tidyverse)
library(magrittr)
library(lubridate)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Roboto")
f1 = "Roboto"

# Data
df <- read.csv("https://query.data.world/s/qy3au2i62neui5gpiy7zutvd5cchm4", header=TRUE, stringsAsFactors=FALSE)

df1 = df %>%
  filter(!is.na(character_age)) %>%
  filter(between(character_age, 14, 17))
  
dim(df1)
  
# Plot

p = df1 %>%
  ggplot(aes(x=age_difference, y=character_age, 
             color=factor(character_age), fill=factor(character_age))) +
  geom_vline(xintercept=c(0, 4,8,12,16), size=.4, color="#ACBABD") +
  geom_segment(
    data = tibble(y = seq(15, 17, by = 1), x1 = -4, x2 = 17),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,color = "#ACBABD",size = .4
  ) +
   geom_segment(
    data = tibble(y = 14, x1 = -4, x2 = 17),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,color = "black",size = .5
  ) + 
   geom_text(
    data = tibble(y = seq(14.1, 17.1, by = 1), x = -4, label = seq(14, 17, by = 1)),
    aes(x = x, y = y, label=label),
    inherit.aes = FALSE,color = "#0C0C0C",size = 4.5, hjust=0, family=f1, fontface="bold"
  ) +
  annotate(geom="text", x=-4, y= 17.65, label=c("Character\nage"), 
           hjust=0, family=f1, fontface="bold", size=4.5, lineheight=.8, color="#0C0C0C") +
  ggdist::stat_dots() +
  scale_x_continuous(breaks=seq(-2,16,2), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("#008C90","#D6AC63","#3DB8C2","#17658E")) +
  scale_color_manual(values=c("#008C90","#D6AC63","#3DB8C2","#17658E")) +
  theme_minimal(base_family=f1) +
  theme(legend.position = "none",
        text=element_text(color="#0C0C0C"),
        plot.margin=margin(.75,.6,.5,.6, unit="cm"),
        plot.title.position = "plot",
        panel.grid=element_blank(),
        axis.ticks.x = element_line(color="black", size=.5),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length.x=unit(.25, "cm"),
        axis.title.x=element_text(margin=margin(t=5), face="bold", size=13),
        axis.text.x=element_text(size=13),
        plot.title=element_text(face="bold", size=20),
        plot.subtitle=element_text(color="black", size=12.5, lineheight = 1.1, margin=margin(b=15)),
        #plot.background = element_rect(fill="#DEEAEC", color=NA),
        plot.caption.position = "plot",
        plot.caption=element_text(size=11.5, color="#4B4B4B",lineheight=1,hjust=0, margin=margin(t=13)),
        ) +
  labs(title="Teenagers on TV",
       subtitle="Age differences between teen characters (age 14 to 17) in TV shows and the actors\nwho portray them. 218 characters in 33 series that premiered between 2000 and 2021.",
       x="Age difference",
       caption="Source: Amber Thomas by way of Data is Plural\n#30DayChartChallenge distribution 12 theme day: The Economist")
       
       
# Add red tag
png("12_economist.png", width=7, height=7,unit='in',res=300)

# function from https://stackoverflow.com/questions/64656234/how-does-the-economist-make-these-lines-near-the-title-using-using-ggplot
annotate_npc <- function(x, y, height, width, ...) {
  grid::grid.draw(grid::rectGrob(
    x = unit(x, "npc"), y = unit(y, "npc"), height = unit(height, "npc"), width = unit(width, "npc"),
    gp = grid::gpar(...)
  ))
}

p
annotate_npc(x = 0.07, y = 1, height = 0.05, width = 0.07, fill = "#D8232A", col = NA)

dev.off()
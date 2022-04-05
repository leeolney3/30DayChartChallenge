# 30DayChartChallenge 04 flora
# Count of Midwest native plant species by environment 
# Data source: grownative.org (https://grownative.org/native-plant-database/)
# Petal chart method: Jake Kaupp @jakekaupp (https://twitter.com/jakekaupp/status/1380214121711435776/photo/1)

# Libraries
library(tidyverse)
library(ggforce)
library(MetBrewer)
library(cowplot)
library(showtext)

# Font
library(showtext)
font_add_google("Fira Sans Condensed")
font_add_google("Saira Semi Condensed")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

f1="Fira Sans Condensed"
f2="Saira Semi Condensed"

# Data
df = tribble(
  ~environment, ~n, 
  "Cliff",12,
  "Forest",100,
  "Glade",52,
  "Prairie",97,
  "Savanna/Woodland",85,
  "Stream Edge",63,
  "Wetland",60
)

df1 = df %>%
  mutate(id=row_number(),
         angle=51.4285*id,
         radians=angle*pi/180,
         x0=n*cos(radians),
         y0=n*sin(radians))

# Colors
met.brewer("Archambault", 7)

# Plot
p = df1 %>%
ggplot(aes(x=environment, y=n, fill=fct_rev(environment))) +
  geom_ellipse(aes(x0=x0, y0=y0, a=n,b=n/2,angle = radians, fill = fct_rev(environment)),
               color="white") +
  scale_fill_manual(values= rev(c("#381a61", "#88a0dc", "#7c4b73", "#ed968c", "#ab3329", "#e78429", "#f9d14a"))) +
  coord_fixed(clip="off") +
  cowplot::theme_map(15) +
  theme(legend.position="none",
        plot.margin=margin(.2,0,.5,0,unit="cm"))
        
final = p+ 
  draw_label("Forest\n100", x=-20, y=100, fontfamily = f1, color="white", size=13) +
  draw_label("Savanna/\nWoodland\n85", x=-20, y=-100, fontfamily = f1, color="white",size=13) +
  draw_label("Wetland\n60", x=70, y=-0, fontfamily = f1, color="black",size=13) +
  draw_label("Stream\nEdge\n63", x=50, y=-60, fontfamily = f1, color="black",size=13) +
  draw_label("Prairie\n97", x=-80, y=-40, fontfamily = f1, color="black",size=13) +
  draw_label("Glade\n52", x=-70, y=30, fontfamily = f1, color="white",size=13) +
  draw_label("Cliff\n12", x=55, y=50, fontfamily = f1, color="#381a61",size=13) +
  draw_line(x=c(10,45), y=c(10,50), color="#381a61", size=.7) +
  draw_label("Lower Midwest\nNative Plants", 
             x=-220, y=177, hjust=0, fontfamily=f2, fontface="bold", size=23.5) +
  draw_label("Count of native plant species in the\nlower Midwest by environment,\nincludes trees, shrubs, vines, grasses,\nsedges, rushes, and wildflowers.",x=-220, y=130, hjust=0, fontfamily=f2, size=10.5, lineheight = 1) +
  draw_label("#30DayChartChallenge 04 flora | Data from grownative.org | Method from Jake Kaupp @jakekaupp", fontfamily=f2, size=9, x=-220, y=-180, hjust=0, color="#212529")
  
# Save plot
save_plot("04_flora.png", final, bg="white", base_asp = 1, base_height = 7)


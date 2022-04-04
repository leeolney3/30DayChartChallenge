# 30DayChartChallenge 02 pictogram
# Estimated % of threatened species in 2021 (Mammals, Birds, Amphibians, Gymnosperms)
# Data source: iucnredlist.org (https://nc.iucnredlist.org/redlist/content/attachment_files/2021-3_RL_Stats_Table_1a_v2.pdf)

library(tidyverse)
library(reactablefmtr)

table1 = tribble(
  ~"Birds",~Mammals,~Amphibians,~Gymnosperms,
  13, 26, 41, 41
) %>%
  reactable(
  .,
  theme=default(header_font_size = 17.5),
  defaultColDef = colDef(align = "left", maxWidth = 180), 
  columns = list(
    Birds = colDef(maxWidth=150, cell = icon_assign(., icon = "dove", icon_size = 25, fill_color = "#343a40")),
    Mammals = colDef(cell = icon_assign(., icon = "hippo", fill_color = "#343a40",icon_size = 25)),
    Amphibians= colDef(maxWidth=170,cell = icon_assign(., icon = "frog", icon_size = 25, fill_color = "#343a40")),
    Gymnosperms= colDef(maxWidth=135,cell = icon_assign(., icon = "envira", icon_size = 25,fill_color = "#343a40"))
  )
) %>%
  google_font(font_family = "Dosis") %>%
  add_title("Estimated % of threatened species in 2021", font_size = 21) %>% 
  add_subtitle(html("Threatened spp. as % of extant data sufficient evaluated species by major groups of organisms, according to<br> <b>IUCN Red List version 2021-3</b>, where one icon represents 1% of the major group.<br><br>"), font_weight = "normal", font_size = 15) %>% 
  add_source(html("<br>Note: Threatened species are those listed as Critically Endangered (CR), Endangered (EN) or Vulnerable (VU). The number of described and<br>evaluated mammals excludes domesticated species like sheep (Ovis aries ), goats (Capra hircus ), Dromedary (Camelus dromedarius ), etc.<br>#30DayChart Challenge 02 pictogram | Data source: iucnredlist.org"), font_size = 12)

table1
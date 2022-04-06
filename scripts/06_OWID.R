#30DayChartChallenge 06 data day OWID
# South America Cereal Yield (t/ha)
# Data source: OWID and FAO, available at https://ourworldindata.org/africa-yields-problem

library(tidyverse)
library(ggtext)
library(countrycode)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Fira Sans Condensed")
f1="Fira Sans Condensed"

# Data
data = read_csv("data/global-food.csv")

# Countries df
df1 = data %>%
  mutate(iso3c= countrycode(Country, origin = "country.name", destination="iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  mutate(region= countrycode(iso3c, origin="iso3c", destination="un.regionintermediate.name")) %>%
  filter(region=="South America") %>%
  janitor::clean_names()
  
# World and region df
df2 = data %>% 
  filter(Country=="World"|Country=="South America") %>% 
  janitor::clean_names() %>%
  mutate(country=case_when(country=="World"~"**World**",
         country=="South America"~"**South America**"))

# Levels (country)
df3 = bind_rows(df1, df2) 

lev = df3 %>% 
  group_by(country) %>%
  filter(year==max(year)) %>%
  ungroup() %>%
  arrange(desc(yield_t_ha)) %>%
  mutate(country=fct_inorder(country),
         country=fct_relevel(country,"**World**",after=0),
         country=fct_relevel(country,"**South America**",after=1),
         country=fct_relevel(country,"French Guiana",after=Inf)
         )%>%
  arrange(country) %>%
  pull(country)
  
# Plot
df3 %>%
  ggplot(aes(x=year, y=factor(country, level=rev(lev)), fill=yield_t_ha)) +
  geom_tile(height=.7, alpha=.9) +
  #scale_fill_gradientn(colors=wesanderson::wes_palette("Zissou1",100, type="continuous")) +
  scico::scale_fill_scico(palette="bamako", direction=-1, breaks=c(0.75,2,4,6,7.15), limits=c(0.75, NA)) +
  scale_x_continuous(breaks=c(1961,2019,1980,2000), position="top") +
  coord_cartesian(expand=F) +
  cowplot::theme_minimal_grid(13.5) +
  theme(text=element_text(family=f1),
        legend.position = "top",
        axis.title=element_blank(),
        plot.margin=margin(.5,1,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(face="bold"),
        panel.grid=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=10.5),
        legend.margin=margin(l=-70),
        plot.subtitle=element_text(size=10.5),
        plot.caption.position = "plot",
        plot.caption=element_text(size=9, color="grey30", hjust=0, lineheight = 1),
        axis.text.x.top = element_text(color="grey30", size=11),
        axis.text.y=element_markdown(size=11)
        ) +
  labs(title="Cereal Yield in South America (1961 - 2019)",
       subtitle="Tonnes per hectare (t/ha). Countries arranged in descending order of 2019 yield.",
       caption="\n#30DayChartChallenge 06 data day OWID | Data source: OWID and FAO\nNote: Cereals include wheat, rice, maize, barley, oats, rye, millet, sorghum, buckwheat, and mixed grains.") +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, barwidth = unit(15, "lines"), barheight = unit(.5, "lines")))
  
ggsave("06_OWID.png", bg="white")

# Version 2: Maize yield 
# h/t to Benjamin Nowak @BjnNowak for suggesting using Maize instead of cereal mix, as here is important productivity diff between species
# Maize data source: https://ourworldindata.org/explorers/global-food?tab=chart&facet=none&hideControls=true&Food=Maize+%28Corn%29&Metric=Yield&Per+Capita=false&country=OWID_WRL~IND~CHN~Africa~USA~GBR~BRA~TCD~NER~BEL~IRL~NLD

data = read_csv("data/global-food_maize.csv")

df1 = data %>%
  mutate(iso3c= countrycode(Country, origin = "country.name", destination="iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  mutate(region= countrycode(iso3c, origin="iso3c", destination="un.regionintermediate.name")) %>%
  filter(region=="South America") %>%
  janitor::clean_names()

df2 = data %>% 
  filter(Country=="World"|Country=="South America") %>% 
  janitor::clean_names() %>%
  mutate(country=case_when(country=="World"~"**World**",
         country=="South America"~"**South America**"))

df3 = bind_rows(df1, df2) 

lev = df3 %>% 
  group_by(country) %>%
  filter(year==max(year)) %>%
  ungroup() %>%
  arrange(desc(yield_t_ha)) %>%
  mutate(country=fct_inorder(country),
         country=fct_relevel(country,"**World**",after=0),
         country=fct_relevel(country,"**South America**",after=1),
         country=fct_relevel(country,"French Guiana",after=Inf)
         )%>%
  arrange(country) %>%
  pull(country)
  
df3 %>%
  filter(!is.na(yield_t_ha)) %>%
  ggplot(aes(x=year, y=factor(country, level=rev(lev)), fill=yield_t_ha)) +
  geom_tile(height=.7, alpha=1) +
  #scale_fill_gradientn(colors=wesanderson::wes_palette("Zissou1",100, type="continuous")) +
  scico::scale_fill_scico(palette="lajolla", direction=1) +
  scale_x_continuous(breaks=c(1961,2019,1980,2000), position="top") +
  coord_cartesian(expand=F) +
  cowplot::theme_minimal_grid(13.5) +
  theme(text=element_text(family=f1),
        legend.position = "top",
        axis.title=element_blank(),
        plot.margin=margin(.5,1,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(face="bold"),
        panel.grid=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=10.5),
        legend.margin=margin(l=-79),
        plot.subtitle=element_text(size=10.5),
        plot.caption.position = "plot",
        plot.caption=element_text(size=9, color="grey30", hjust=0, lineheight = 1),
        axis.text.x.top = element_text(color="grey30", size=11),
        axis.text.y=element_markdown(size=11)
        ) +
  labs(title="Maize Yield in South America (1961 - 2019)",
       subtitle="Tonnes per hectare (t/ha). Countries arranged in descending order of 2019 yield.",
       caption="\n#30DayChartChallenge 06 data day OWID | Data source: OWID and FAO | h/t to @BjnNowak for") +
  guides(fill = guide_colorbar(label.position = "top", title.hjust = .6, barwidth = unit(15, "lines"), barheight = unit(.5, "lines")))
  
ggsave("06_OWID_maize.png", bg="white")

# 30DayChartChallenge 10 Experimental
# Belgium gas storage from from January 1, 2011 to March 31, 2022
# Data source: Gas Infrastructure Europe, by way of Data Is Plural
# Dataset link: https://agsi.gie.eu/#/graphs/BE

# Libraries
library(tidyverse)
library(spiralize)
library(ComplexHeatmap)
library(showtext)

# Font
font_add_google("Fira Sans Condensed")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import data
df = read_csv("data/gas_storage_bel.csv") %>% janitor::clean_names()
summary(df)
range(df$gas_day_started_on)

# Plot
# reference: https://jokergoo.github.io/spiralize_vignettes/spiralize.html
# reference: https://twitter.com/nrennie35/status/1468263492838961152
# reference: https://twitter.com/drkeithyoder/status/1508944213853057030

png('10_experimental.png', width=8.5, height=7, units = 'in', res=300)

# layout 
grid.newpage()

# viewport with 3 rows
top.vp <-
  viewport(layout = grid.layout(3, 1,
                                heights=unit(c(4, 1, 2), c('lines', 'null', 'lines'))))
titlevp = viewport(layout.pos.col=1, layout.pos.row=1, name='titlevp')
plotvp = viewport(layout.pos.col=1, layout.pos.row=2, name='plotvp')
capvp = viewport(layout.pos.col=1, layout.pos.row=3, name='capvp')
splot = vpTree(top.vp, vpList(titlevp, plotvp, capvp))
pushViewport(splot)

# title and subtitle   
seekViewport('titlevp')
grid.text("Belgium Gas Storage", 
          x=unit(0.5, 'npc'), y = unit(.75, 'npc') - unit(.9, 'lines'),
          gp = gpar(fontsize=18, fontfamily="Fira Sans Condensed",fontface='bold', hjust=.5))
grid.text("Country-level amount of gas storage (TWh) by month of year, from January 1, 2011 to March 31, 2022", 
          x=unit(0.5, 'npc'), y = unit(.5, 'npc') - unit(2, 'lines'),
          gp = gpar(fontsize=10.7, hjust=.5, fontfamily="Fira Sans Condensed"))

# spiral plot
seekViewport('plotvp')
spiral_initialize_by_time(xlim = range(df$gas_day_started_on), unit_on_axis = "month", period = "year",
                          period_per_loop = 1,padding = unit(2, "cm"),normalize_year = TRUE,
                          start = 90, clockwise=TRUE,newpage = FALSE)

spiral_track(height=0.8) 
spiral_horizon(df$gas_day_started_on, df$gas_in_storage_t_wh, use_bars = TRUE, pos_fill = "#ca6702")

# month labels
s = current_spiral()
d = (seq(15, 360, by = 30) + 60) %% 360
for(i in seq_along((d))) {
  if (i == 1) {
    foo = polar_to_cartesian(d[i]/180*pi, (s$max_radius + 1)*1.01)
    grid.text(month.name[i], x = foo[1, 1], y = foo[1, 2], default.unit = "native",
        rot = ifelse(d[i] > 0 & d[i] < 180, d[i] - 90, d[i] + 90), gp = gpar(fontsize = 9.5, fontfamily="Fira Sans Condensed"))
  } else {
    foo = polar_to_cartesian(d[i]/180*pi, (s$max_radius + 1)*1.01)
    grid.text(month.name[14-i], x = foo[1, 1], y = foo[1, 2], default.unit = "native",
        rot = ifelse(d[i] > 0 & d[i] < 180, d[i] - 90, d[i] + 90), gp = gpar(fontsize = 9.5,fontfamily="Fira Sans Condensed"))
  }
    
}

# year labels
spiral_text(as_date(str_c(2011:2022, '-01-1')),
            .5, c(2011:2022), facing='downward',
            gp = gpar(fontsize=8.6, col='black', fontfamily="Fira Sans Condensed"))

# legend 
lgd = horizon_legend(lt, title = "Gas in Storage (TWh)", 
                     labels_gp = gpar(fontsize = 10,fontfamily="Fira Sans Condensed"), 
                     title_gp=gpar(fontsize = 10,fontfamily="Fira Sans Condensed", fontface="bold"))
draw(lgd, x = unit(1, "npc") + unit(5, "mm"), just = "left")

# caption
seekViewport('capvp')
grid.text('#30DayChartChallenge 10 Experimental | Data source: Gas Infrastructure Europe, by way of Data is Plural',
          x = unit(.68, 'npc'),
          gp = gpar(fontsize=8.5, fontfamily="Fira Sans Condensed", hjust=1))
upViewport(0)
dev.off()



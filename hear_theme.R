require('ggplot2')

#require('extrafont')
#loadfonts(quiet = TRUE)

hear_theme <- theme(
  #dark gray grid lines with light background
  panel.grid.major = element_blank(),
  panel.background = element_rect(fill = '#55555f'),
  
  #remove tickmarks and minor grid lines
  axis.ticks = element_blank(),
  panel.grid.minor = element_blank(),
  
  #add padding to chart and make background light gray
  plot.margin = unit(c(15, 15, 15, 15), 'point'),
  plot.background = element_rect(fill = '#55555f'),
  
  #set axis text
  axis.text = element_text(color = '#444444', size = 14),

  axis.title = element_text(size = 14, face='bold'),
  
  #position titles and labels
  plot.title = element_text(color = '#444444', hjust = 0, margin = margin(0,0,20,0), size = 23, face='bold'),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  
  #legend styling
  legend.background = element_rect(fill = NA, color = '#999999'),
  legend.key = element_rect(fill = NA ,color = NA),
  legend.margin = unit(15, 'pt'),
  legend.position = 'none'
)

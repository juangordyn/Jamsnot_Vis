library('shiny')
library('ggplot2')
library('plotly')
library('gganimate')
library('gifski')
library('ggimage')
library('dplyr')

fines_df <- read.csv('fines_df.csv')
fines_df$hour <- as.character(fines_df$hour)
fines_df$hour <- gsub(':00', '', fines_df$hour)
fines_df$hour <- as.numeric(fines_df$hour)
fines_df$fine_prob <- round(fines_df$fine_prob, 2)
fines_df$annotation_text <- gsub('tuvieja','\n',fines_df$annotation_text)
fines_df$annotation_text

plot_fines<- ggplot(data=fines_df, aes(x = hour, y = fine_prob, group = 1,)) + geom_line() +
  scale_x_continuous(name = 'Hour of Day', breaks = seq(5,20,1)) +
  labs(y='Fine probability (%)') + theme(plot.title = element_text(face ='bold'), legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle('Fine probability vs Hour of Day') +   scale_y_continuous(breaks=seq(0,25, 1)) +
  geom_text(aes(label=annotation_text, x=x, y=y, fontface='bold',colour='red')) +
  transition_manual(hour, cumulative =TRUE)
animate(plot_fines, duration = 25, end_pause=10)

anim_save("fines_animation.gif", animate(plot_fines, duration = 25, end_pause=10))
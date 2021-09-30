library('shiny')
library('ggplot2')
library('plotly')
library('gganimate')
library('gifski')
library('ggimage')
library('dplyr')

cost_df <- read.csv('parking_cost.csv')
cost_df$maximum_stay <- as.factor(cost_df$maximum_stay)
cost_df
plot_cost <- ggplot(cost_df, aes(x = maximum_stay, y = avg_cost_of_stay, fill = maximum_stay, group = 1)) + geom_bar(stat = 'identity', colour = 'black') +
  scale_fill_manual(values = c("#76B756","yellow","orange", "#F64F4C",'red')) + 
  scale_y_continuous(breaks=seq(0,25, 1)) +
  geom_text(aes(label = text_annotations, y = text_height)) + geom_image(aes(image = image), size = cost_df$size) +
  theme(plot.title = element_text(face ='bold'), legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(face = "bold.italic", color = "black", size = 12)) + 
  labs(x = 'Maximum Stay (mins)' , y='Avg Total Parking Cost ($)') +
  ggtitle('Total Parking Cost vs Length of Stay') +
  transition_states(frame, transition_length = 1, state_length = 0, wrap = FALSE) +
  ease_aes('sine-in-out')
  # animate(plot_transport_emission, renderer = gifski_renderer(loop =FALSE))
  
anim_save("cost_animation.gif", animate(plot_cost, duration = 20, end_pause=65))


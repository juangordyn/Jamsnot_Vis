library('shiny')
library('ggplot2')
library('plotly')
library('gganimate')
library('gifski')
library('ggimage')
library('dplyr')

transport_types <- c('Car', 'Bus', 'Tram', 'Train')
text_annotations <- c('5 people', '60 people', '300 people', '1200 people')
images <- rep('person_64.png',4)
emissions_df_5 <- data.frame(transport = transport_types,
                             emissions = c(5, 60, 300, 1200), frame = rep('e', 4), image = images, label = text_annotations, text_height = c(80, 150, 450, 1400), size = c(0.04,0.0665,0.133,0.18))
emissions_df_1 <- data.frame(transport = transport_types,
                             emissions = c(0, 0, 0, 0), frame = rep('a', 4) , image = images, size = c(0.04,0.04,0.04,0.04))
emissions_df_2 <- data.frame(transport = transport_types,
                             emissions = c(5, 5, 5, 5), frame = rep('b', 4) , image = images, size = c(0.04,0.04,0.04,0.04))
emissions_df_3 <- data.frame(transport = transport_types,
                             emissions = c(5, 60, 60, 60), frame = rep('c', 4) , image = images, size = c(0.04,0.0665,0.0665,0.0665))
emissions_df_4 <- data.frame(transport = transport_types,
                             emissions = c(5, 60, 300,300), frame = rep('d', 4) , image = images, size = c(0.04,0.0665,0.133,0.133))
emissions_df <- bind_rows(emissions_df_1,emissions_df_2, emissions_df_3,emissions_df_4,emissions_df_5)
print(emissions_df)
emissions_df$transport <- factor(emissions_df$transport, levels = transport_types)
plot_carriage <- ggplot(emissions_df, aes(x = transport, y = emissions, fill = transport, group = 1)) + geom_bar(stat = 'identity', colour = 'black') +
  scale_fill_manual(values = c("#F64F4C", "orange", "yellow", "#76B756")) + 
  scale_y_continuous(breaks=seq(0,1500, 100)) +
  geom_text(aes(label = label, y = text_height)) + geom_image(aes(image = image), size = emissions_df$size) +
  theme(plot.title = element_text(face ='bold'), legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(face = "bold.italic", color = "black", size = 12)) + 
  labs(x = '' , y='Maximum number of people') +
  ggtitle('Maximum carriage by type of Transport') +
  transition_states(frame, transition_length = 1, state_length = 0, wrap = FALSE) +
  ease_aes('sine-in-out')
anim_save('carriage_animation.gif', animate(plot_carriage, duration = 20, end_pause=40))

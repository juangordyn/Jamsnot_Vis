library('shiny')
library('ggplot2')
library('plotly')
library('gganimate')
library('gifski')
library('ggimage')
library('dplyr')

transport_types <- c('Car', 'Bus', 'Tram', 'Train')
text_annotations <- c('184 gCO2/km', '18 gCO2/km ', '13 gCO2/km', '12 gCO2/km')
images <- c('car_64.png', 'bus_64.png', 'tram_64.png', 'train_64.png')
emissions_df_1 <- data.frame(transport = transport_types,
                             emissions = c(184, 18, 13, 12), frame = rep('b', 4), image = images, label = text_annotations, text_height = c(203, 35, 32, 30 ))
emissions_df_2 <- data.frame(transport = transport_types,
                             emissions = c(0, 0, 0, 0), frame = rep('a', 4) , image = images)
emissions_df <- bind_rows(emissions_df_2,emissions_df_1)
print(emissions_df)
emissions_df$transport <- factor(emissions_df$transport, levels = transport_types)
key = row.names(emissions_df)
key_list = list()
for(i in (1:length(key))){
  key_list[[i]] <- c(key[i], transport_types)
}
plot_transport_emission <- ggplot(emissions_df, aes(x = transport, y = emissions, fill = transport, group = 1)) + geom_bar(stat = 'identity', colour = 'black') +
  scale_fill_manual(values = c("#F64F4C", "orange", "yellow", "#76B756")) + 
  scale_y_continuous(breaks=seq(0,250, 25)) +
  geom_text(aes(label = label, y = text_height)) + geom_image(aes(image = image), size = 0.075) +
  theme(plot.title = element_text(face ='bold'), legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(face = "bold.italic", color = "black", size = 12)) + 
  labs(x = '' , y='Emissions per km (gCO2/km) per person') +
  ggtitle('Gas emissions by type of transport') +
  transition_states(frame, transition_length = 1, state_length = 0, wrap = FALSE) +
  ease_aes('sine-in-out') 
anim_save("emissions_animation.gif", animate(plot_transport_emission, duration = 15, end_pause=65))

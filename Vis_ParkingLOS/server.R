library('shiny')
library('ggplot2')
library('plotly')

parking_df <- read.csv('parking_los.csv')
parking_df$length_of_stay <- as.factor(parking_df$length_of_stay)
hour_range_vector <- c('8:00-11:00', '11:00-15:00', '15:00-19:00')
parking_df$hour_range <- factor(parking_df$hour_range, levels = hour_range_vector)

plotParkingTime <- function(){
  parking_df_1 <- parking_df[parking_df$hour_range=='8:00-11:00', ]
  maximum_stay_vector <- parking_df_1$length_of_stay
  key = row.names(parking_df_1)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_time_1<- ggplot(data=parking_df_1, aes(x = length_of_stay, y = parking_time, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg Time to find Parking: ', parking_time, 'min'))) + geom_line()+
    labs(x = '', y='') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Time to find Parking vs Length of Stay')
  plot_parking_time_1 <- ggplotly(, tooltip = c('text'), source = 'B') %>% layout(title = 'tuvieja', xaxis =list(title ='Length of stay (min)'), font = list(size=11))%>% add_annotations(
    text = "Hour range 08:00-11:00",
    x = 0,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "left",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  parking_df_2 <- parking_df[parking_df$hour_range=='11:00-15:00', ]
  maximum_stay_vector <- parking_df_2$length_of_stay
  key = row.names(parking_df_2)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_time_2<- ggplot(data=parking_df_2, aes(x = length_of_stay, y = parking_time, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg Time to find Parking: ', parking_time, 'min'))) + geom_line()+
    labs(x = '', y='') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Time to find Parking vs Length of Stay')
  plot_parking_time_2 <- ggplotly(, tooltip = c('text'), source = 'B') %>% layout(yaxis=list(title='Time to find Parking\n (min)'), xaxis =list(title ='Length of stay (min)'), font = list(size=11))%>% add_annotations(
    text = "Hour range 11:00-15:00",
    x = 0,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "left",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  parking_df_3 <- parking_df[parking_df$hour_range=='15:00-19:00', ]
  maximum_stay_vector <- parking_df_3$length_of_stay
  key = row.names(parking_df_3)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_time_3<- ggplot(data=parking_df_3, aes(x = length_of_stay, y = parking_time, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg Time to find Parking: ', parking_time, 'min'))) + geom_line()+
    labs(x = 'Length of stay (min)', y='') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Time to find Parking vs Length of Stay')
  plot_parking_time_3 <- ggplotly(, tooltip = c('text'), source = 'B') %>% layout(xaxis =list(title ='Length of stay (min)'), font = list(size=11)) %>%
  add_annotations(
      text = "Hour range 15:00-19:00",
      x = 0,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    ) %>% 
  layout(yaxis = list(title = ""), yaxis2 = list(title = "Time to find Parking (min)"), yaxis3 = list(title = ""))
  
  final_plot<-subplot(plot_parking_time_1, plot_parking_time_2, plot_parking_time_3, margin = 0.04, nrows=3, titleX=TRUE, shareX=TRUE, titleY=TRUE, shareY=TRUE)
  
  return(final_plot) 
}

plotDistance <- function(){
  parking_df_1 <- parking_df[parking_df$hour_range=='8:00-11:00', ]
  maximum_stay_vector <- parking_df_1$length_of_stay
  key = row.names(parking_df_1)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_distance_1<- ggplot(data=parking_df_1, aes(x = length_of_stay, y = parking_distance, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg distance from Parking to destination: ', parking_distance, 'mts'))) + geom_line()+
    labs(x = '', y='') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Distance from Parking to destination vs Length of stay')
  plot_parking_distance_1 <- ggplotly(, tooltip = c('text'), source = 'B') %>% layout(title = 'tuvieja', xaxis =list(title ='Length of stay (min)'), font = list(size=11))%>% add_annotations(
    text = "Hour range 08:00-11:00",
    x = 0,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "left",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  parking_df_2 <- parking_df[parking_df$hour_range=='11:00-15:00', ]
  maximum_stay_vector <- parking_df_2$length_of_stay
  key = row.names(parking_df_2)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_distance_2<- ggplot(data=parking_df_2, aes(x = length_of_stay, y = parking_distance, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg distance from Parking to destination: ', parking_distance, 'mts'))) + geom_line()+
    labs(x = '', y='') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Distance from Parking to destination vs Length of stay')
  plot_parking_distance_2 <- ggplotly(, tooltip = c('text'), source = 'B') %>% layout(yaxis=list(title='Distance from Parking to destination\n (mts)'), xaxis =list(title ='Length of stay (min)'), font = list(size=11))%>% add_annotations(
    text = "Hour range 11:00-15:00",
    x = 0,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "left",
    yanchor = "top",
    yshift = 20,
    showarrow = FALSE,
    font = list(size = 15)
  )
  
  parking_df_3 <- parking_df[parking_df$hour_range=='15:00-19:00', ]
  maximum_stay_vector <- parking_df_3$length_of_stay
  key = row.names(parking_df_3)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_distance_3<- ggplot(data=parking_df_3, aes(x = length_of_stay, y = parking_distance, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg distance from Parking to destination: ', parking_distance, 'mts'))) + geom_line()+
    labs(x = 'Length of stay (min)', y='') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Distance from Parking to destination vs Length of stay')
  plot_parking_distance_3 <- ggplotly(, tooltip = c('text'), source = 'B') %>% layout(xaxis =list(title ='Length of stay (min)'), font = list(size=11)) %>%
    add_annotations(
      text = "Hour range 15:00-19:00",
      x = 0,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(size = 15)
    ) %>% 
    layout(yaxis = list(title = ""), yaxis2 = list(title = "Time to find Parking (min)"), yaxis3 = list(title = ""))
  
  final_plot<-subplot(plot_parking_distance_1, plot_parking_distance_2, plot_parking_distance_3, margin = 0.04, nrows=3, titleX=TRUE, shareX=TRUE, titleY=TRUE, shareY=TRUE)
  
  return(final_plot) 
}

function(input, output, session){
  
  observe({
  if(input$plot_type=='Avg time to find Parking vs Length of stay'){
    output$parking_plot <- renderUI({fluidRow(column(3,''),column(6,align = 'center', plotlyOutput("parkingPlot")), column(3,''))})
    output$parkingPlot <- renderPlotly({plotParkingTime()})
  }
    else{
      output$parking_plot <- renderUI({fluidRow(column(3,''),column(6,align = 'center', plotlyOutput("parkingDistance")), column(3,''))})
      output$parkingDistance <- renderPlotly({plotDistance()})
    }
  })
  
  
  
  
}
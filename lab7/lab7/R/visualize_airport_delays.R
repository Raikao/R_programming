#' Visualize airport delays
#'
#' Visualizes the average delays in the airports plotted in the map of North America
#'
#' @import dplyr
#' @import maps
#' @import nycflights13
#' @import plotly 
#' @import ggplot2
#' @import magrittr
#'
#'@return A ggplot2 object
#'
#'@export


visualize_airport_delays <- function(){

  airport <- nycflights13::airports %>%
    dplyr::mutate(ID = faa) %>%
    dplyr::select(-faa, -alt, -tz, -dst)
  
  dep <- nycflights13::flights %>%
    dplyr::group_by(origin) %>%
    dplyr::summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    dplyr::arrange(avg_dep_delay) %>%
    dplyr::mutate(ID = origin) %>%
    dplyr::select(-origin) %>%
    dplyr::right_join(airport, by = 'ID') %>%
    dplyr::filter(!is.na(avg_dep_delay))%>%
    dplyr::mutate(type = ifelse(avg_dep_delay >= 0, 'behind', 'ahead')) %>%
    dplyr::mutate(avg_dep_delay = ifelse(avg_dep_delay < 0, avg_dep_delay * (-1), avg_dep_delay))
  
  
  
  arr <- nycflights13::flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(avg_arr_delay=mean(arr_delay, na.rm= TRUE))%>%
    dplyr::arrange(avg_arr_delay)%>%
    dplyr::mutate(ID=dest)%>%
    dplyr::select(-dest) %>%
    dplyr::right_join(airport, by = 'ID')%>%
    dplyr::filter(!is.na(avg_arr_delay))%>%
    dplyr::mutate(type= ifelse(avg_arr_delay >= 0, 'behind', 'ahead'))%>%
    dplyr::mutate(avg_arr_delay = ifelse(avg_arr_delay < 0, avg_arr_delay * (-1), avg_arr_delay))
  
  
  max_latitude <- c(min(arr$lat, dep$lat), max(arr$lat, dep$lat))*1.02
  max_longitude <- c(min(arr$lon, dep$lon), max(arr$lon, dep$lon))*1.02
  
  
  us <- ggplot2::map_data("world")
  
  ggplot2::ggplot()+
    geom_polygon(data=us, fill = "lightyellow", color = "black",
                 aes(x=long, y=lat, group=group))+
    geom_point(data = arr, aes(x = arr$lon, y = arr$lat,
                                fill= as.factor(type),
                                size=sqrt(arr$avg_arr_delay)), alpha=0.5,
               shape=21)+
    
    geom_point(data=dep, aes(x = dep$lon, y = dep$lat,
                             fill = as.factor(type),
                             size = sqrt(dep$avg_dep_delay)), alpha=0.5,
                shape = 24)+
    labs(fill="Delay", size="size" )+
    coord_cartesian(xlim= max_longitude, ylim=max_latitude)+
    ggtitle('Average delay of flights in USA')+
    xlab('Longitude')+
    ylab('Latitude')
    
  
  
  
  
  
  
  
}

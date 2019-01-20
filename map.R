library(caret)
library(dplyr)
library(ggmap)

height <- max(airbnb$latitude) - min(airbnb$latitude)
width <- max(airbnb$longitude) - min(airbnb$longitude)
sac_borders <- c(bottom  = min(airbnb$latitude)  - 0.2 * height, 
                 top     = max(airbnb$latitude)  + 0.2 * height,
                 left    = min(airbnb$longitude) - 0.2 * width,
                 right   = max(airbnb$longitude) + 0.2 * width)

map <- get_stamenmap(sac_borders, zoom = 11, maptype = "toner-lite")

airbnb_plot <- airbnb %>%
  group_by(neighborhood) %>%
  summarise(count = n(), lat = mean(latitude), long = mean(longitude), price=mean(price))

ggmap(map) +
  geom_point(data = airbnb_plot, mapping = aes(x = long, y = lat, 
                                      size=count, col=price)) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  guides(color=guide_legend(title="Average Price"),
         size=guide_legend(title="Number of Houses")) +
  scale_size_continuous(range = c(4, 9))

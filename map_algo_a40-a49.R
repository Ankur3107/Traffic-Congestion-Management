#library(ggmap)

legs_df <- route(
  '51.860147, -2.649044',
  '51.917209, -2.632783', 
  alternatives = TRUE, mode = c("driving")
)
map_plot <- qmap('51.897498, -2.644227', zoom = 12, maptype = 'roadmap',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route')


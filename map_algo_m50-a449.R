library(ggmap)
legs_df <- route(
  '51.928827, -2.567545',
  '52.029352, -2.413756', 
  alternatives = TRUE, mode = c("driving")
)
qmap('51.977893, -2.471598', zoom = 11, maptype = 'roadmap',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')


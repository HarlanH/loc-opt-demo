library(plyr)
library(rMaps)

venues <- structure(list(name = c("pivotal", "aol", "columbia"), lat = c(40.7403372, 
                    40.7308948, 40.8074358), lon = c(-73.9951462, -73.9917096, -73.9625858 )), 
                    .Names = c("name", "lat", "lon"), row.names = c(NA, -3L), class = "data.frame")

times_square <- c(40.7577, -73.9857)

map <- Leaflet$new()
map$setView(times_square, zoom = 10)
map$tileLayer(provider = 'Stamen.Toner')

mk_polygon <- function(lats, lons) {
    stopifnot(length(lats)==length(lons))
    coord_list <- llply(seq_along(lats), function(i) c(lons[[i]], lats[[i]]))
    list(type='Feature',
         geometry=list(type='Polygon', coordinates=coord_list))
}
polygons <- mk_polygon(venues$lat, venues$lon)
map$geoJson(polygons) #, list(style=list(color="#ff7800", weight=5, opacity=0.65)))
# throwing very weird error!
map


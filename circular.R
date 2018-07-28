# R 3.4.4
require(tidyverse)
#─ Attaching packages ──────────────────────────────────────────────────────── tidyverse 1.2.1 ─
#✔ ggplot2 3.0.0     ✔ purrr   0.2.5
#✔ tibble  1.4.1     ✔ dplyr   0.7.4
#✔ tidyr   0.8.0     ✔ stringr 1.3.0
#✔ readr   1.1.1     ✔ forcats 0.3.0

require(sf)  # ver. 0.6-3
# https://notchained.hatenablog.com/entry/2017/01/06/213333
# libgdal-dev needed
require(osmdata)  # ver. 0.0.7
require(ggthemes)  # ver. 3.5
require(xkcd)  # ver. 0.0.6
# https://notchained.hatenablog.com/entry/2018/05/28/003910
# https://uribo.hatenablog.com/entry/2018/05/28/075500
require(stringr)  # ver. 1.3.0

#
# How to drop unneccesarry cols
#
#

###### create query #####
# https://wiki.openstreetmap.org/wiki/JA:Overpass_API
as_bb_bbox <- function(bb){
  bb <- as.numeric(bb)
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  return(bb)
}

### query functions for highway
highway_not_in <- c(
  "cycleway", "footway", "pedestrian", "steps", "corridor",
  "track", "proposed", "construction", "bridleway", "abandoned",
  "platform", "raceway", "service", "path"
  )
conditions <- list(
  c(key="area", value="!yes"),
  c(key="highway", value=paste0("!", paste(highway_not_in, collapse="|"))),
  c(key="motor_vehicle", value="!no"),
  c(key="motorcar", value="!no"),
  c(key="access", value="!private"),
  c(key="service", value="!parking|parking_aisle|driveway|private|emergency_access")
)
make_query_driving_highway <- function(geocode, cond=conditions){
  query <- opq(geocode)
  for(i in 1:length(cond)){
    query <- query %>% add_osm_feature(cond[[i]]["key"], cond[[i]]["value"], value_exact=F)
  }
  query$prefix <- "[out:xml][timeout:100];\n(\n"
  return(query)
}

### get rectangle wrapping 23 wards
district23 <- c(
  'Chiyoda', 'Chuo', 'Minato',
  'Shinjuku', 'Bunkyo', 'Taito',
  'Sumida', 'Koto', 'Shinagawa',
  'Meguro', 'Ota', 'Setagaya',
  'Shibuya', 'Nakano', 'Suginami',
  'Toshima', 'Kita', 'Arakawa',
  'Itabashi', 'Nerima', 'Adachi',
  'Katsushika', 'Edogawa'
)
rect <- (function(){
  rect <- list()
  for(d in district23){
    print(d)
    rect[[d]] <- getbb(paste0(d, "-ku, Tokyo, Japan"))
  }
  return(rect)
})()

rect <- purrr::map(rect, as.numeric) %>% as.data.frame %>%
  rownames_to_column() %>%
  gather(key=district, value=value, -rowname) %>%
  spread(key = rowname, value = value) %>%
  rename(xmin="1", ymin="2", xmax="3", ymax="4") %>%
  summarise(xmin=min(xmin), ymin=min(ymin), xmax=max(xmax), ymax=max(ymax))
geocode_tokyo23 <-matrix(as.numeric(rect), nrow=2, ncol=2)
rownames(geocode_tokyo23) <- c("x", "y")
colnames(geocode_tokyo23) <- c("min", "max")


# query for roads in Tokyo 23 wards
query_road_tokyo23 <- make_query_driving_highway(geocode_tokyo23)


### query for adminisitrative boundary of Tokyo 23 special wards
query_boundary_tokyo23 <- opq(geocode_tokyo23)
query_boundary_tokyo23 <- query_boundary_tokyo23 %>% add_osm_feature(key="type", value="boundary") %>%
  add_osm_feature(key="boundary", value="administrative") %>%
  add_osm_feature(key="admin_level", value="7", value_exact=F) %>%
  add_osm_feature(key="place", value="city|special ward", value_exact=F) %>%
  add_osm_feature(key="name", value="区", value_exact=F)
query_boundary_tokyo23$prefix <- "[out:xml][timeout:100];\n(\n"
opq_string(query_boundary_tokyo23)


##### request #####
boundary_tokyo23 <- osmdata_sf(query_boundary_tokyo23)$osm_multipolygons %>% dplyr::select(name, name.en)
# fill holes and union boundaries of 23 wards 
# TODO: smarter way needed
boundary_tokyo23_outline <- st_difference(
  boundary_tokyo23 %>% st_buffer(.00001),
  boundary_tokyo23 %>% st_buffer(.00001)) %>%
  st_combine() %>% st_union()
# save(boundary_tokyo23, boundary_tokyo23_outline, file="boundary.RData")
# load("boundary.RData")
land_tokyo23 <- read_sf("N03-180101_13_GML/N03-18_13_180101.shp", options = "ENCODING=CP932", stringsAsFactors=FALSE) %>%
  filter(grepl("区$", N03_004)) %>% dplyr::select(N03_004) %>% rename(name=N03_004) %>% group_by(name) %>% summarise()

  
osmdata_xml(query_road_tokyo23, filename="tokyo.xml")
road_tokyo23 <- read_sf("tokyo.xml", layer='lines', quiet = TRUE,
                        options = "ENCODING=UTF-8", stringsAsFactors=FALSE) %>%
  filter(!highway %in% highway_not_in) %>% filter(!is.na(highway)) %>%
  st_intersection(boundary_tokyo23_outline)
# save(road_tokyo23, file="road.RData")
# load("road.RData")

# plot road map colord by highway type
plot_road <- function(road_data, area_data){
  # aggregate geometry to mitigate draw ggplot payload
  road_data <- dplyr::select(road_data, highway) %>%
    mutate(highway=sub("_link$", "", highway)) %>%
    group_by(highway) %>% summarise(do_union=F)
  # how to override POLYGON and LINE
  # https://notchained.hatenablog.com/entry/2018/06/05/221255
  g <- ggplot() + geom_sf(aes(fill=name), data=area_data, color="grey", linetype="twodash", alpha=.2) +
    geom_sf(aes(color=highway), data=road_data) +
    scale_fill_discrete(guide=F) + scale_color_pander() +
    theme_tufte() + theme(panel.grid=element_line(color="grey", size=.5)) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
  return(g)
}

plot_road(road_tokyo23, land_tokyo23)

# create dataset for circular plot
make_polar_data <- function(data){
  polar <- st_cast(data %>%
                     dplyr::select(osm_id, highway), to = "MULTIPOINT") %>% st_cast("POINT") 
  polar <- bind_cols(polar,
                     do.call(rbind, st_geometry(polar)) %>%
                       as_tibble() %>% setNames(c("lon", "lat"))
  ) %>% 
    mutate(dx= lon - lag(lon), dy = lat - lag(lat)) %>%
    mutate(angle = atan2(dy, dx), radius = sqrt(dx^2 + dy^2) ) %>%
    dplyr::select(osm_id, highway, angle, radius) %>%
    filter(!is.na(radius)) %>%
    filter(radius > 0) %>%
    mutate(highway=sub("_link$", "", highway))
  return(polar)
}

polar_tokyo23 <- make_polar_data(road_tokyo23)

g <- ggplot(polar_tokyo23, aes(x=angle, fill=highway)) + geom_histogram(bins=64) +
  coord_polar(start=pi) +
  scale_x_continuous(limits = c(-pi, pi), breaks=c(-pi, -pi/2, 0, pi/2), labels = c("S", "W", "N", "E")) + 
  theme_tufte() + theme(panel.grid.major = element_line(colour="grey", size=.5), axis.title = element_blank()) +
  scale_fill_pander() + labs(title="Polar Plot of Driving Road in Tokyo 23 Wards")
print(g)
ggsave(filename = "polar_tokyo.png", g)

# weight by road length
g <- ggplot(polar_tokyo23, aes(x=angle, fill=highway, weight=radius)) + geom_histogram(bins = 64) +
  coord_polar(start=pi) +
  scale_x_continuous(limits = c(-pi, pi), breaks=c(-pi, -pi/2, 0, pi/2), labels = c("S", "W", "N", "E")) + 
  theme_tufte() + theme(panel.grid.major = element_line(colour="grey", size=.5), axis.title=element_blank()) +
  scale_fill_pander() + labs(title="Polar Plot of Driving Road in Tokyo 23 Wards (Weighted by Length)")
print(g)
ggsave(filename = "polar_length_weighted.png", g)


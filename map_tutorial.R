# Install package if not exists
pkgs <- c(
  'tidyverse','sf','jsonlite','psych','clipr','hexjsonwidget',
  'geojson','geojsonio','geojsonsf','maps','ggrepel','broom','cartogram'
  )

new.pkgs <- pkgs[!(pkgs %in% installed.packages())]
if (length(new.pkgs)) {
  install.packages(new.pkgs, repos = 'http://cran.csie.ntu.edu.tw/')
}

library(tidyverse)
library(sf)
library(jsonlite)
library(hexjsonwidget)
library(geojson)
library(geojsonio)
library(geojsonsf)
library(maps)
library(ggrepel)
library(broom)
library(cartogram)
# library(clipr)
# library(psych)


##### Map: Background Map

### read shp directly
sf_county_shp_big = st_read("Sharing/data/geo_raw/mapdata201911261001/COUNTY_MOI_1081121.shp")
sf_county_shp_big %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() + geom_sf()

### read simplified shp
sf_county_shp_small = st_read("Sharing/data/geo_raw/COUNTY_MOI_1081121/COUNTY_MOI_1081121.shp")
sf_county_shp_small %>%
  # st_crop(xmin=117,xmax=123,ymin=21,ymax=26) %>%
  ggplot() + geom_sf()

### read geojson - from mapshaper
county_json_raw = read_json("Sharing/data/geo_raw/tawian_county.json") 
sf_county_geojson_simplified = county_json_raw %>% as.json() %>% geojson_sf()
sf_county_geojson_simplified %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() + geom_sf()

### read geojson
json_county_raw = read_json("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/taiwan.geojson")
sf_county_geojson = json_county_raw %>% as.json() %>% geojson_sf()

sf_county_geojson %>%
  ggplot() + geom_sf()

##### Map: Choroleth

### read rds
df_president_county_result <- read_rds("Sharing/data/2020/df_president_county_result_raw.rds") %>%
  select(place, number, party, name, vote, per, vote_place) 
### read csv if needed
# df_president_county_result <- read_csv("Sharing/data/2020/df_president_county_result_raw.csv") %>%
#   select(place, number, party, name, vote, per, vote_place) 

### filter KMT presidential vote
df_president_county_result_blue =
  df_president_county_result %>% 
  select(-party, -vote) %>%
  filter(number == 2)

### add centroid column for further use
sf_county_centroid =
  sf_county_geojson_simplified %>%
  st_centroid(of_largest_polygon = T)

df_coordination = 
  sf_county_centroid %>% 
  st_coordinates()

### join vote & Taiwan sf data
sf_county_president <- 
  sf_county_geojson_simplified %>% 
  left_join(df_president_county_result_blue, by = c("COUNTYNAME" = "place")) %>%
  mutate(rn = row_number()) %>%
  mutate(x_centroid = df_coordination[,"X"],
         y_centroid = df_coordination[,"Y"])

### plotting + text label
sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot()+
  geom_sf(size = 0.2, aes(fill = per))

sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot()+
  geom_sf(size = 0.2, aes(fill = per)) +
  # geom_text(
  #   aes(
  #     x=x_centroid,y=y_centroid, 
  #     label = round(per, 2)
  #   ), size=3
  # ) +
  scale_fill_gradientn(colors = c("white","#CCCCEE","#9999DD","#4141EF")) +#, "#33AE33", "#66C266", "#99D799"
  # scale_fill_gradientn(colors = c("white", "#33AE33", "#66C266", "#99D799"))
  coord_sf(datum = NA)+
  ggthemes::theme_map()

##### Dot Distribution Map/Bubble Map

## Get the world polygon and extract taiwan
df_map_taiwan <- map_data("world") %>% 
  filter(region=="Taiwan")

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
df_city_taiwan <- world.cities %>% 
  filter(country.etc=="Taiwan")

df_city_taiwan %>% head(3)
df_map_taiwan %>% head(3)

### plotting: each point equals a city
sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() +
  geom_sf(size = 0.2) +
  geom_point(data=df_city_taiwan, aes(x=long, y=lat)) +
  theme_void()

### plotting: city population size matters
sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() +
  geom_sf(size = 0.2) +
  geom_point(data=df_city_taiwan, aes(x=long, y=lat, size = pop)) +
  theme_void()

sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() +
  geom_sf(size = 0.2) +
  geom_point(data=df_city_taiwan, aes(x=long, y=lat)) +
  geom_text_repel( data=df_city_taiwan %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point(data=df_city_taiwan %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + 
  theme(legend.position="none")

### comarison: geom_sf vs. geom_polygon
sf_county_president
df_map_taiwan

df_map_taiwan %>%
  ggplot() +
  geom_polygon(aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)

sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() +
  geom_sf(size = 0.2)

### plot with geom_polygon
ggplot() +
  geom_polygon(data = df_map_taiwan, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=df_city_taiwan, aes(x=long, y=lat)) +
  theme_void() +
  coord_map() 

### plot with geom_polygon: city size
ggplot() +
  geom_polygon(data = df_map_taiwan, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=df_city_taiwan, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel(data=df_city_taiwan %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point(data=df_city_taiwan %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + coord_map() +
  theme(legend.position="none")

##### Cartogram
sp_county_president = sf_county_president %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  as('Spatial')

president_cartogram <- cartogram_cont(sp_county_president, "vote_place", itermax=5)
spdf_fortified <- tidy(president_cartogram)
spdf_fortified = spdf_fortified %>% 
  mutate(id = as.double(id)) %>%
  left_join(. , president_cartogram@data, by=c("id"="rn"))

spdf_fortified %>%
  ggplot() +
  geom_polygon(aes(fill = per, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  scale_fill_gradientn(colors = c("white","#CCCCEE","#9999DD","#4141EF")) +#, "#33AE33", "#66C266", "#99D799"
  coord_map() +
  theme_void()

##### Hexmap/Tilegram

#skip

##### Parliament Plot
library(ggparliament)

### read summary data
df_parliament_raw <- read_rds("Sharing/data/2020/df_parliament_summary_raw.rds")
# df_parliament_raw <- read_csv("Sharing/data/2020/df_parliament_summary_raw.csv")
df_parliament_raw
### use ggparliament::parliament_data to create cooridinate system & theta
df_parliament_final <- parliament_data(election_data = df_parliament_raw,
                                       type = "semicircle",
                                       parl_rows = 6,
                                       party_seats = df_parliament_raw$seats)
df_parliament_final %>% glimpse()
df_parliament_final %>% glimpse()

### plot
df_parliament_final %>%
  ggplot(aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 5) 

### plot: add colours
df_parliament_final %>%
  ggplot(aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 5)  +
  scale_colour_manual(values = unique(us_house$colour), 
                      limits = unique(us_house$party_short))

### plot: add colours
df_parliament_final %>%
  ggplot(aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 5) + 
  geom_highlight_government(government == 1) +
  geom_parliament_bar(colour = colour, party = party_long) + 
  draw_majoritythreshold(n = 57, label = TRUE, type = 'semicircle')+
  theme_ggparliament() +
  #other aesthetics
  labs(colour = NULL, 
       title = "United States House of Representatives",
       subtitle = "Party that controls the House highlighted.") +
  scale_colour_manual(values = unique(us_house$colour), 
                      limits = unique(us_house$party_short)) 

#type = "horseshoe", "semicircle", "circle", "classroom", "opposing_benches"


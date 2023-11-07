library(extrafont)
library(here) #consistent local paths, check it out

# register downloaded Tolkien.tff with extrafont package
# use your own folder path
font_import(paths = here("posts", "shire-map", "data"))

# import font data into extrafont library within R
loadfonts()

# fonts loaded this way are availble for future sessions 
# just load extrafont with this verison of R

library(tidyverse)
library(stringi)
library(sf)
library(terra)
library(ggspatial)
library(ggpattern)
library(here)
library(extrafont)

# keep colors consistent
clr_green <- "#035711"
clr_blue <- "#9CC2EA"
clr_yellow <- "#fffce3"

# quick conversion
miles_to_meters <- function(x)  x * 1609.344

# ensure consistent spatial data import
load_me_data <- function(path) { 
  # load shapefile
  st_read(path, as_tibble = T, options = "ENCODING=ISO-8859-1") %>% 
    # drop accents in placenames (font doesn't have them)
    rename_with(.fn = str_to_lower) %>% 
    mutate(NAME = stri_trans_general(name, "Latin-ASCII")) %>% 
    # re-project to consistent coordinate system
    st_transform(32631)
}

# load middle earth vector files
me_forests <- load_me_data(here("posts", "shire-map", "data", "ME-GIS-master", "Forests.shp"))
me_rivers <- load_me_data(here("posts", "shire-map", "data", "ME-GIS-master", "Rivers.shp"))
me_towns <- load_me_data(here("posts", "shire-map", "data", "ME-GIS-master", "Towns.shp"))
me_lakes <- load_me_data(here("posts", "shire-map", "data", "ME-GIS-master", "Lakes.shp"))
me_wetlands <- load_me_data(here("posts", "shire-map", "data", "ME-GIS-master", "Wetlands02.shp"))
me_roads <- load_me_data(here("posts", "shire-map", "data", "ME-GIS-master", "Roads.shp"))

# load digital elevation model
me_dem <- rast(here("posts", "shire-map", "data", "DEM", "10K.jpg"))

# match coordinate system to rest of data
crs(me_dem) <- "epsg:32631"

# grab hobbiton to anchor map extent
hobbiton <- me_towns %>% 
  filter(name == "Hobbiton") %>% 
  mutate(geometry_x = map_dbl(geometry, ~as.numeric(.)[1]),
         geometry_y = map_dbl(geometry, ~as.numeric(.)[2]))

# create bounding box to select elevation data
shire_bb <- st_bbox(c(xmin = hobbiton$geometry_x - miles_to_meters(50),
                      xmax = hobbiton$geometry_x + miles_to_meters(70),
                      ymax = hobbiton$geometry_y + miles_to_meters(50),
                      ymin = hobbiton$geometry_y - miles_to_meters(50)),
                    crs = st_crs(32631))

# limit DEM to shire extent
shire_dem <- crop(me_dem, ext(shire_bb))

# local weighted mean, Gaussian kernel with 200m radius
smooth <- focal(shire_dem, w = focalMat(shire_dem, d = 200, type = "Gauss"), fun = "mean")

# calculate terrain surface for rayshading the map shadow effect
sl <- terrain(smooth, "slope", unit = "radians")
asp <- terrain(smooth, "aspect", unit = "radians")

# shade from different sun angles and create composite shade values
hillmulti <- map(
  # angles of light source
  c(270, 15, 60, 330), 
  # lambda function to get shade generated at each cell
  \(dir) shade(sl, asp, angle = 45, direction = dir, normalize=TRUE)
) %>% 
  rast() %>% 
  sum()

# convert raster to df for ggplot
hillmultidf <- as.data.frame(hillmulti, xy = TRUE)

# settlements
towns_anno <- tibble(
  name = c("Hobbiton", "Bywater", "Bree", "Stock", "Michel\nDelving", "Scary", 
           "Sackville", "Longbottom", "Waymeet", "Needlehole", "Nobottle", "Overhill",
           "Frogmorton", "Bucklebury", "Newbury", "Tuckburrow", "Tookbank", "Greenholm",
           "Oatbarton"),
  x = c(512700, 526000, 596000, 550500, 479517.2, 545000, 
        513500, 522000, 505000, 495000, 486000, 520000,
        537000, 561000, 568000, 517000, 513000, 456000,
        529000),
  y = c(1047200, 1045500, 1043500, 1039200, 1029736, 1054000, 
        997500, 1001500, 1039000, 1056000, 1047500, 1052300,
        1041000, 1030500, 1037500, 1030000, 1036000, 1026000,
        1065068),
  angle = c(-15, -7, -16, -6, 25, 0, 
            0, 0, 12, -30, 0, 30,
            12, 0, 0, 20, 45, 0,
            0)
)

# rivers
rivers_anno <- tibble(
  name = c("The Water", "River Brandywine\n(Baranduin)", "River Withywindle", "River Shirebourn"),
  x = c(539000, 542500, 563000, 531000),
  y = c(1047000, 998000, 1024000, 1011700),
  angle = c(0, 52, 22, -20)
)

# woods and marshes
woods_anno <- tibble(
  name = c("Woody\nEnd", "The Marish", "Overbourn\nMarshes", "Chetwood", 
           "Bindbode\nWood", "The Old\nForest", "Green Hills\nCountry"),
  x = c(536000, 546000, 547000, 604000,
        513000, 567000, 524000),
  y = c(1028000, 1027000, 1012000, 1052000,
        1056000, 1015000, 1025500),
  angle = c(20, 65, 0, 25,
            15, 0, 0)
)

# roads
roads_anno <- tibble(
  name = c("Great East Road", "North-South Road\n(Greenway)"),
  x = c(575000, 599500),
  y = c(1050000, 1062000),
  angle = c(8, 79)
)

# hills
hills_anno <- tibble(
  name = c("The\nBarrow-downs", "The\nFar\nDowns"),
  x = c(590000, 460000),
  y = c(1037000, 1036000),
  angle = c(0, 0)
)

# combine map features
anno_df <- bind_rows(list(towns_anno, rivers_anno,woods_anno, roads_anno, hills_anno))

# title
title <- tibble(
  txt = c("The Shire"),
  x = c(477000),
  y = c(1000500)
)

# cartographer and date
captions <- tibble(
  txt = c("Ben Bellman    August 15, 2023"),
  x = c(477000),
  y = c(993000)
)

ggplot() +
  # start with elevation rayshade
  geom_raster(data = hillmultidf,
              aes(x, y, fill = sum),
              show.legend = FALSE,
              alpha = 0.5) +
  scale_fill_distiller(palette = "Greys") +
  # physical geography
  geom_sf(data = me_forests, linewidth = 0, fill = clr_green, alpha = 0.3) +
  geom_sf(data = me_rivers, linewidth = 0.75, color = clr_blue) +
  geom_sf(data = me_lakes, color = clr_blue, fill = clr_blue) +
  geom_sf_pattern(data = me_wetlands, pattern = "stripe", pattern_density = 0.95, pattern_spacing = 0.02,
                  pattern_color = clr_blue, pattern_fill = clr_green, fill = clr_blue, color = clr_blue, pattern_alpha = 0.25) +
  # roads
  geom_sf(data = filter(me_roads, type == "PRIMARY"), linewidth = 0.7, color = "#483C32", alpha = 0.5) +
  geom_sf(data = filter(me_roads, type == "SECONDARY"), linewidth = 0.4, color = "#483C32", alpha = 0.5) +
  geom_sf(data = filter(me_roads, type == "TERTIARY"), linewidth = 0.2, color = "#483C32", alpha = 0.5) +
  # settlements
  geom_sf(data = filter(me_towns, type == "Town"), size = 1.5, pch = 18, color = "#483C32") +
  geom_sf(data = filter(me_towns, type == "Village"), size = 0.5, color = "#483C32") +
  # map annotations
  geom_text(
    data = anno_df,
    aes(x = x, y = y, label = name, angle = angle), 
    family = "Tolkien", size = 3.5, color = "black"
  ) + 
  # title
  geom_text(
    data = title,
    aes(x = x, y = y, label = txt), 
    family = "Tolkien", size = 15
  ) + 
  # captions
  geom_text(
    data = captions,
    aes(x = x, y = y, label = txt), 
    family = "Tolkien", size = 4.5
  ) + 
  # scale bar
  annotation_scale(location = "br", bar_cols = c("black", "white"),
                   text_family = "Tolkien",
                   unit_category = "imperial") +
  # base map extent on hobbiton
  coord_sf( # figuring out this extent was trial and error
    xlim = c(hobbiton$geometry_x - miles_to_meters(38),
             hobbiton$geometry_x + miles_to_meters(53)),
    ylim = c(hobbiton$geometry_y - miles_to_meters(32),
             hobbiton$geometry_y + miles_to_meters(18.625)),
    crs = 32631
  ) +
  # map vibes
  theme_void() +
  theme(
    panel.background = element_rect(fill = clr_yellow, color = NA),
    legend.position = "none"
  ) -> p

# save the map, specify size by pixels
ggsave(p, filename = here("posts", "shire-map", "shire_map_desktop.png"), 
       device = ragg::agg_png, width = 2560, height = 1440, units = "px")



###

fonttable()
fonts()

ggplot(tibble(x = 1, y = 1, label = "Test")) +
  geom_text(aes(x = x, y = y, label = label), family = "Tolkien")

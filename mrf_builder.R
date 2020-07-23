# build the Markov random field for the locations

library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(stars)

# get the projection
proj <- readLines("3338.prj")

# get unique locations
fishy_files <- dir("data", full=TRUE)
fish <- c()
for(ff in fishy_files){
  ffile <- read_csv(ff,  na = c("", "NA", -9999))
  if(any(is.na(ffile$LATITUDE))){
    ffile <- ffile[-which(is.na(ffile$LATITUDE)), ]
  }
  fish <- bind_rows(fish, ffile)
}
# get unique locations
grid <- fish %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all=TRUE) %>%
  # make VESSEL a factor
  mutate(VESSEL = as.factor(VESSEL)) %>%
  select(LATITUDE, LONGITUDE, YEAR, BOT_DEPTH,
         BOT_TEMP, SURF_TEMP, VESSEL, STATION)


# project
xy <- sf_project(from="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                 to=proj, grid[, c("LONGITUDE", "LATITUDE")])
grid$x <- xy[, 1]
grid$y <- xy[, 2]


# find unique year-station combinations
station_year_combinations <- unique(grid[, c("STATION", "YEAR")])

# now get all of the station IDs
# this isn't great but good enough (stations move slightly between years)
station_ind <- duplicated(grid$STATION)
grid <- grid[!station_ind, ]
grid <- grid[,c("LATITUDE", "LONGITUDE", "BOT_DEPTH", "STATION", "x", "y")]

# now create a spatial object based on those unique locations
d <- st_as_sf(grid, crs=proj, coords = c("x", "y"))
d <- st_union(d)

# make a hull around them all
hull <- st_buffer(d, dist=30000)
# now do a Voronoi tiling to get the trawl areas
# roughly reconstructing the sampling grid at:
# https://archive.fisheries.noaa.gov/afsc/RACE/groundfish/survey_data/ebswater.htm
v <- st_voronoi(d)
# use the hull to truncate the outer edges of the tiling
mrf_sp <- st_intersection(st_cast(v), hull)

# for mgcv format
mrf_mgcv <- lapply(mrf_sp, as.matrix)

## yet more mrf foolishness
#mk_1d_mrf <- function(x){
#  xx <- as.list(1:length(x))
#  names(xx) <- order(x)
#  for(i in seq_along(xx)){
#    if(i==1){
#      xx[[i]] <- names(xx)[i+1]
#    }else if(i==length(xx)){
#      xx[[i]] <- names(xx)[i-1]
#    }else{
#      xx[[i]] <- c(names(xx)[i-1], names(xx)[i+1])
#    }
#  }
#  xx
#}
#
#bot_temp_mrf <- mk_1d_mrf(fish$BOT_TEMP)
#fish$BOT_TEMP_ID <- as.factor(order(fish$BOT_TEMP))
#surf_temp_mrf <- mk_1d_mrf(fish$SURF_TEMP)
#fish$SURF_TEMP_ID <- as.factor(order(fish$SURF_TEMP))
#bot_depth_mrf <- mk_1d_mrf(fish$BOT_DEPTH)
#fish$BOT_DEPTH_ID <- as.factor(order(fish$BOT_DEPTH))

save(mrf_mgcv, mrf_sp, station_year_combinations,
     file="fish_mrf.RData")


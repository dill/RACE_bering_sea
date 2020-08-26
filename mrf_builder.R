# build the Markov random field for the locations

library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(stars)
library(deldir)

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

fish$STATION <- as.factor(fish$STATION)

# get unique locations
grid <- fish %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all=TRUE) %>%
  # make VESSEL a factor
  mutate(VESSEL = as.factor(VESSEL)) %>%
  select(LATITUDE, LONGITUDE, YEAR, BOT_DEPTH,
         BOT_TEMP, SURF_TEMP, VESSEL, STATION, STRATUM)



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
grid <- grid[,c("LATITUDE", "LONGITUDE", "BOT_DEPTH", "STATION", "STRATUM", "x", "y")]


# make a hull around them all
d <- st_as_sf(grid, crs=proj, coords = c("x", "y"))
d <- st_union(d)
hull <- st_buffer(d, dist=30000)
# now do a Voronoi tiling to get the trawl areas
# roughly reconstructing the sampling grid at:
# https://archive.fisheries.noaa.gov/afsc/RACE/groundfish/survey_data/ebswater.htm
# st_voronoi is bust since it doesn't preserve ordering, so use deldir
# https://github.com/r-spatial/sf/issues/1371
v <- deldir(grid$x, grid$y)

mrf_mgcv <- list()

pairs <- cbind(c(v$dirsgs$ind1, v$dirsgs$ind2),
               c(v$dirsgs$ind2, v$dirsgs$ind1))

for (p in 1:length(unique(grid$STATION))){
  set <- pairs[which(pairs[,1]==p),]
  buds <- list(array(grid$STATION[set[,2]]))
  names(buds) <- p
  mrf_mgcv <- c(mrf_mgcv, buds)
}

names(mrf_mgcv) <- unique(grid$STATION)

ll <- lapply(tile.list(v), function(x) st_sf(st_sfc(st_polygon(list(cbind(x$x[c(1:length(x$x), 1)],x$y[c(1:length(x$x), 1)]))), crs=proj)))


lll <- do.call(what = sf:::rbind.sf, args = ll)
mrf_sp <- st_intersection(st_cast(lll), hull)
mrf_sp <- cbind(mrf_sp, grid)
mrf_sp$x <- NULL
mrf_sp$y <- NULL
mrf_sp$LATITUDE <- NULL
mrf_sp$LONGITUDE <- NULL

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

# centroids
mrf_centroids <- v$summary[, c("x", "y")]
mrf_centroids$STATION <- grid$STATION

save(mrf_mgcv, mrf_sp, mrf_centroids, station_year_combinations,
     file="fish_mrf.RData")


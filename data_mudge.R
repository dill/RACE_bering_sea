# mudge the fish data

library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(stars)

# get the projection
proj <- readLines("3338.prj")

# load the data and squish it together
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
         BOT_TEMP, SURF_TEMP, VESSEL)
grid <- grid[!is.na(grid$YEAR),]

# time to get our mudge on...
# get the observations where *something* got caught
fish <- fish %>%
  filter(COMMON=="Arctic cod") %>%
  # make VESSEL a factor
  mutate(VESSEL = as.factor(VESSEL)) %>%
  select(LATITUDE, LONGITUDE, YEAR, NUMCPUE, BOT_DEPTH,
         BOT_TEMP, SURF_TEMP, VESSEL)

# projection
xy <- sf_project(from="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                 to=proj, fish[, c("LONGITUDE", "LATITUDE")])
fish$x <- xy[, 1]
fish$y <- xy[, 2]
xy <- sf_project(from="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                 to=proj, grid[, c("LONGITUDE", "LATITUDE")])
grid$x <- xy[, 1]
grid$y <- xy[, 2]

# get the bits we missed
ff <- anti_join(grid, fish, by=c("LATITUDE", "LONGITUDE", "YEAR"))
# now take the zeros we just found and join the non-zeros (the fish)
fish <- bind_rows(fish, ff)
# make the NA CPUEs be 0s
fish$NUMCPUE[is.na(fish$NUMCPUE)] <- 0

# how does that look?
library(ggplot2)
p <- ggplot(fish) +
  geom_point(aes(x=x, y=y, colour=log(NUMCPUE)), size=0.4) +
  coord_equal() +
  scale_colour_viridis_c(begin=0.3, end=1, option="A", na.value="grey80") +
  theme_minimal() +
  theme(axis.text=element_blank(), axis.title=element_blank(), legend.position="bottom") +
  facet_wrap(~YEAR)
print(p)

xy_st <- st_as_sf(grid, coords=c("LONGITUDE", "LATITUDE"),
                  crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
xy_st <- st_transform(xy_st, proj)
rr <- st_rasterize(xy_st, deltax=37040, deltay=37040,
                   options = c("ALL_TOUCHED=TRUE"))

# this seems baaaaaaad
#library(akima)
#mk_interp <- function(grid, colname){
#  akima.li <- interp(grid$x, grid$y, grid[[colname]], duplicate = "median",
#                     xo=seq(min(grid$x), max(grid$x), by=37040),
#                     yo=seq(min(grid$y), max(grid$y), by=37040))
#  xx <- expand.grid(x=seq(min(grid$x), max(grid$x), by=37040),
#                    y=seq(min(grid$y), max(grid$y), by=37040))
#  xx[[colname]] <- as.vector(akima.li$z)
#  xx
#}
#ii <- cbind(mk_interp(grid, "BOT_TEMP"),
#            BOT_DEPTH=mk_interp(grid, "BOT_DEPTH")$BOT_DEPTH,
#            SURF_TEMP=mk_interp(grid, "SURF_TEMP")$SURF_TEMP)
#pred <- ii[!is.na(ii$SURF_TEMP), ]

# did that work?
#p <- ggplot(pred) +
#  geom_tile(aes(x=x, y=y, fill=BOT_DEPTH, width=37040, height=37040))
#print(p)

fish <- as.data.frame(fish)

fish$BOT_TEMP_ID <- as.factor(order(fish$BOT_TEMP))
fish$SURF_TEMP_ID <- as.factor(order(fish$SURF_TEMP))
fish$BOT_DEPTH_ID <- as.factor(order(fish$BOT_DEPTH))

#save(fish, pred, file="fish.RData")
save(fish, grid, file="fish.RData")


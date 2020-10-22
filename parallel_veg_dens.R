library(tictoc)


library(fs)
library(raster)
library(readr)
library(sf)
library(doParallel)
library(foreach)


mosaics = "Z:/DEC/MangroveMonitoring/Working/Camden_Sound/VegMachine_ARD/Data/index/mosaics"
rastkey = "AA.ers"
calibration = "./supplementary/calibration.csv"


## make tile grid
# create grid to tile with
# rasters to process
mos <- fs::dir_ls(mosaics, glob = paste0("*", rastkey, "$"))
r <- raster::raster(mos[1])
bb_poly <- st_as_sfc(st_bbox(r))

# muck around with cell size (tiles) to suit
grid <- bb_poly %>% 
  st_make_grid(cellsize = 30000, what = "polygons") %>% # grid of polys
  st_intersection(bb_poly)

# if you want to save it out as a whole grid to visualise
# grid_name <- "path/name.shp" # path must exist
# st_write(grid, grid_name)

# must save out individual tiles
vector_fold <-  "./vectors/tiles"
if (!file.exists(vector_fold)) {dir.create(vector_fold)}
for(i in 1:length(grid)){
  # name
  t_name <- paste0(vector_fold, "/tile_", sprintf("%03d", i), ".shp") # path must exist
  # write out
  st_write(grid[i], t_name)
}

# Create mini stacks - parallel code --------------------------------------
tic("pazza")
# Define how many cores (cpu processing limiting here)
UseCores <- 8

# Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)

# Full path to each tile
path_vectors <- "./vectors/tiles/"
vector_list <- list.files(path_vectors, pattern = ".shp$", full.names = TRUE)

# calibration file
calib <- readr::read_csv(calibration)
# cover formula
coef <- calib[["coef"]]
intercept <- calib[["intercept"]]
multiple <- calib[["multiple"]]
# make tile output folder
tile_fold <-  "./veg_dens/tiles"
if (!file.exists(tile_fold)) {dir.create(tile_fold)}
vdens <- function(x) (((x * coef) + intercept) * 100)

for(m in seq_along(mos)){
  r <- raster::raster(mos[m])
  #Use foreach loop and %dopar% command
  foreach(i = 1:length(vector_list)) %dopar% {
    #load libraries in foreach loop
    library(sf)
    library(raster)
    # make stack name - change substr start and stop to suit your data
    v_tile_name <- substr(vector_list[i], nchar(vector_list[i])-11, 
                          nchar(vector_list[i])-4)
    tile_name <- paste0(tile_fold, "/", v_tile_name) # path must exist
    # get tile vector
    tile <- st_read(vector_list[i])
    # crop stack
    crp <- crop(r, as.vector(st_bbox(tile))[c(1, 3, 2, 4)])
    
    # apply functions per pixel
    r2 <- raster::overlay(crp, fun = vdens)
    # clean
    r2[r2 < calib[["lower"]]] <- NA
    r2[r2 > calib[["upper"]]] <- NA
    
    # write out
    writeRaster(r2, tile_name, overwrite = TRUE)
  }
  ## Mosaic workflow
  path_output <- list.files(path = "./veg_dens/tiles/", 
                            pattern = ".grd", full.names = TRUE)
  
  l <- vector("list", length = length(path_output))
  
  for(i in 1:length(l)){
    r <- raster(path_output[i])
    l[i] <- r
  }
  
  # name for mosaicked data
  fname <- paste0("./veg_dens/", gsub("ndvi", "V_Dens", basename(mos[m])))
  l$filename <- gsub(".ers", ".tif", fname)
  l$fun <- mean
  l$na.rm <- FALSE
  
  # do
  do.call(mosaic, l)
}
toc(log = TRUE)

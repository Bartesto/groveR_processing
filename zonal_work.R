library(raster)
library(sf)
library(fasterize)
library(stringr)

setwd("Z:/DEC/MangroveMonitoring/Working/groveR_processing")











# thing to calculate on
r <- raster("./trend_class/LgCSMP_mangroves_2009-2019_trendclass.img")

plot(r)


# test extract for points
ext <- st_read(dsn ="./vectors/mini_extentAA.shp")
sites <- st_read(dsn ="./vectors/mini_test_extractAA.shp")


#stack to match trend calcs
rs <- fs::dir_ls("./veg_dens_mskd_for_trends", glob = "*.tif$")[21:30]
rst <- raster::stack(rs)
ministk <- crop(rst, ext)
plot(ministk)
d <- raster::extract(ministk, sites, df = TRUE)
names(d) <- c("id", paste0("y", 10:19))

library(tidyverse)

df <- d %>%
  mutate(id = c("major increase", "stable", "minor increase", "minor decrease", "major decrease")) %>%
  pivot_longer(-id, names_to = "col", values_to = "vegdens") %>%
  mutate(year = case_when(
    col == "y10" ~ 2010,
    col == "y11" ~ 2011,
    col == "y12" ~ 2012,
    col == "y13" ~ 2013,
    col == "y14" ~ 2014,
    col == "y15" ~ 2015,
    col == "y16" ~ 2016,
    col == "y17" ~ 2017,
    col == "y18" ~ 2018,
    col == "y19" ~ 2019,
    TRUE ~ 0
  ))

slps <- df %>%
  group_by(year) %>%
  mutate(
    slope = lm(vegdens ~ year)$coefficients[2])
  
ggplot(df) +
  geom_point(aes(x=year, y=vegdens)) +
  geom_line(aes(x=year, y=vegdens, colour = id)) +
  geom_smooth(aes(x=year, y=vegdens, colour = id), method = "lm", se = FALSE) +
  scale_colour_manual(values = c("red", "blue", "orange", "green", "grey")) +
  scale_x_continuous(breaks = c(2010:2019)) +
  scale_y_continuous(breaks = c(seq(from = 40, to = 100, by =2)),
                     sec.axis = dup_axis()) +
  theme_bw()


































# 2. Example of encoding cloud mask into layer avoiding water -------------


##EXAMPLE
# raster objects
r1 <- raster(ncols=36, nrows=18)
values(r1) <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
r2[r2 < 0.5] <- NA
r3 <- cover(r2, r1)


#raster to mask
path = "./veg_dens_mskd_for_trends/LgCSMP_Landsat_NBART_V_Dens_MskdTrends_1989_AA.tif"
start <- raster::raster(path)
# clip_sf <- st_read("./test/test_clip_AA.shp")
# rc <- crop(start, clip_sf) 
# plot(rc)

#cloud info
cloud_sf <- st_read("./test/USGS_Camden_mosaic_2007_ALL_utm51_cloudmask_AA_polygons.shp")
cloud_r <- fasterize::fasterize(cloud_sf, start)
cc <-  crop(cloud_r, clip_sf) 
plot(cc)

cc[cc == 1] <- 6 # code for cloud

outr <- cover(rc, cc)
plot(outr)

# previous raster
path2 = "./veg_dens_mskd/LgCSMP_Landsat_NBART_V_Dens_Mskd_2006_AA.tif"
prev <- raster(path2)
pc <- crop(prev, clip_sf) 
plot(pc)


# stack them 
st <- stack(pc, outr)

fun1 = function(x1, x2){ 
  ifelse(is.na(x1) & is.na(x2), NA,
         ifelse(x2 == 6 & is.na(x1), x1, x2))}

res <- overlay(st, fun=fun1)
plot(res)
writeRaster(res, filename = "test.tif")

y2011 <- raster::raster("./veg_dens_mskd_for_trends/LgCSMP_Landsat_NBART_V_Dens_MskdTrends_2011_AA.tif")
y2012 <- y2011
y2012[] <- NA
writeRaster(y2012, filename = "./veg_dens_mskd_for_trends/LgCSMP_Landsat_NBART_DUMMY_MskdTrends_2012_AA.tif")



## 

irast = "./trend_class/LgCSMP_mangroves_2010-2019_trendclass.img"
iregions = "./vectors/monitoring/mcr_LCSMP_ALL_parts_of_Park_monitoring_WAMMP_AA.shp"
attribname = "Stats_Regi"

trend_class_areas <- function(irast, iregions, attribname){
  suppressWarnings({
    regions <- sf::st_read(iregions)
    reps <- unique(regions$Stats_Regi)
    # trendclass raster
    tcs <- raster::raster(irast)
    out_list <- list()
    for(i in seq_along(reps)){
      # monitoring vector
      rep_i <- regions[i, attribname]
      name_r <- str_split(reps[i], "_")[[1]][1]
      name_s <- str_split(reps[i], "_")[[1]][2]
      cat(paste0("working on ", name_s, "...\n"))
      # make raster mask
      rep_ir <- fasterize::fasterize(sf = rep_i, raster = tcs)
      # mask out
      msk_ir <- raster::mask(x = tcs, mask = rep_ir)
      # calc freq
      stats <- tibble::as_tibble(raster::freq(msk_ir)) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(Region = name_r,
                      Site = name_s,
                      Area = count * 0.09,
                      TrendClass = case_when(
                        value == 1 ~ "Major Gain",
                        value == 2 ~ "Minor Gain",
                        value == 3 ~ "Stable",
                        value == 4 ~ "Minor Loss",
                        TRUE ~ "Major Loss"
                      )) %>%
        dplyr::select(-value, -count)
      out_list[[i]] <- stats
    }
    # output
    out_df <- do.call("rbind", out_list)
    o_name <- gsub(extension(irast), "_area_stats.csv", irast)
    readr::write_csv(out_df, path = o_name)
      
  })
}

irast = "./trend_class/LgCSMP_mangroves_2010-2019_trendclass.img"
iregions = "./vectors/fake_parksAA.shp"
attribname = "Stats_Regi"

regions <- sf::st_read(iregions)
reps <- unique(regions$Stats_Regi)
# trendclass raster
tcs <- raster::raster(irast)
out_list <- list()
for(i in seq_along(reps)){
  # monitoring vector
  rep_i <- regions[i, attribname]
  name_r <- str_split(reps[i], "_")[[1]][1]
  name_s <- str_split(reps[i], "_")[[1]][2]
  cat(paste0("working on ", name_s, "...\n"))
  # make raster mask
  rep_ir <- fasterize::fasterize(sf = rep_i, raster = tcs)
  # mask out
  msk_ir <- raster::mask(x = tcs, mask = rep_ir)
  # calc freq
  stats <- tibble::as_tibble(raster::freq(msk_ir)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(Region = name_r,
                  Site = name_s,
                  Area = count * 0.09,
                  TrendClass = case_when(
                    value == 1 ~ "Major Gain",
                    value == 2 ~ "Minor Gain",
                    value == 3 ~ "Stable",
                    value == 4 ~ "Minor Loss",
                    TRUE ~ "Major Loss"
                  )) %>%
    dplyr::select(-value, -count)
  out_list[[i]] <- stats
}
# output
out_df <- do.call("rbind", out_list)
o_name <- gsub(extension(irast), "_area_stats.csv", irast)
readr::write_csv(out_df, path = o_name)



getwd()
r <- raster::raster("./veg_dens/LgCSMP_Landsat_NBART_V_Dens_2017_AA.tif")
plot(r)

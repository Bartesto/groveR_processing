library(fs)
library(raster)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(sf)
setwd('Z:/DEC/MangroveMonitoring/Working/groveR_processing')

irast = "./veg_dens_mskd_for_trends"
rastkey = ".tif"
choice = c("1989", "2000", "2001")
imask = "./raster_masks/cloud_masks"
maskkey = ".ers"


# choice_fun <- function(choice, irast, rastkey){
#   todo <- tibble::tibble(rasters = fs::dir_ls(irast, 
#                                               glob = paste0("*", rastkey, "$"))) %>%
#     dplyr::mutate(yr = readr::parse_number(basename(rasters))) %>%
#     dplyr::filter(yr %in% choice) %>%
#     dplyr::pull(rasters)
# }


# do when you are catching up on a whole stack with multiple dates of cloudy and non
# cloudy scenes
specific_mask_bulk <- function(irast, rastkey, imask, maskkey){
  suppressWarnings({
    rastdf <- dplyr::tibble(path = fs::dir_ls(irast, 
                                              glob = paste0("*", 
                                                            rastkey, "$"))) %>%
      dplyr::mutate(pathnew = stringr::str_replace(path, "mskd_for_trends",
                                                   "mskd_for_class"),
                    pathnew = stringr::str_replace(pathnew, "MskdTrends_",
                                                   "MskdClass_"),
                    year = readr::parse_number(basename(path)))
    maskdf <- dplyr::tibble(path = fs::dir_ls(imask, 
                                              glob = paste0("*",
                                                            maskkey, "$"))) %>%
      dplyr::mutate(year = readr::parse_number(basename(path)))
    
    jdf <- dplyr::left_join(rastdf, maskdf, by = "year") %>%
      dplyr::mutate(cloud = !is.na(path.y))
    # output folder
    out <- "./veg_dens_mskd_for_class"
    if (!file.exists(out)) {dir.create(out)}
    if(jdf$cloud[[1]] == TRUE){
      # deal with first of time series having clouds but no prior year reference image
      s_rast <- raster::raster(jdf[[1]][1])
      c_rast <- raster::raster(jdf[[4]][1])
      cat("Masking...", basename(jdf[[1]][1]), "\n")
      # find mask val
      cmax <- raster::maxValue(c_rast)
      c_rast[c_rast == cmax] <- - 99 #make -99 so outside allowable limits for veg dens
      o_rast <- cover(s_rast, c_rast)
      raster::writeRaster(x = o_rast, filename = jdf[[2]][1], overwrite=TRUE)
      
      # deal with non- cloud affected images (i.e. just rename and move)
      cat("Copying and renaming unaffected images...",  "\n")
      copydf <- jdf %>%
        dplyr::filter(cloud == FALSE)
      fs::file_copy(copydf[[1]], copydf[[2]], overwrite=TRUE)
      
      # deal with cloud and prior reference image
      # function for later
      cldfun = function(x1, x2){ 
        ifelse(is.na(x1) & is.na(x2), NA,
               ifelse(x2 == -99 & is.na(x1), x1, x2))}
      # a df without first image
      jdf_short <- jdf %>%
        dplyr::slice(-1)
      # a character string of cloudy years
      cloud_yrs <- jdf_short %>%
        dplyr::filter(cloud == TRUE) %>%
        pull(year)
      
      for(i in seq_along(cloud_yrs)){
        # cloudy year
        yr <- cloud_yrs[i]
        # cloudy image name
        c_im <- jdf_short %>%
          dplyr::filter(year == yr) %>%
          pull(path.x)
        cat("Masking images with reference images...", basename(c_im), "\n")
        # prior image name
        p_im <- jdf_short %>%
          dplyr::filter(year == yr-1) %>%
          pull(path.x)
        # cloudy mask name
        cs_n <- jdf_short %>%
          dplyr::filter(year == yr) %>%
          pull(path.y)
        # cloudy mask
        cs_msk <- raster::raster(cs_n)
        csmax <- raster::maxValue(cs_msk)
        # cloudy raster to do, put in cloud
        c_rst <- raster::raster(c_im)
        cs_msk[cs_msk == csmax] <- - 99 # turn user mask vals to -99
        o_rst <- cover(c_rst, cs_msk)
        # bring in prior image
        p_rst <- raster::raster(p_im)
        st <- raster::stack(p_rst, o_rst)
        # now clean up based on prior image
        clean_rst <- overlay(st, fun = cldfun)
        # new name
        n_n <- jdf_short %>%
          dplyr::filter(year == yr) %>%
          pull(pathnew)
        writeRaster(clean_rst, filename = n_n, overwrite = TRUE, 
                    options=c('OVR=YES'))
      }
    } else{
      # deal with non- cloud affected images (i.e. just rename and move)
      cat("Copying and renaming unaffected images...",  "\n")
      copydf <- jdf %>%
        dplyr::filter(cloud == FALSE)
      fs::file_copy(copydf[[1]], copydf[[2]], overwrite=TRUE)
      
      # deal with cloud and prior reference image
      # function for later
      cldfun = function(x1, x2){ 
        ifelse(is.na(x1) & is.na(x2), NA,
               ifelse(x2 == -99 & is.na(x1), x1, x2))}
      # clouds years left to do
      cloud_yrs <- jdf %>%
        dplyr::filter(cloud == TRUE) %>%
        pull(year)
      
      for(i in seq_along(cloud_yrs)){
        # cloudy year
        yr <- cloud_yrs[i]
        # cloudy image name
        c_im <- jdf %>%
          dplyr::filter(year == yr) %>%
          pull(path.x)
        cat("Masking images with reference images...", basename(c_im), "\n")
        # prior image name
        p_im <- jdf%>%
          dplyr::filter(year == yr-1) %>%
          pull(path.x)
        # cloudy mask name
        cs_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          pull(path.y)
        # cloudy mask
        cs_msk <- raster::raster(cs_n)
        csmax <- raster::maxValue(cs_msk)
        # cloudy raster to do, put in cloud
        c_rst <- raster::raster(c_im)
        cs_msk[cs_msk == csmax] <- - 99
        o_rst <- cover(c_rst, cs_msk)
        # bring in prior image
        p_rst <- raster::raster(p_im)
        st <- raster::stack(p_rst, o_rst)
        # now clean up based on prior image
        clean_rst <- overlay(st, fun = cldfun)
        # new name
        n_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          pull(pathnew)
        writeRaster(clean_rst, filename = n_n, overwrite = TRUE, 
                    options=c('OVR=YES'))
      }
    }
  })
}

specific_mask_bulk(irast, rastkey, imask, maskkey)


specific_mask_select <- function(irast, rastkey, choice, imask, maskkey){
  suppressWarnings({
    rastdf <- dplyr::tibble(path = fs::dir_ls(irast, 
                                              glob = paste0("*", 
                                                            rastkey, "$"))) %>%
      dplyr::mutate(pathnew = stringr::str_replace(path, "mskd_for_trends",
                                                   "mskd_for_class"),
                    pathnew = stringr::str_replace(pathnew, "MskdTrends_",
                                                   "MskdClass_"),
                    year = readr::parse_number(basename(path)))
    maskdf <- dplyr::tibble(path = fs::dir_ls(imask, 
                                              glob = paste0("*",
                                                            maskkey, "$"))) %>%
      dplyr::mutate(year = readr::parse_number(basename(path))) %>%
      dplyr::filter(year %in% choice)
    jdf <- dplyr::left_join(rastdf, maskdf, by = "year") %>%
      dplyr::mutate(cloud = !is.na(path.y))
    cldfun <- function(x1, x2){ 
      ifelse(is.na(x1) & is.na(x2), NA,
             ifelse(x2 == -99 & is.na(x1), x1, x2))}
    # output folder
    out <- "./veg_dens_mskd_for_class"
    if (!file.exists(out)) {dir.create(out)}
    for(i in seq_along(choice)){
      yr1 <- jdf[[3]][1]
      cloudyr1 <- jdf[[5]][1]
      cdata <- jdf %>%
        dplyr::filter(year == as.numeric(choice[i])) %>%
        dplyr::pull(cloud)
      if(choice[i] == yr1 & cloudyr1 == TRUE){
        # deal with first of time series having clouds but no prior year reference image
        s_rast <- raster::raster(jdf[[1]][1])
        c_rast <- raster::raster(jdf[[4]][1])
        cat("Masking...", basename(jdf[[1]][1]), "\n")
        # find mask val
        cmax <- raster::maxValue(c_rast)
        c_rast[c_rast == cmax] <- - 99 #make -99 so outside allowable limits for veg dens
        o_rast <- cover(s_rast, c_rast)
        raster::writeRaster(x = o_rast, filename = jdf[[2]][1], overwrite=TRUE)
      } else if(choice[i] != yr1 & cdata == FALSE){
        # deal with non- cloud affected images (i.e. just rename and move)
        cat("Copying and renaming unaffected image...",  "\n")
        copydf <- jdf %>%
          dplyr::filter(year == choice[i])
        fs::file_copy(copydf[[1]], copydf[[2]], overwrite=TRUE)
      } else if(choice[i] != yr1 & cdata == TRUE){
        yr <- as.numeric(choice[i])
        # cloudy image name
        c_im <- jdf %>%
          dplyr::filter(year == yr) %>%
          pull(path.x)
        cat("Masking images with reference images...", basename(c_im), "\n")
        # prior image name
        p_im <- jdf %>%
          dplyr::filter(year == yr-1) %>%
          pull(path.x)
        # cloudy mask name
        cs_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          pull(path.y)
        # cloudy mask
        cs_msk <- raster::raster(cs_n)
        csmax <- raster::maxValue(cs_msk)
        # cloudy raster to do, put in cloud
        c_rst <- raster::raster(c_im)
        cs_msk[cs_msk == csmax] <- - 99
        o_rst <- cover(c_rst, cs_msk)
        # bring in prior image
        p_rst <- raster::raster(p_im)
        st <- raster::stack(p_rst, o_rst)
        # now clean up based on prior image
        clean_rst <- overlay(st, fun = cldfun)
        # new name
        n_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          pull(pathnew)
        writeRaster(clean_rst, filename = n_n, overwrite = TRUE,
                    options=c('OVR=YES'))
      }
      
    }
    
  })
}
specific_mask_select(irast, rastkey, choice, imask, maskkey)

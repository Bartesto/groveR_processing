library(tictoc)
library(fs)
library(raster)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(sf)

# tic("total time")

# tic("mosaic to veg density")
#options=c('OVR=YES')

## helpers
# function to selectively choose input rasters (not do all)



choice_fun <- function(choice, irast, rastkey){
  todo <- tibble::tibble(rasters = fs::dir_ls(irast, 
                                            glob = paste0("*", rastkey, "$"))) %>%
    dplyr::mutate(yr = readr::parse_number(basename(rasters))) %>%
    dplyr::filter(yr %in% choice) %>%
    dplyr::pull(rasters)
}





# 1. Make veg density -----------------------------------------------------
## Makes a veg density product from a mosaic according to a calibration csv
irast = "Z:/DEC/MangroveMonitoring/Working/LGMPs/Data/Landsat/Mosaics"
rastkey = "AA.ers"
choice = "2020"
calibration = "./supplementary/calibration.csv"

veg_dens <- function(irast, rastkey, choice, calibration){
  suppressWarnings({
    # rasters to process
    if(choice == "all"){
      mos <- fs::dir_ls(irast, glob = paste0("*", rastkey, "$"))
    } else {
      mos <- choice_fun(choice, irast, rastkey)
    }
    # calibration file
    calib <- readr::read_csv(calibration)
    # cover formula
    coef <- calib[["coef"]]
    intercept <- calib[["intercept"]]
    multiple <- calib[["multiple"]]
    vdens <- function(x) (((x * coef) + intercept) * 100)
    # output folder
    out <- "./veg_dens"
    if (!file.exists(out)) {dir.create(out)}
    # process rasters
    for(i in seq_along(mos)){
      mos1 <- mos[i]
      cat("Calculating vegetation density..." , basename(mos1), "\n")
      r1 <- raster::raster(mos1)
      # apply functions per pixel
      r2 <- raster::overlay(r1, fun = vdens)
      # clean
      r2[r2 < calib[["lower"]]] <- NA
      r2[r2 > calib[["upper"]]] <- NA
      # write output to file
      fname <- paste0("./veg_dens/", gsub("ndvi", "V_Dens", basename(mos1)))
      raster::writeRaster(r2, filename = gsub(".ers", ".tif", fname), 
                          overwrite = TRUE, options=c('OVR=YES'))
    }
  })
}

veg_dens(irast, rastkey, choice, calibration)
toc(log = TRUE) #- 3.5 hrs


tic("mask product")

# 2. Do generalised masking -----------------------------------------------

choice_fun <- function(choice, irast, rastkey){
  todo <- tibble::tibble(rasters = fs::dir_ls(irast, 
                                              glob = paste0("*", rastkey, "$"))) %>%
    dplyr::mutate(yr = readr::parse_number(basename(rasters))) %>%
    dplyr::filter(yr %in% choice) %>%
    dplyr::pull(rasters)
}
## Masks out features that would appear in all years
irast = "Z:/DEC/MangroveMonitoring/Working/groveR_processing/veg_dens"
rastkey = ".tif"
choice = "2001"
imask = "Z:/DEC/MangroveMonitoring/Working/groveR_processing/raster_masks"
maskkey = ".img"

mask_product <- function(irast, rastkey, choice, imask, maskkey){
  suppressWarnings({
    if(choice == "all"){
      rastdf <- dplyr::tibble(path = fs::dir_ls(irast, 
                                                glob = paste0("*", 
                                                              rastkey, "$"))) %>%
        dplyr::mutate(bname = stringr::str_replace(basename(path), "Dens_",
                                                   "Dens_MskdTrends_"))
    } else {
      rastdf <- dplyr::tibble(path = fs::dir_ls(irast, 
                                                glob = paste0("*", 
                                                              rastkey, "$"))) %>%
        dplyr::mutate(yr = readr::parse_number(basename(path))) %>%
        dplyr::filter(yr %in% choice) %>%
        dplyr::mutate(bname = stringr::str_replace(basename(path), "Dens_",
                                                   "Dens_MskdTrends_"))
    }
    
    maskdf <- dplyr::tibble(path = fs::dir_ls(imask, 
                                              glob = paste0("*",
                                                            maskkey, "$"))) %>%
      dplyr::mutate(inverse = stringr::str_detect(basename(path), "INV"))
    # output folder
    out <- "./veg_dens_mskd_for_trends"
    if (!file.exists(out)) {dir.create(out)}
    for(i in seq_along(rastdf[[1]])){
      ir1 <-  raster::raster(rastdf[[1]][i])
      cat("Masking...", basename(rastdf[[1]][i]), "\n")
      for(j in seq_along(maskdf[[1]])){
        m <- raster::raster(maskdf[[1]][j])
        if(maskdf[[2]][j] == FALSE){
          ir1 <- raster::mask(ir1, m)
        } else {
          ir1 <- raster::mask(ir1, m, inverse = TRUE)
        }
      }
      mskdname <- paste0(out, "/", rastdf[[3]][i])
      raster::writeRaster(ir1, filename = mskdname, overwrite = TRUE, 
                          options=c('OVR=YES'))
    }
  })
}

mask_product(irast, rastkey, choice, imask, maskkey)
toc(log = TRUE) #- 15 mins



# 3. Do specific masking --------------------------------------------------

## Encodes rasters with values for features that don't appear in all layers
tic("cloud biz")

## For bulk stack
irast = "./veg_dens_mskd_for_trends"
rastkey = ".tif"
imask = "./raster_masks/cloud_masks"
maskkey = ".ers"

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
toc(log = TRUE)

## Encodes rasters with values for features that don't appear in all layers
tic("cloud biz2")

## For redoing old select images or newly added years
irast = "./veg_dens_mskd_for_trends"
rastkey = ".tif"
choice = c(1989, 2000, 2001)
imask = "./raster_masks/cloud_masks"
maskkey = ".ers"

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
toc(log = TRUE)


# 4. Do veg density classification ----------------------------------------
tic("veg class probs")
irast = "./veg_dens_mskd_for_class"
rastkey = ".tif"
imask = "./raster_masks/cloud_masks"
maskkey = ".ers"
classes = "./supplementary/density_classes.csv"

veg_class_probs <- function(irast, rastkey, imask, maskkey, classes){
  suppressWarnings({
    # get classes
    cl <- as.matrix(readr::read_csv(classes))
    # dealing with first image cloudy?
    rastdf <- dplyr::tibble(path = fs::dir_ls(irast, 
                                              glob = paste0("*", 
                                                            rastkey, 
                                                            "$"))) %>%
      dplyr::mutate(pathnew = stringr::str_replace(path, "dens_mskd_for_class",
                                                   "class_cloud_prob"),
                    pathnew = stringr::str_replace(pathnew, "V_Dens_MskdClass_",
                                                   "Veg_Class_Cloud_Probabilities_"),
                    year = readr::parse_number(basename(path)))
    maskdf <- dplyr::tibble(path = fs::dir_ls(imask, 
                                              glob = paste0("*",
                                                            maskkey, "$"))) %>%
      dplyr::mutate(year = readr::parse_number(basename(path)))
    jdf <- dplyr::left_join(rastdf, maskdf, by = "year") %>%
      dplyr::mutate(cloud = !is.na(path.y))
    out <- "./veg_class_cloud_prob"
    if (!file.exists(out)) {dir.create(out)}
    if(jdf$cloud[[1]] == TRUE){
      # deal with first of time series having clouds but no prior year reference image
      s_rast <- raster::raster(jdf[[1]][1])
      cat("Classifying...", basename(jdf[[1]][1]), "\n")
      rcl <- raster::reclassify(s_rast, cl)
      # grab a non cloudy to effect some masking
      nci <- jdf %>%
        dplyr::filter(cloud == FALSE) %>%
        pull(path.x)
      nci <- nci[1]
      nci_rst <- raster::raster(nci)
      st <- raster::stack(nci, rcl)
      mskfun = function(x1, x2){
        ifelse(x2 == 6 & is.na(x1), NA, x2)
      }
      cl_rst <- raster::overlay(st, fun = mskfun)
      
      writeRaster(x = cl_rst, filename = jdf[[2]][1], datatype = 'INT1U', 
                  overwrite=TRUE)
      
      # classify non cloud affected images
      jdf2 <- jdf %>%
        dplyr::filter(cloud == FALSE)
      for(i in seq_along(jdf2[[1]])){
        cat("Classifying...", basename(jdf2[[1]][i]), "\n")
        rst <- raster::raster(jdf2[[1]][i])
        rcl <- raster::reclassify(rst, cl)
        writeRaster(x = rcl, filename = jdf2[[2]][i], datatype = 'INT1U', 
                    overwrite=TRUE)
      }
      # classify cloudy using previous image
      # a df without first image
      jdf_short <- jdf %>%
        dplyr::slice(-1)#important for when first image is cloudy
      cloud_yrs <- jdf_short %>%
        dplyr::filter(cloud == TRUE) %>%
        pull(year)
      # function for later
      cldfun2 = function(x1, x2){ 
        ifelse(is.na(x1) & is.na(x2), NA,
               ifelse(x2 == 6 & is.na(x1), x1,
                      ifelse(x2 == 6 & !is.na(x1), x1 + 10, x2)))}
      for(j in seq_along(cloud_yrs)){
        yr <- cloud_yrs[j]
        c_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          dplyr::pull(path.x)
        cat("Classifying...", basename(c_n), "\n")
        p_n <- jdf %>%
          dplyr::filter(year == yr - 1) %>%
          dplyr::pull(pathnew)#bring in classified image
        # current cloudy raster
        c_rst <- raster::raster(c_n)
        # classify it
        c_rst_c <- raster::reclassify(c_rst, cl)
        # previously already classified yr
        p_rst <- raster::raster(p_n)
        # stack it
        st <- raster::stack(p_rst, c_rst_c)
        # now clean up based on prior image
        clean_rst <- overlay(st, fun = cldfun2)
        o_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          dplyr::pull(pathnew)
        writeRaster(x = clean_rst, filename = o_n, datatype = "INTU",
                    overwrite = TRUE)
      }
      
    } else {
      # no cloudy first image so move straight on
      # classify non cloud affected images
      jdf2 <- jdf %>%
        dplyr::filter(cloud == FALSE)
      for(i in seq_along(jdf2[[1]])){
        cat("Classifying...", basename(jdf2[[1]][i]), "\n")
        rst <- raster::raster(jdf2[[1]][i])
        rcl <- raster::reclassify(rst, cl)
        writeRaster(x = rcl, filename = jdf2[[2]][i], datatype = 'INT1U', 
                    overwrite=TRUE)
      }
      # classify cloudy using previous image
      cloud_yrs <- jdf %>%
        dplyr::filter(cloud == TRUE) %>%
        pull(year)
      # function for later
      cldfun2 = function(x1, x2){ 
        ifelse(is.na(x1) & is.na(x2), NA,
               ifelse(x2 == -99 & is.na(x1), x1,
                      ifelse(x2 == -99 & !is.na(x1), x1 + 10, x1)))}
      for(j in seq_along(cloud_yrs)){
        yr <- cloud_yrs[j]
        c_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          dplyr::pull(path.x)
        cat("Classifying...", basename(c_n), "\n")
        p_n <- jdf %>%
          dplyr::filter(year == yr - 1) %>%
          dplyr::pull(pathnew)#bring in classified image
        # current cloudy raster
        c_rst <- raster::raster(c_n)
        # classify it
        c_rst_c <- raster::reclassify(c_rst, cl)
        # previously already classified yr
        p_rst <- raster::raster(p_n)
        # stack it
        st <- raster::stack(p_rst, c_rst_c)
        # now clean up based on prior image
        clean_rst <- overlay(st, fun = cldfun2)
        o_n <- jdf %>%
          dplyr::filter(year == yr) %>%
          dplyr::pull(pathnew)
        writeRaster(x = clean_rst, filename = o_n, datatype = "INTU",
                    overwrite = TRUE)
      }
    }
  })
}

veg_class_probs(irast, rastkey, imask, maskkey, classes)

toc(log=TRUE)


## rethink the below
# tic("veg class")
# irast = "./veg_dens_mskd_for_class"
# rastkey = ".tif"
# cloud_prob = TRUE
# imask = "./raster_masks/cloud_masks"
# classes = "./supplementary/density_classes.csv"
# 
# veg_class <- function(irast, rastkey, cloud_prob = FALSE, imask = NULL){
#   suppressWarnings({  })
#   }







tic("veg class areas")
irast = "./veg_class_cloud_prob"
rastkey = ".tif"
dummies = c("2012", "1999")
iregions = "./vectors/monitoring/mcr_LCSMP_ALL_parts_of_Park_monitoring_WAMMP_AA.shp"
attribname = "Stats_Regi"

setwd("Z:/DEC/MangroveMonitoring/Working/groveR_processing")

veg_class_area <- function(irast, rastkey, dummies, iregions, attribname){
  suppressWarnings({
    irs <- fs::dir_ls(irast, glob = paste0("*", rastkey, "$"))
    rastdf <- dplyr::tibble(path = irs) %>%
      dplyr::mutate(yr = readr::parse_number(basename(path))) %>%
      dplyr::filter(!yr %in% dummies)
    regions <- sf::st_read(iregions)
    reps <- unique(regions[[attribname]])
    # output folder
    out <- "./products"
    if (!file.exists(out)) {dir.create(out)}
    # stack rasters
    rsk <- raster::stack(rastdf[[1]])
    stats <- tibble::tibble()
    cat("Calculating veg classes... \n")
    for(i in seq_along(reps)){
      rep_i <- regions[i, attribname]
      name_r <- str_split(reps[i], "_")[[1]][1]
      name_s <- str_split(reps[i], "_")[[1]][2]
      # make raster mask
      rep_ir <- fasterize::fasterize(sf = rep_i, raster = rsk[[1]])
      # mask out
      msk_ir <- raster::mask(x = rsk, mask = rep_ir)
      # calc freq table on stack
      stk <- raster::freq(msk_ir)
      # sensible stack names
      s_layer_names <- paste0(sapply(str_split(basename(rastdf[[1]]), "_"), "[[", 1), "_",
                              rastdf[[2]])
      names(stk) <- s_layer_names
      # area outputs
      out_df <- stk %>%
        purrr::map_df(~ as.data.frame(.x), .id = "id") %>%
        dplyr::mutate(Region = name_r,
                      Site = name_s,
                      Area = count * 0.09,
                      DensityClass = case_when(
                        value == 1 ~ '10-19%',
                        value == 2 ~ '20-29%',
                        value == 3 ~ '30-49%',
                        value == 4 ~ '50-69%',
                        value == 5 ~ '70-100%',
                        value == 6 ~ 'Cloud',
                        value == 11 ~ 'Cloud 10-19%',
                        value == 12 ~ 'Cloud 20-29%',
                        value == 13 ~ 'Cloud 30-49%',
                        value == 14 ~ 'Cloud 50-69%',
                        value == 15 ~ 'Cloud 70-100%',
                        TRUE ~ "Other"
                      ),
                      Habitat = case_when(
                        value == 1 ~ "Very Sparse Mangroves",
                        value >= 2 & value <= 5 ~ "Mangroves",
                        value == 6 ~ "Cloud",
                        value == 11 ~ "Cloud likely Very Sparse Mangroves",
                        value >= 12 & value <= 15 ~ "Cloud likely Mangrove",
                        TRUE ~ "Other"
                      ),
                      Density = case_when(
                        value == 1 ~ "Very Sparse",
                        value == 2 ~ "Sparse",
                        value == 3 ~ "Sparse - Medium",
                        value == 4 ~ "Medium - Dense",
                        value == 5 ~ "Dense",
                        value == 6 ~ "Cloud",
                        value == 11 ~ "Cloud likely Very Sparse",
                        value == 12 ~ "Cloud likely Sparse",
                        value == 13 ~ "Cloud likely Sparse - Medium",
                        value == 14 ~ "Cloud likely Medium - Dense",
                        value == 15 ~ "Cloud likely Dense",
                        TRUE ~ "Other"
                      ),
                      Year = readr::parse_number(id)) %>%
        dplyr::select(-count, -value, -id)
      stats <- dplyr::bind_rows(stats, out_df)
    }
    # find start end year
    ayrs <- unique(stats$Year)
    yrs <- paste0("_", min(ayrs), "-", max(ayrs), "_")
    # park
    park <- unlist(str_split(stats$Region[1], "_"))[1]
    # output name
    oname <- paste0(out, "/", park, yrs, "extent_summaries.csv")
    readr::write_csv(stats, path = oname)
  })
}
veg_class_area(irast, rastkey, dummies, iregions, attribname)
toc(log=TRUE)



tic("trend class")
irast = "./veg_dens_mskd_for_trends"
rastkey = ".tif"
end = "2001"
period = 10
classes = "./supplementary/trend_classes.csv"

trend_class <- function(irast, rastkey, end, period, classes){
  suppressWarnings({
    irs <- fs::dir_ls(irast, glob = paste0("*", rastkey, "$"))
    # make stack and sensible names
    stk <- raster::stack(irs)
    s_layer_names <- readr::parse_number(basename(irs))
    names(stk) <- s_layer_names
    # subset stack
    stk_e <- grep(pattern = end, names(stk))
    stk_s <- stk_e - (period - 1)
    trnd_stk <- stk[[stk_s:stk_e]]
    # output folder
    out <- "./trend_class"
    if (!file.exists(out)) {dir.create(out)}
    # per pixel lm
    time <- 1:raster::nlayers(trnd_stk)
    c_off <- length(time)/2 # min data for lm choice
    lin_fun <- function(x) {
      if (sum(is.na(x)) > c_off) {
        NA
      } else {
        m = lm(x ~ time)
        m$coefficients[2]
      }
    }
    trend <- raster::calc(trnd_stk, lin_fun)
    # get classes
    cl <- as.matrix(readr::read_csv(classes))
    # reclassify to trend class
    trnd_cl <- raster::reclassify(trend, cl)
    # name and save
    ayrs <- parse_number(names(trnd_stk))
    yrs <- paste0("_", min(ayrs), "-", max(ayrs), "_")
    park <- unlist(str_split(basename(irs[1]), "_"))[1]
    oname <- paste0(out, "/", park, "_mangroves", yrs, "trendclass.img")
    tname <- paste0(out, "/", park, "_mangroves", yrs, "trend.img")
    raster::writeRaster(trnd_cl, filename = oname, datatype = 'INT1U', 
                        overwrite = TRUE)
    raster::writeRaster(trend, filename = tname, datatype = 'FLT4S', 
                        overwrite = TRUE)
  })
}

trend_class(irast, rastkey, end, period, classes)
toc(log = TRUE) # 20 mins

tic("trend class area")
irast = "./trend_class/LgCSMP_mangroves_2010-2019_trendclass.img"
iregions = "./vectors/monitoring/mcr_LCSMP_ALL_parts_of_Park_monitoring_WAMMP_AA.shp"
attribname = "Stats_Regi"

trend_class_areas <- function(irast, iregions, attribname){
  suppressWarnings({
    regions <- sf::st_read(iregions)
    reps <- unique(regions[[attribname]])
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

trend_class_areas(irast, iregions, attribname)

toc(log = TRUE) # 20 secs!

## change stuff
tic("extent change")
irast = "./veg_class_cloud_prob"
rastkey = ".tif"
dummies = c("2012", "1999")
iregions = "./vectors/monitoring/mcr_LCSMP_ALL_parts_of_Park_monitoring_WAMMP_AA.shp"
attribname = "Stats_Regi"

change_extent <- function(irast, rastkey, dummies, iregions, attribname){
  suppressWarnings({
    irs <- rev(fs::dir_ls(irast, glob = paste0("*", rastkey, "$")))
    cdate <- Sys.Date()
    rastdf <- dplyr::tibble(path = irs) %>%
      dplyr::mutate(yr = readr::parse_number(basename(path))) %>%
      dplyr::mutate(pathnew = stringr::str_replace(path, "veg_class_cloud_prob",
                                                   "extent_change"),
                    pathnew1 = stringr::str_replace(pathnew, "Veg_Class_Cloud_Probabilities_",
                                                   "extent_change_"),
                    pathnew2 = stringr::str_replace(pathnew1, "AA.tif",
                                                   paste0(yr-1, ".tif")),
                    csv_name = stringr::str_replace_all(pathnew1, c("_AA.tif" = ".csv")),
                    csv_name = gsub(yr, cdate, csv_name)) %>%
      dplyr::filter(!yr %in% dummies)
    # iterator needs to be one less to deal with last year not having a prior :)
    end <- length(rastdf[[1]])- 1
    #classification matrix for extent calcs
    rcl <- c(0, 1 , 1,
             1, 5, 2,
             5, 6 , 6,
             6, 11, 7,
             11, 15, 8,
             15, Inf, NA)
    rclm <- matrix(rcl, ncol = 3, byrow = TRUE)
    
    out <- "./extent_change"
    if (!file.exists(out)) {dir.create(out)}
    # parks/monitoring regions
    regions <- sf::st_read(iregions)
    reps <- unique(regions[[attribname]])
    stats <- tibble::tibble()
    for(i in 1:end){
      cat("Calculating change between...", rastdf[[2]][i], "and", 
          rastdf[[2]][i + 1], "\n")
      # b relates to current year (remember file path order reversed - most recent first)
      b1 <- raster::raster(rastdf[[1]][i])
      b <- raster::reclassify(b1, rclm)
      # a relates to prior year
      a1 <- raster::raster(rastdf[[1]][i + 1])
      a <- raster::reclassify(a1, rclm)
      # empty raster based on other
      c = raster(a); c[] = NA
      # classifications based on Kathy M rules
      c[a==1 & b==1] <- NA
      c[a==2 & b==1] <- 11
      c[a==6 & b==1] <- NA
      c[a==7 & b==1] <- NA
      c[a==8 & b==1] <- 14
      c[is.na(a) & b==1] <- NA
      c[a==1 & b==2] <- 10
      c[a==2 & b==2] <- 12
      c[a==6 & b==2] <- 16
      c[a==7 & b==2] <- 13
      c[a==8 & b==2] <- 15
      c[is.na(a) & b==2] <- 10
      c[a==1 & b==6] <- NA 
      c[a==2 & b==6] <- 16  
      c[a==6 & b==6] <- NA 
      c[a==7 & b==6] <- NA 
      c[a==8 & b==6] <- 14 
      c[is.na(a) & b==6] <- NA 
      c[a==1 & b==7] <- NA
      c[a==2 & b==7] <- 14
      c[a==6 & b==7] <- NA
      c[a==7 & b==7] <- NA
      c[a==8 & b==7] <- 14
      c[is.na(a) & b==7] <- NA
      c[a==1 & b==8] <- 13
      c[a==2 & b==8] <- 15
      c[a==6 & b==8] <- 13 
      c[a==7 & b==8] <- 13
      c[a==8 & b==8] <- 15
      c[is.na(a) & b==8] <- 13
      c[a==1 & is.na(b)] <- NA
      c[a==2 & is.na(b)] <- 11
      c[a==6 & is.na(b)] <- NA
      c[a==7 & is.na(b)] <- NA
      c[a==8 & is.na(b)] <- 14
      c[is.na(a) & is.na(b)] <- NA
      # output raster
      writeRaster(c, filename = rastdf[[5]][i], overwrite = TRUE)
      for(j in seq_along(reps)){
        # monitoring vector
        rep_j <- regions[j, attribname]
        name_r <- str_split(reps[j], "_")[[1]][1]
        name_s <- str_split(reps[j], "_")[[1]][2]
        # make raster mask
        rep_ir <- fasterize::fasterize(sf = rep_j, raster = c)
        # mask out
        c_m <- raster::mask(x = c, mask = rep_ir)
        # area calcs
        period <- paste0(rastdf[[2]][i], "-", rastdf[[2]][i +1])
        out_df <-  tibble::as_tibble(raster::freq(c_m)) %>%
          dplyr::mutate(Region = name_r,
                        Site = name_s,
                        Area = count * 0.09,
                        Status = case_when(
                          value == 10 ~ 'gain',
                          value == 11 ~ 'loss',
                          value == 12 ~ 'stable',
                          value == 13 ~ 'cloud likely gain',
                          value == 14 ~ 'cloud likely loss',
                          value == 15 ~ 'cloud likely stable',
                          value == 16 ~ 'cloud no data',
                          TRUE ~ "NA"
                        ),
                        Period = period) %>%
          dplyr::select(-value, -count)
        stats <- dplyr::bind_rows(stats, out_df)
      }
      
    }
    cname <- rastdf[[6]][1]
    write_csv(stats, path = cname)
  })}
change_extent(irast, rastkey)
toc(log = TRUE)




# Plots -------------------------------------------------------------------


icsv = "./products/All MPA_1989-2019_extent_summaries.csv"

veg_dens_class_plot <- function(icsv){
  dens_class_cols <- c('10-19%' = "#33FE31",
                       '20-29%' = "#20CB27",
                       '30-49%' = "#109A1D",
                       '50-69%' = "#046C13",
                       '70-100%' = "#004108",
                       'Cloud' = "black",
                       'Cloud 10-19%' = "#AFEF5A",
                       'Cloud 20-29%' = "#8EC045",
                       'Cloud 30-49%' = "#6E9232",
                       'Cloud 50-69%' = "#4F6820",
                       'Cloud 70-100%' = "#314010")
  # summarise data
  df <-  readr::read_csv(icsv) %>%
    dplyr::filter(DensityClass != "Other") %>%
    dplyr::group_by(Region, Site, Year, DensityClass) %>%
    dplyr::summarise(a = sum(Area))
  
  sites <- unique(df$Site)
  for(i in seq_along(sites)){
    site <- sites[i]
    df2 <- df %>%
      dplyr::filter(Site == site)
    # helpers
    yr_range <- paste0(min(df$Year), '-', max(df$Year))
    scalex <- min(df$Year):max(df$Year)
    
    p  <- ggplot(df2, aes(x = Year, y = a, fill = DensityClass)) +
      geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(values = dens_class_cols) +
      scale_x_continuous(breaks = scalex) +
      labs(title = paste("Mangrove density class summary", yr_range),
           subtitle = paste(df2$Region[1], df2$Site[1]),
           y = "Area (ha)",
           x = "",
           caption = expression(italic("Brought to you by groveR"))) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=70, vjust = 0.5))
    
    pname <- paste0(dirname(icsv), "/", gsub(" ", "_", df2$Site[1]),"_", 
                    yr_range, ".png")
    ggsave(p, filename = pname, width = 10, height = 7)
    return(p)
  }
  
}

veg_dens_class_plot(icsv)


icsv = "./extent_change/LgCSMP_Landsat_NBART_extent_change_2020-09-25.csv"

ext_chng_plot <- function(icsv){
  ext_chng_cols <- c('gain' = '#0000CC',
                     'stable' = '#808080',
                     'loss' = '#FF0000',
                     'cloud no data' = 'black',
                     'cloud likely gain' = '#00CCFF',
                     'cloud likely stable' = '#D9D9D9',
                     'cloud likely loss' = "#FF9999"
  )
  # summarise data - factors for plotting order for Status
  df <-  readr::read_csv(icsv) %>%
    dplyr::filter(!is.na(Status)) %>%
    dplyr::group_by(Region, Site, Period, Status) %>%
    dplyr::summarise(a = sum(Area)) %>%
    dplyr::mutate(Status = factor(Status, 
                                  levels = c('loss', 'cloud likely loss','gain', 'cloud likely gain', 
                                             'stable', 'cloud likely stable',
                                             'cloud no data', NA)))
  sites <- unique(df$Site)
  for(i in seq_along(sites)){
    site <- sites[i]
    df2 <- df %>%
      dplyr::filter(Site == site)
    # helpers
    as_of <- Sys.Date()
    
    p  <- ggplot(df2, aes(x = Period, y = a, fill = Status)) +
      geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(values = ext_chng_cols) +
      labs(title = "Mangrove change summaries",
           subtitle = paste(df2$Region[1], df2$Site[1]),
           y = "Area (ha)",
           x = "",
           caption = expression(italic("Brought to you by groveR"))) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=70, vjust = 0.5))
    
    pname <- paste0(dirname(icsv), "/", gsub(" ", "_", df2$Site[1]),"_", 
                    as_of, ".png")
    ggsave(p, filename = pname, width = 9, height = 7)
    return(p)
  }
}

ext_chng_plot(icsv)
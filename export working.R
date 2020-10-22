library(raster)
library(fs)
library(sf)
library(tidyverse)
library(purrr)


irast = "./veg_class_mskd"
rastkey = ".tif"

# get rasters
irs <- fs::dir_ls(irast, glob = paste0("*", rastkey, "$"))
# stack rasters
rsk <- raster::stack(irs)
# calc freq table on stack
out <- raster::freq(rsk)
# sensible list names
s_layer_names <- paste0(sapply(str_split(basename(irs), "_"), "[[", 1), "_",
                        parse_number(basename(irs)))
names(out) <- s_layer_names


x = out[[1]]

clean_tibbs <- function(x) {
  as_tibble(x) %>% dplyr::mutate(area = count * 0.09,
                                    name = names(x),
                                    class = case_when(
                                      value == 1 ~ '10-19',
                                      value == 2 ~ '20-29',
                                      value == 3 ~ '30-49',
                                      value == 4 ~ '50-69',
                                      value == 5 ~ '70-100',
                                      TRUE ~ "other"
                                    ))
}
lapply(out, FUN = clean_tibbs)

out1 <- out[1]
a <- out1 %>%
  as_tibble %>%
  mutate(layer = names(out1))


freq_table_tibble <- function(f){
  f %>%
    bind_rows %>%
    dplyr::mutate(area = count * 0.09,
                  name = names(f),
                  class = case_when(
                    value == 1 ~ '10-19',
                    value == 2 ~ '20-29',
                    value == 3 ~ '30-49',
                    value == 4 ~ '50-69',
                    value == 5 ~ '70-100',
                    TRUE ~ "other"
                  ))
}

out %>% map_dfr(freq_table_tibble)




tib_test <- as_tibble(out)
tib_test[[1]]

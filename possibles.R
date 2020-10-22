library(tidyverse)
pyr <- c(1,2,6,7,8,NA)
cyr <- c(1,2,6,7,8,NA)
possibles <- expand.grid(pyr, cyr)

df <- possibles %>%
  rename(p = Var1, c = Var2) %>%
  mutate(newval = case_when(
    p==1 & c==1 ~ NA_real_,
    p==2 & c==1 ~ 11,
    p==6 & c==1 ~ NA_real_,
    p==7 & c==1 ~ NA_real_,
    p==8 & c==1 ~ 14,
    is.na(p) & c==1 ~ NA_real_,
    p==1 & c==2 ~ 10,
    p==2 & c==2 ~ 12,
    p==6 & c==2 ~ 16,
    p==7 & c==2 ~ 13,
    p==8 & c==2 ~ 15,
    is.na(p) & c==2 ~ 10,
    p==1 & c==6 ~ NA_real_, # can never have a 6 in cyr if there is a pyr
    p==2 & c==6 ~ 16, # can never have a 6 in cyr if there is a pyr
    p==6 & c==6 ~ NA_real_, # can never have a 6 in cyr if there is a pyr
    p==7 & c==6 ~ NA_real_, # can never have a 6 in cyr if there is a pyr
    p==8 & c==6 ~ 14, # can never have a 6 in cyr if there is a pyr
    is.na(p) & c==6 ~ NA_real_, # can never have a 6 in cyr if there is a pyr
    p==1 & c==7 ~ NA_real_,
    p==2 & c==7 ~ 14,
    p==6 & c==7 ~ NA_real_,# can never have a cloudy likely... when cloud in 1st pixel
    p==7 & c==7 ~ NA_real_,
    p==8 & c==7 ~ 14,
    is.na(p) & c==7 ~ NA_real_,
    p==1 & c==8 ~ 13,
    p==2 & c==8 ~ 15,
    p==6 & c==8 ~ 13, # can never have a cloudy likely... when cloud in 1st pixel
    p==7 & c==8 ~ 13,
    p==8 & c==8 ~ 15,
    is.na(p) & c==8 ~ 13,
    p==1 & is.na(c) ~ NA_real_,
    p==2 & is.na(c) ~ 11,
    p==6 & is.na(c) ~ NA_real_,
    p==7 & is.na(c) ~ NA_real_,
    p==8 & is.na(c) ~ 14,
    is.na(p) & is.na(c) ~ NA_real_,
    TRUE ~ NA_real_
  )) %>%
  mutate(pname = case_when(
    p==1 ~ 'very sparse',
    p==2 ~ 'mangroves',
    p==6 ~ 'cloud',
    p==7 ~ 'cloud very sparse',
    p==8 ~ 'cloud mangroves',
    TRUE ~ 'null'
  ),
  cname = case_when(
    c==1 ~ 'very sparse',
    c==2 ~ 'mangroves',
    c==6 ~ 'cloud',
    c==7 ~ 'cloud very sparse',
    c==8 ~ 'cloud mangroves',
    TRUE ~ 'null'
  ),
  necode = case_when(
    is.na(newval) ~ 'null',
    newval==10 ~ 'gain',
    newval==11 ~ 'loss',
    newval==12 ~ 'stable',
    newval==13 ~ 'cloud likely gain',
    newval==14 ~ 'cloud likely loss',
    newval==15 ~ 'cloud likely stble',
    newval==16 ~ 'cloud no data',
    TRUE ~ 'null'
  ))
write_csv(df, "cloudy_possibilities.csv")


## recode between two years

pyr <- c(1,2,6,7,8,NA)
cyr <- c(1,2,6,7,8,NA)
possibles <- expand.grid(pyr, cyr)

df <- possibles %>%
  rename(a = Var1, b = Var2) %>%
  mutate(c = case_when(
    a==1 & b==1 ~ NA_real_,
    a==2 & b==1 ~ 11,
    a==6 & b==1 ~ NA_real_,
    a==7 & b==1 ~ NA_real_,
    a==8 & b==1 ~ 14,
    is.na(a) & b==1 ~ NA_real_,
    a==1 & b==2 ~ 10,
    a==2 & b==2 ~ 12,
    a==6 & b==2 ~ 16,
    a==7 & b==2 ~ 13,
    a==8 & b==2 ~ 15,
    is.na(a) & b==2 ~ 10,
    a==1 & b==6 ~ NA_real_, # ban never have a 6 in byr if there is a ayr
    a==2 & b==6 ~ 16, # ban never have a 6 in byr if there is a ayr
    a==6 & b==6 ~ NA_real_, # ban never have a 6 in byr if there is a ayr
    a==7 & b==6 ~ NA_real_, # ban never have a 6 in byr if there is a ayr
    a==8 & b==6 ~ 14, # ban never have a 6 in byr if there is a ayr
    is.na(a) & b==6 ~ NA_real_, # ban never have a 6 in byr if there is a ayr
    a==1 & b==7 ~ NA_real_,
    a==2 & b==7 ~ 14,
    a==6 & b==7 ~ NA_real_,# ban never have a bloudy likely... when bloud in 1st aixel
    a==7 & b==7 ~ NA_real_,
    a==8 & b==7 ~ 14,
    is.na(a) & b==7 ~ NA_real_,
    a==1 & b==8 ~ 13,
    a==2 & b==8 ~ 15,
    a==6 & b==8 ~ 13, # ban never have a bloudy likely... when bloud in 1st aixel
    a==7 & b==8 ~ 13,
    a==8 & b==8 ~ 15,
    is.na(a) & b==8 ~ 13,
    a==1 & is.na(b) ~ NA_real_,
    a==2 & is.na(b) ~ 11,
    a==6 & is.na(b) ~ NA_real_,
    a==7 & is.na(b) ~ NA_real_,
    a==8 & is.na(b) ~ 14,
    is.na(a) & is.na(b) ~ NA_real_,
    TRUE ~ NA_real_
  ))

# function to code 1 to 8 for extents
chngfun1 <- function(x){
  ifelse(x >= 2 | x <= 5, 2,
         ifelse(x == 11, 7,
                ifelse(x >= 12 | x <= 15, 8, x)))
}
a = raster("./veg_class_cloud_prob/LgCSMP_Landsat_NBART_Veg_Class_Cloud_Probabilities_1989_AA.tif")
a_rst <- raster::calc(a, fun = chngfun1)
b = raster("./veg_class_cloud_prob/LgCSMP_Landsat_NBART_Veg_Class_Cloud_Probabilities_1990_AA.tif")
b_rst = raster::calc(b, fun = chngfun1)
out_rst = raster(a_rst); out_rst[] = NA
d = data.frame(a = a_rst[], b = b_rst[])
vec = c(1:nrow(d))
d[,3] = vec
m = merge(d, df, all.x=TRUE)
colnames(m)[3] = "ord"

m = m[order(m$ord),]

out_rst[] = m$c
writeRaster(out_rst, filename = "sample_likelies.tif")
# # pared down to only combos with specif change other than to NA
df2 <- possibles %>%
  rename(p = Var1, c = Var2) %>%
  mutate(newval = case_when(
    p==2 & c==1 ~ 11,
    p==8 & c==1 ~ 14,
    p==1 & c==2 ~ 10,
    p==2 & c==2 ~ 12,
    p==6 & c==2 ~ 16,
    p==7 & c==2 ~ 13,
    p==8 & c==2 ~ 15,
    is.na(p) & c==2 ~ 10,
    p==2 & c==7 ~ 14,
    p==8 & c==7 ~ 14,
    p==1 & c==8 ~ 13,
    p==2 & c==8 ~ 15,
    p==7 & c==8 ~ 13,
    p==8 & c==8 ~ 15,
    is.na(p) & c==8 ~ 13,
    p==2 & is.na(c) ~ 11,
    p==8 & is.na(c) ~ 14,
    TRUE ~ NA_real_
  )) %>%
  mutate(pname = case_when(
    p==1 ~ 'very sparse',
    p==2 ~ 'mangroves',
    p==6 ~ 'cloud',
    p==7 ~ 'cloud very sparse',
    p==8 ~ 'cloud mangroves',
    TRUE ~ 'null'
  ),
  cname = case_when(
    c==1 ~ 'very sparse',
    c==2 ~ 'mangroves',
    c==6 ~ 'cloud',
    c==7 ~ 'cloud very sparse',
    c==8 ~ 'cloud mangroves',
    TRUE ~ 'null'
  ),
  necode = case_when(
    is.na(newval) ~ 'null',
    newval==10 ~ 'gain',
    newval==11 ~ 'loss',
    newval==12 ~ 'stable',
    newval==13 ~ 'cloud likely gain',
    newval==14 ~ 'cloud likely loss',
    newval==15 ~ 'cloud likely stble',
    newval==16 ~ 'cloud no data',
    TRUE ~ 'null'
  ))
# write_csv(df2, "cloudy_possibilities2.csv")



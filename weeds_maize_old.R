library(rgbif)
library(dplyr)
library(raster)
library(rgdal)
library(rvest)
library(sp)
library(sf)
library(data.table)
library(RVenn)
library(devtools)
#install_github("xavi-rp/PreSPickR", 
#               ref = "v2", 
#               INSTALL_opts = c("--no-multiarch"))  # https://github.com/rstudio/renv/issues/162
library(PreSPickR)
library(ENMeval)
library(dismo)
library(virtualspecies)


#library(terra)


#sessionInfo()

if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", 
                              "jeodpp-terminal-03",
                              "jeodpp-terminal-dev-12")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/weeds/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/weeds/")
  wd <- "/eos/jeodpp/home/users/rotllxa/weeds/"
  gbif_creds <- "/home/rotllxa/Documents/"
}else{
  wd <- ""
  gbif_creds <- ""
}

setwd(wd)



## Crop Map ####

list.files("/eos/jeodpp/home/users/rotllxa")
list.files("/eos/jeodpp/home/users/rotllxa/data")
list.files("/storage/rotllxa")
list.files("/storage/rotllxa/Documents")

getwd()
list.files("/home/rotllxa/")
list.files("/home/rotllxa/Documents/")

list.files("/eos/jeodpp/data/base/")
#list.files("/eos/jeodpp/data/base/", recursive = TRUE)


list.files("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/")
list.files("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/2018")   ## EUCropMap

list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/")
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega")  ## Agricultural Management Intensity Map (Rega et al., 2020)
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/DataRestoration")
list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/DataRestoration/cropmap_res")



cropmap2018 <- raster("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/2018/EUCROPMAP_2018.tif")  # at 10m
#sp:::CRS("+init=EPSG:3035")

cropmap2018
cropmap2018_vals <- getValues(cropmap2018)
sum(cropmap2018_vals == 216, na.rm = TRUE)




cat_coords <-  c(3500000, 3800000, 1900000, 2300000)   # Catalonia (LAEA, m) (xmin, xmax, ymin, ymax)

cropmap2018_cat <- crop(cropmap2018, extent(cat_coords), 
                        filename = "cropmap2018_cat.tif",
                        overwrite = TRUE)
plot(cropmap2018_cat)
cropmap2018_cat <- raster("cropmap2018_cat.tif")


fr_coords <-  c(3100000, 4300000, 2200000, 3400000)   # France (LAEA, m) (xmin, xmax, ymin, ymax)

cropmap2018_fr <- crop(cropmap2018, extent(fr_coords), 
                       filename = "cropmap2018_fr.tif",
                       overwrite = TRUE)
plot(cropmap2018_fr)


#pixac_v7_byte_masked <- brick("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/2018/pixac_v7_byte_masked.tif")
#pixac_v7_byte_masked


# CropMap classes
#library(readr)
#cropmap_classes_2018 <- read_file("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/2018/EuroCropMap.qml")

crops_categs <- c(100, 211, 212, 213, 214, 215, 216, 217, 218, 219, 221, 222, 223, 230, 231, 232, 233, 240, 250, 290, 300, 500, 600, 800)
crops_names <- c("Artificial", "Common wheat", "Durum wheat", "Barley", "Rye", "Oats", "Maize", "Rice", "Triticale", "Other cereals", "Potatoes", "Sugar beet", "Other root crops", "Other non permanent industrial crops", "Sunflower", "Rape and turnip rape", "Soya", "Dry pulses", "Fodder crops (cereals and leguminous)", "Bare arable land", "Woodland and Shrubland (incl. permanent crops)", "Grasslands", "Bare land", "Wetlands")

cropmap_classes <- data.frame("crop_categ" = crops_categs, "crop_names" = crops_names)
View(cropmap_classes)

# Maiz: 216


### Aggregating maiz to 1km or 10km ####

#cropmap2018_maiz <- cropmap2018
#cropmap2018_maiz[cropmap2018_maiz$EUCROPMAP_2018 != 216] <- 0

aggr_fun_1km <- function(x, ...) {     # returns share of maize at 1km (0 to 1)
  if (all(is.na(x))){
    mz_share <- NA
  }else{
    mz_share <- sum(x == 216, na.rm = TRUE) / 10000
  }
  return(mz_share)
}

cropmap2018_maiz_1km <- aggregate(x = cropmap2018, 
                                  fact = 100,        # 1km
                                  fun = aggr_fun_1km, 
                                  expand = TRUE, 
                                  na.rm = TRUE, 
                                  #filename = "cropmap2018_maiz_1km.tif",
                                  filename = "",
                                  overwrite = TRUE)

cropmap2018_maiz_1km
plot(cropmap2018_maiz_1km)

#sf::st_crs(3035)
wkt <- sf::st_crs(3035)[[2]]
#sp::CRS(wkt)
crs(cropmap2018_maiz_1km) <- sp::CRS(wkt)

writeRaster(cropmap2018_maiz_1km, "cropmap2018_maiz_1km.tif", overwrite = TRUE)
cropmap2018_maiz_1km <- raster("cropmap2018_maiz_1km_cat.tif")
cropmap2018_maiz_1km <- raster("cropmap2018_maiz_1km_fr.tif")
cropmap2018_maiz_1km <- raster("cropmap2018_maiz_1km.tif")
#crs(cropmap2018_maiz_1km) <- sp::CRS(wkt)


cropmap2018_maiz_1km_vals <- getValues(cropmap2018_maiz_1km) 

sum(!is.na(cropmap2018_maiz_1km_vals))             # for Cat, 31059 pixels with some maize. For EU 3500020
sum(cropmap2018_maiz_1km_vals > 0, na.rm = TRUE)   # over a total of 61533 (not NA). For EU 4285358
(sum(cropmap2018_maiz_1km_vals > 0, na.rm = TRUE) / sum(!is.na(cropmap2018_maiz_1km_vals))) * 100   # Cat: 50.47%; EU: 81.67 

cropmap2018_maiz_1km_vals <- cropmap2018_maiz_1km_vals[!is.na(cropmap2018_maiz_1km_vals)]
sum(cropmap2018_maiz_1km_vals > 0)
summary(cropmap2018_maiz_1km_vals[cropmap2018_maiz_1km_vals > 0])
quantile(cropmap2018_maiz_1km_vals[cropmap2018_maiz_1km_vals > 0], seq(0, 1, 0.1))
quantile(cropmap2018_maiz_1km_vals[cropmap2018_maiz_1km_vals > 0], 0.47) # 47% of "maize pixels have less than 1% of maize
quantile(cropmap2018_maiz_1km_vals[cropmap2018_maiz_1km_vals > 0], 0.37) # 37% of "maize pixels have less than 0.5% of maize
quantile(cropmap2018_maiz_1km_vals[cropmap2018_maiz_1km_vals > 0], 0.18) # 18% of "maize pixels have less than 0.1% of maize



## 10km
aggr_fun_10km <- function(x, ...) {     # returns share of maize at 10km (0 to 1)
  if (all(is.na(x))){
    mz_share <- NA
  }else{
    mz_share <- sum(x == 216, na.rm = TRUE) / 10^6    # 10^6 10-m pixels in a 10-km pixel
  }
  return(mz_share)
}

t0 <- Sys.time()
cropmap2018_maiz_10km <- aggregate(x = cropmap2018, 
                                  fact = 1000,        # 10km
                                  fun = aggr_fun_10km,  # 10km
                                  expand = TRUE, 
                                  na.rm = TRUE, 
                                  #filename = "cropmap2018_maiz_10km.tif",
                                  filename = "",
                                  overwrite = TRUE)
Sys.time() - t0

cropmap2018_maiz_10km
writeRaster(cropmap2018_maiz_10km, "cropmap2018_maiz_10km.tif", overwrite = TRUE)
cropmap2018_maiz_10km <- raster("cropmap2018_maiz_10km.tif")



## 16km
aggr_fun_16km <- function(x, ...) {     # returns share of maize at 16km (0 to 1)
  if (all(is.na(x))){
    mz_share <- NA
  }else{
    mz_share <- sum(x == 216, na.rm = TRUE) / 2560000    # 2560000 (1600*1600) 10-m pixels in a 16-km pixel 
  }
  return(mz_share)
}

t0 <- Sys.time()
cropmap2018_maiz_16km <- aggregate(x = cropmap2018, 
                                   fact = 1600,        # 16km
                                   fun = aggr_fun_16km,  # 16km
                                   expand = TRUE, 
                                   na.rm = TRUE, 
                                   #filename = "cropmap2018_maiz_16km.tif",
                                   filename = "",
                                   overwrite = TRUE)
Sys.time() - t0

cropmap2018_maiz_16km
writeRaster(cropmap2018_maiz_16km, "cropmap2018_maiz_16km.tif", overwrite = TRUE)
cropmap2018_maiz_16km <- raster("cropmap2018_maiz_16km.tif")
cropmap2018_maiz_16km



### Aggregating Arable and Non-Arable Land to 1km or 10km ####

aggr_NonAL_1km <- function(x, ...) {     # returns share of Non-Arable Land at 1km (0 to 1)
  if (all(is.na(x))){
    nal_share <- NA
  }else{
    nal_share <- sum(x %in% c(500, 600, 800), na.rm = TRUE) / 10000
  }
  
  return(nal_share)
}


aggr_ArabLand_1km <- function(x, ...) {     # returns share of Arable Land at 1km (0 to 1)
  if (all(is.na(x))){
    al_share <- NA
  }else{
    al_share <- sum(!x %in% c(100, # artificial
                              300, # Woodland and Shrubland (incl. permanent crops)
                              500, # Grasslands
                              600, # Bare land
                              800  # Wetlands
                              ) & 
                      !is.na(x), 
    na.rm = TRUE) / 10000
  }
  
  return(al_share)
}


aggr_ArabLand_10km <- function(x, ...) {     # returns share of Arable Land at 1km (0 to 1)
  if (all(is.na(x))){
    al_share <- NA
  }else{
    al_share <- sum(!x %in% c(100, # artificial
                              300, # Woodland and Shrubland (incl. permanent crops)
                              500, # Grasslands
                              600, # Bare land
                              800  # Wetlands
                              )& 
                      !is.na(x), 
    na.rm = TRUE) / 10^6    # 10^6 10-m pixels in a 10-km pixel
  }
  
  return(al_share)
}


cropmap2018_arabland_1km <- aggregate(x = cropmap2018,
                                      #x = cropmap2018_cat,
                                      fact = 100,        # 1km
                                      fun = aggr_ArabLand_1km, 
                                      expand = TRUE, 
                                      na.rm = TRUE, 
                                      filename = "cropmap2018_ArableLand_1km.tif",
                                      #filename = "",
                                      overwrite = TRUE)

cropmap2018_arabland_1km
plot(cropmap2018_arabland_1km)

#sf::st_crs(3035)
wkt <- sf::st_crs(3035)[[2]]
#sp::CRS(wkt)
crs(cropmap2018_arabland_1km) <- sp::CRS(wkt)

#writeRaster(cropmap2018_arabland_1km, "cropmap2018_NonAL_1km.tif", overwrite = TRUE)
cropmap2018_nal_1km <- raster("cropmap2018_NonAL_1km_cat.tif")
cropmap2018_nal_1km <- raster("cropmap2018_NonAL_1km.tif")
cropmap2018_arabland_1km <- raster("cropmap2018_ArableLand_1km_cat.tif")
#cropmap2018_arabland_1km_kk <- raster("cropmap2018_ArableLand_1km.tif")
#writeRaster(cropmap2018_arabland_1km_kk, "cropmap2018_ArableLand_1km_kk.tif")
cropmap2018_arabland_1km <- raster("cropmap2018_ArableLand_1km.tif")


## Merging (rasters) maize share and arable land share
cropmap2018_maiz_1km$cropmap2018_arabland_1km <- getValues(cropmap2018_arabland_1km)
cropmap2018_maiz_1km

summary(cropmap2018_maiz_1km$cropmap2018_arabland_1km)


# This need to be recalculated!!!
cropmap2018_arabland_10km <- aggregate(x = cropmap2018,
                                       fact = 1000,        # 10km
                                       fun = aggr_ArabLand_10km, 
                                       expand = TRUE, 
                                       na.rm = TRUE, 
                                       filename = "cropmap2018_ArableLand_10km.tif",
                                       #filename = "",
                                       overwrite = TRUE)

# This need to be recalculated!!!   cropmap2018_arabland_10km <- raster("cropmap2018_ArableLand_10km.tif")


## Maize weeds ####

weeds_maize <- read.csv("weeds_maize_report_2011.csv", header = TRUE)
head(weeds_maize)
nrow(weeds_maize)

occs_all <- fread(paste0(getwd(), "/../exploring_lucas_data/D5_FFGRCC_gbif_occ/sp_records_20210709.csv"), header = TRUE)
if(nchar(occs_all$sp2[1]) == 7) occs_all[, sp2 := gsub(" ", "_", occs_all$species)]
occs_all
cols_order <- c("species", "decimalLatitude", "decimalLongitude", "gbifID", "countryCode", "year", "sp2")
occs_all <- occs_all[, .SD, .SDcols = cols_order]
occs_all <- occs_all[occs_all$species != "", ]
occs_all_2018 <- occs_all[occs_all$year == 2018, ]  # 1591221 occs for 2018

nrow(occs_all_2018)
sum(occs_all_2018$species == "")
length(unique(occs_all_2018$species))


occs_2018_specie <- unique(occs_all_2018$species)

head(sort(occs_2018_specie), 50)
head(sort(weeds_maize$Species))

sum(occs_2018_specie %in% weeds_maize$Species)   # 151 sp
sum(weeds_maize$Species %in% occs_2018_specie)   # 151 sp
sum(!weeds_maize$Species %in% occs_2018_specie)   # 53 sp (maize weeds) which we don't have in 2018


weeds_maiz_gbib <- sort(weeds_maize$Species[weeds_maize$Species %in% occs_2018_specie])
weeds_maiz_not_gbib <- sort(weeds_maize$Species[!weeds_maize$Species %in% occs_2018_specie])


occs_all_2018_maiz <- occs_all_2018[occs_all_2018$species %in% weeds_maiz_gbib, ]

nrow(occs_all_2018_maiz)   # 158427 occurrences for 2018
nrow(occs_all_2018)        # over 1591221 in total for 2018
occs_all_2018_maiz

sum(occs_all_2018_maiz$countryCode == "ES")
sum(occs_all_2018_maiz$countryCode == "FR")
occs_all_2018_maiz_fr <- occs_all_2018_maiz[occs_all_2018_maiz$countryCode == "FR", ]
occs_all_2018_maiz_fr
sort(table(occs_all_2018_maiz_fr$species))

sps_subset <- c("Digitaria sanguinalis", "Erodium cicutarium", "Beta vulgaris", "Phytolacca americana")
#occs_00_indicators <- fread("sp_indicators.csv", header = TRUE)
#sps_subset <- occs_00_indicators$Var1
  
sort(table(occs_all_2018_maiz_fr[occs_all_2018_maiz_fr$species %in% sps_subset, ]$species))
occs_all_2018_maiz_fr_subset <- occs_all_2018_maiz_fr[occs_all_2018_maiz_fr$species %in% sps_subset, ]
occs_all_2018_maiz_fr_subset
write.csv(occs_all_2018_maiz_fr_subset, "occurrences_weeds_gbif_FR_2018_subset.csv", row.names = FALSE)



setnames(occs_all_2018_maiz, c("decimalLongitude", "decimalLatitude"), c("x", "y"))

occs_all_2018_maiz <- occs_all_2018_maiz[, .SD, .SDcols = c("species", "x", "y", "gbifID", "countryCode", "year")]

occs_all_2018_maiz_sf <- st_as_sf(as.data.frame(occs_all_2018_maiz), coords = c("x", "y"), crs = 4326)#, agr = "constant")
occs_all_2018_maiz_sf

#sf::st_crs(3035)
wkt <- sf::st_crs(3035)[[2]]
#sp::CRS(wkt)
#occs_all_2018_maiz_sf[occs_all_2018_maiz_sf$countryCode == "ES", ]


occs_all_2018_maiz_sf_laea <- st_transform(occs_all_2018_maiz_sf, crs = sp::CRS(wkt))
occs_all_2018_maiz_sf_laea

occs_maizeShare <- as.data.table(extract(cropmap2018_maiz_1km, occs_all_2018_maiz_sf_laea, cellnumbers = TRUE))
occs_maizeShare  


## all occurrences (not only maize weeds)
setnames(occs_all_2018, c("decimalLongitude", "decimalLatitude"), c("x", "y"))

occs_all_2018 <- occs_all_2018[, .SD, .SDcols = c("species", "x", "y", "gbifID", "countryCode", "year")]

occs_all_2018_sf <- st_as_sf(as.data.frame(occs_all_2018), coords = c("x", "y"), crs = 4326)#, agr = "constant")
occs_all_2018_sf


occs_all_2018_sf_laea <- st_transform(occs_all_2018_sf, crs = sp::CRS(wkt))
occs_all_2018_sf_laea

occs_all_maizeShare <- as.data.table(extract(cropmap2018_maiz_1km, occs_all_2018_sf_laea, cellnumbers = TRUE))
occs_all_maizeShare  



# We keep pixels with some maize and at least one of the weeds !!! 

occs_all_2018_maiz_sf_laea_dt <- as.data.table(occs_all_2018_maiz_sf_laea)

occs_maizeShare <- cbind(occs_all_2018_maiz_sf_laea_dt, occs_maizeShare)
occs_maizeShare

occs_maizeShare <- na.omit(occs_maizeShare)

setkeyv(occs_maizeShare, "cells")
occs_maizeShare

# Removing repeated occurrences (same sp) in the same pixel, because we want to work with Species Richness
occs_maizeShare_abundances <- occs_maizeShare
occs_maizeShare <- occs_maizeShare[!duplicated(occs_maizeShare[, c("species", "cells")]), ]


sum(occs_maizeShare$cropmap2018_maiz_1km == 0)
sum(occs_maizeShare$cropmap2018_maiz_1km != 0)  



## We keep only pixels with at least 20% of arable land because we want to be focused on 
#  agricultural areas. (see https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2664.2005.01072.x
#                       for a reference for why 30%. But we need 20% to keep Galium tri)

occs_maizeShare <- occs_maizeShare[cropmap2018_arabland_1km >= 0.2, ]
range(occs_maizeShare$cropmap2018_arabland_1km)


## We also remove those pixels with less than 0.2% of maize share to avoid some noise produced by the CropMap
occs_maizeShare <- occs_maizeShare[cropmap2018_maiz_1km >= 0.002, ]
range(occs_maizeShare$cropmap2018_maiz_1km)



occs_maizeShare
length(unique(occs_maizeShare$cells))  # cells with one or more weeds 

length(unique(occs_maizeShare[occs_maizeShare$cropmap2018_maiz_1km != 0, cells])) # cells with maize and one or more weeds





summary(occs_maizeShare$cropmap2018_maiz_1km)  
#     Min.  1st Qu.  Median    Mean  3rd Qu.    Max. 
#   0.0020  0.0214  0.0753    0.1086  0.1639  0.8798  
quantile(occs_maizeShare$cropmap2018_maiz_1km, seq(0, 1, 0.1))
quantile(occs_maizeShare$cropmap2018_maiz_1km, 0.45)

occs_maizeShare_kk <- occs_maizeShare[!duplicated(occs_maizeShare$cells), ]
occs_maizeShare_kk
quantile(occs_maizeShare_kk$cropmap2018_maiz_1km, seq(0, 1, 0.1))
quantile(occs_maizeShare_kk$cropmap2018_maiz_1km, 0.46)

# percentiles of shares for those "maize pixels" with one or more weeds
# e.g. around % of "maize pixels" with weeds have a maize share below 1%
#     0%      10%    20%    30%      40%    50%      60%    70%    80%       90%   100% 
# 0.00200 0.00970 0.02290 0.04370 0.06900 0.09750 0.12926 0.16610 0.20880 0.27970 0.87980 



occs_maizeShare_aggr <- as.data.table(table(occs_maizeShare$cells))
occs_maizeShare_aggr <- occs_maizeShare_aggr[, lapply(.SD, as.numeric)]
str(occs_maizeShare_aggr)

occs_all_maizeShare_aggr <- as.data.table(table(occs_all_maizeShare$cells))  # all species
occs_all_maizeShare_aggr <- occs_all_maizeShare_aggr[, lapply(.SD, as.numeric)]
occs_all_maizeShare_aggr


#occs_maizeShare_1 <- occs_maizeShare[, 6:7]
occs_maizeShare_1 <- occs_maizeShare[, 6:8]
occs_maizeShare_1[!duplicated(occs_maizeShare_1$cells), ]

occs_maizeShare_1 <- unique(occs_maizeShare_1, by = "cells") 

occs_maizeShare_1 <- merge(occs_maizeShare_1, occs_maizeShare_aggr, by.x = "cells", by.y = "V1", all.x = TRUE)
occs_maizeShare_1

summary(occs_maizeShare_1$cropmap2018_maiz_1km_cat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#

summary(occs_maizeShare_1$cropmap2018_fr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 

summary(occs_maizeShare_1$cropmap2018_maiz_1km)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0020  0.0318  0.0975  0.1255  0.1868  0.8798


# merging all species
setnames(occs_all_maizeShare_aggr, "N", "N_all")
occs_maizeShare_1 <- merge(occs_maizeShare_1, occs_all_maizeShare_aggr, by.x = "cells", by.y = "V1", all.x = TRUE)


## Assessing correlations ####

png("scatterplot_SpRichness_maize.png",  
    width = 16, height = 15, units = "cm", res = 150)
plot(y = occs_maizeShare_1$N,  # x
     #x = occs_maizeShare_1$EUCROPMAP_2018, # y
     #x = occs_maizeShare_1$cropmap2018_maiz_1km_cat, # y
     x = occs_maizeShare_1$cropmap2018_maiz_1km, # y
     main = "",
     ylab = "Species richness", 
     xlab = "Maize share", 
     pch = 19)
abline(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maiz_1km), col = "red") # regression line (y~x)

pears_cor <- cor(occs_maizeShare_1$N, occs_maizeShare_1$cropmap2018_maiz_1km, method = "pearson")
mtext(paste0("Pearson's r = ", round(pears_cor, 3)),
      col = "black",
      side = 1, line = 3, 
      adj = 1,
      cex = 0.8)

lregr <- summary(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maiz_1km))
lregr$adj.r.squared
#mtext(paste0("R-squared = ", round(lregr$r.squared, 3)),
#      col = "red",
#      side = 1, line = 2, 
#      adj = 1,
#      cex = 0.8)

dev.off()


lregr
coef(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maiz_1km))


# sp richness against arable land (to demonstrate that the change in sp richness 
# is due to the share of maize, thus intensification, and not to the share of arable)

png("scatterplot_SpRichness_Arable.png",  
    width = 16, height = 15, units = "cm", res = 150)
plot(y = occs_maizeShare_1$N,  # x
     x = occs_maizeShare_1$cropmap2018_arabland_1km, # y
     main = "",
     ylab = "Species richness", 
     xlab = "Arable land share", 
     pch = 19)
abline(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_arabland_1km), col = "red") # regression line (y~x)

pears_cor <- cor(occs_maizeShare_1$N, occs_maizeShare_1$cropmap2018_arabland_1km, method = "pearson")
mtext(paste0("Pearson's r = ", round(pears_cor, 3)),
      col = "black",
      side = 1, line = 3, 
      adj = 1,
      cex = 0.8)

lregr <- summary(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_arabland_1km))
lregr$adj.r.squared
#mtext(paste0("R-squared = ", round(lregr$r.squared, 3)),
#      col = "red",
#      side = 1, line = 2, 
#      adj = 1,
#      cex = 0.8)

dev.off()




## Sp richness aggregated by maize share class 

occs_maizeShare_1[, maize_share_class := cut(occs_maizeShare_1$cropmap2018_maiz_1km,
                                             breaks = seq(0, 1, 0.1),
                                             include.lowest = FALSE,
                                             right = TRUE)]
sort(unique(occs_maizeShare_1$maize_share_class))

png("boxplot_SpRichness_maize.png",
     width = 20, height = 10, units = "cm", res = 150)
boxplot(occs_maizeShare_1$N ~ occs_maizeShare_1$maize_share_class, 
        main = "",
        xlab = "Maize shares", 
        ylab = "Species richness")
dev.off()


# rounding Maize shares
occs_maizeShare_2 <- occs_maizeShare_1
occs_maizeShare_2$EUCROPMAP_2018 <- round(occs_maizeShare_2$EUCROPMAP_2018, 1)
occs_maizeShare_2$cropmap2018_maiz_1km <- round(occs_maizeShare_2$cropmap2018_maiz_1km, 1)

plot(y = occs_maizeShare_2$N, 
     x = occs_maizeShare_2$cropmap2018_maiz_1km,
     main = "",
     ylab = "Number of occurrences", 
     xlab = "Maize share (rounded)", 
     pch = 19)

abline(lm(occs_maizeShare_2$N ~ occs_maizeShare_2$cropmap2018_maiz_1km), col = "red") # regression line (y~x)
dev.off()


# Linear Regression
summary(lm(occs_maizeShare_2$N ~ occs_maizeShare_2$cropmap2018_maiz_1km))

# Pearson Correlation
cor(occs_maizeShare_2$N, occs_maizeShare_2$cropmap2018_maiz_1km, method = "pearson") # Cat: -0.017; FR: -0.032; Eur: -0.1336785



# Outliers
boxplot(occs_maizeShare_1$EUCROPMAP_2018)
boxplot(occs_maizeShare_1$cropmap2018_maiz_1km)
boxplot(occs_maizeShare_1$N)

hist(occs_maizeShare_1$N, xlab = "Number of occurrences")
hist(occs_maizeShare_1$EUCROPMAP_2018)
hist(occs_maizeShare_1$cropmap2018_maiz_1km, xlab = "Maize share")

tail(sort(occs_maizeShare_1$N), 40)

summary(occs_maizeShare_1$N)
quantile(occs_maizeShare_1$N, c(0.95, 0.9772, 0.98, 0.99, 0.995, 0.997, 0.9999))


occs_maizeShare_2 <- occs_maizeShare_1
nrow(occs_maizeShare_1)
occs_maizeShare_2 <- occs_maizeShare_2[occs_maizeShare_2$N <= quantile(occs_maizeShare_2$N, 0.9999), ]
nrow(occs_maizeShare_2)

#pdf("Occs_MaizeShare_Eur_NoOutliers_9999.pdf")
plot(y = occs_maizeShare_2$N, 
     #x = occs_maizeShare_2$EUCROPMAP_2018,
     x = occs_maizeShare_2$cropmap2018_maiz_1km,
     main = "",
     ylab = "Number of occurrences (<= 99.99th Percentile)", 
     xlab = "Maize share", 
     pch = 19)
dev.off()

abline(lm(occs_maizeShare_2$N ~ occs_maizeShare_2$cropmap2018_maiz_1km), col = "red") # regression line (y~x)

pears_cor <- cor(occs_maizeShare_2$N, occs_maizeShare_2$EUCROPMAP_2018, method = "pearson") # Eur: -0.031
pears_cor <- cor(occs_maizeShare_2$N, occs_maizeShare_2$cropmap2018_maiz_1km, method = "pearson") # Eur: -0.023
pears_cor

mtext(paste0("Pearson's r = ", round(pears_cor, 3)),
      col = "red",
      side = 1, line = 2.5, 
      adj = 1,
      cex = 1)

# Linear Regression
summary(lm(occs_maizeShare_2$N ~ occs_maizeShare_2$cropmap2018_maiz_1km))


boxplot(occs_maizeShare_2$N ~ occs_maizeShare_2$cropmap2018_maiz_1km, 
        main = "",
        xlab = "Maize shares", 
        ylab = "Number of occurrences")



## kk
occs_maizeShare_3 <- occs_maizeShare_1
nrow(occs_maizeShare_1)
occs_maizeShare_3 <- occs_maizeShare_3[occs_maizeShare_3$N <= quantile(occs_maizeShare_3$N, 0.9999), ]
nrow(occs_maizeShare_3)

#pdf("Occs_MaizeShare_Eur_NoOutliers.pdf")
plot(y = log(occs_maizeShare_3$N), 
     x =  log(occs_maizeShare_3$cropmap2018_maiz_1km),
     main = "",
     #ylab = "Number of occurrences (5 <= N <= 99.99th Percentile)", 
     ylab = "log - Number of occurrences (<= 99.99th Percentile)", 
     xlab = "log - Maize share", 
     pch = 19)
abline(lm(log(occs_maizeShare_3$N) ~ log(occs_maizeShare_3$cropmap2018_maiz_1km)), col = "red") # regression line (y~x)

pears_cor <- cor(occs_maizeShare_3$N, occs_maizeShare_3$cropmap2018_maiz_1km, method = "pearson") # Eur: -0.023
pears_cor

mtext(paste0("Pearson's r = ", round(pears_cor, 3)),
      col = "red",
      side = 1, line = 2.5, 
      adj = 1,
      cex = 1)

dev.off()

## Linear Regression
model1 <- lm(occs_maizeShare_3$N ~ occs_maizeShare_3$cropmap2018_maiz_1km)
coef(model1)
summary(model1)



## Non Linear (exponential) regression
# https://rpubs.com/mengxu/exponential-model

occs_maizeShare_3 <- occs_maizeShare_1

# Select an approximate $\theta$, since theta must be lower than min(y), and greater than zero
theta.0 <- min(occs_maizeShare_3$N) * 0.5 

# Estimate the rest parameters using a linear model
model.0 <- lm(log(occs_maizeShare_3$N - theta.0) ~ occs_maizeShare_3$cropmap2018_maiz_1km) 
coef(model.0)
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
start

# Fit the model
model <- nls(N ~ alpha * exp(beta * cropmap2018_maiz_1km) + theta, data = occs_maizeShare_3, start = start)

# Plot fitted curve
plot(occs_maizeShare_3$cropmap2018_maiz_1km, occs_maizeShare_3$N)
lines(occs_maizeShare_3$cropmap2018_maiz_1km, predict(model, list(x = occs_maizeShare_3$cropmap2018_maiz_1km)), col = 'skyblue', lwd = 3)

summary(model)



## Polynomial model

model2 <- lm(N ~ cropmap2018_maiz_1km + I(cropmap2018_maiz_1km^2), data = occs_maizeShare_3)
summary(model2) # only the first order of the function is significant... no non-linear relation!


## GAM
library(mgcv)
model <- gam(N ~ s(cropmap2018_maiz_1km), data = occs_maizeShare_3)
summary(model)


## Poisson
hist(occs_maizeShare_1$N)
hist(occs_maizeShare_1$cropmap2018_maiz_1km)
#hist(occs_maizeShare_1$adjusted_richness)
cor(occs_maizeShare_1$N, occs_maizeShare_1$cropmap2018_maiz_1km)

library(vcd)
freqs <- table(occs_maizeShare_1$N)
freqs <- as.data.frame(freqs)
freqs$Var1 <- as.numeric(freqs$Var1)
gf <- goodfit(freqs[, 2:1], "poisson")
plot(gf, type = "standing", scale = "raw")

var(occs_maizeShare_1$N)
mean(occs_maizeShare_1$N)



model <- glm(N ~ cropmap2018_maiz_1km, family = "poisson", data = occs_maizeShare_1)
summary(model)

1 - (model$deviance / model$null.deviance)  # McFadden's pseudo-R2 (for lm/glm(family ="gaussian"), it is the one reported as Multiple R-squared)



## Zero-inflated poisson
library(pscl)
model <- zeroinfl(N ~ cropmap2018_maiz_1km, data = occs_maizeShare_1)

sum(occs_maizeShare_1$N == 1)
table(occs_maizeShare_1$N)

# Zeroes have been already removed from the study, as we are focused on "maize areas" where at least one of the weeds is present
# We have a lot of pixels with N = 1, making the distribution very skewed 



## Bootstraping for correlation

# It is a way to estimate the distribution of some statistic (mean, standard error, Pearson's correlation coeff, etc), 
# given only one sample. So if I want to estimate the mean of a population using bootstrap methods, I generate many 
# bootstrap samples, compute the mean of each of these bootstrap samples, and then use the distribution of those values 
# to deduce where the unknown population mean is likely to fall and compute a confidence interval for the statistic.

# https://stackoverflow.com/questions/58393608/bootstrapped-correlation-in-r
# https://www.datacamp.com/community/tutorials/bootstrap-r
library(boot)

foo <- function(data, indices = NULL, cor.type){
  if(!is.null(indices)){
    dt<-data[indices,]
  }else{
    dt <- data
  }
  c(cor(dt[,1], dt[,2], method=cor.type))
}

myBootstrap <- boot(occs_maizeShare_1[, .SD, .SDcols = c("cropmap2018_maiz_1km", "N")], foo, 
                    sim = "balanced",
                    R=1000, cor.type='s')

myBootstrap
head(myBootstrap$t)
mean(myBootstrap$t)
myBootstrap$t0
plot(myBootstrap)

boot.ci(myBootstrap, type=c('basic', "perc"))
boot.ci(myBootstrap, type='norm')$norm


## Assess correlation between N and N_all (i.e. number of weeds vs number of all species -not including weeds)

occs_maizeShare_1$N_all_NoWeeds <- occs_maizeShare_1$N_all - occs_maizeShare_1$N

hist(occs_maizeShare_1$N_all_NoWeeds, xlab = "All sp (no weeds)")

plot(x = occs_maizeShare_1$N_all_NoWeeds, 
     y = occs_maizeShare_1$N, 
     main = "",
     xlab = "Number of all species (except weeds; occs.)", 
     ylab = "Number of Weeds (occurrences)", 
     pch = 19
)

abline(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$N_all_NoWeeds), col = "red") # regression line (y~x)

pears_cor_1 <- cor(occs_maizeShare_1$N, occs_maizeShare_1$N_all_NoWeeds, method = "pearson") # Eur: 0.50  (Not a very strong correlation, but still)
pears_cor_1

summary(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$N_all_NoWeeds))  # significant; R-squared = 0.2556; but no normal variables

# GAM
library(mgcv)
summary(gam(N ~ s(N_all_NoWeeds), data = occs_maizeShare_1))   # R-sq.(adj) =  0.605
summary(gam(N ~ N_all_NoWeeds, data = occs_maizeShare_1))      # R-sq.(adj) =  0.495

# GLM
mdl1 <- glm(occs_maizeShare_1$N ~ occs_maizeShare_1$N_all_NoWeeds, family = "poisson")
summary(mdl1)    # 
coef(mdl1)       # It gives a positive effect
aov(mdl1)
with(summary(mdl1), 1 - deviance/null.deviance)  # goodness of fit   # 0.18





## Adding number of occurrences of other species (not weeds) as a covariate 
#  although both variables are quite correlated

mdl <- lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maiz_1km + occs_maizeShare_1$N_all_NoWeeds)
summary(mdl)    # The effect of N_all_NoWeeds is masking the effect of maize share
coef(mdl)       # It gives a positive effect

mdl1 <- glm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maiz_1km + occs_maizeShare_1$N_all_NoWeeds, family = "poisson")
summary(mdl1)    # The effect of N_all_NoWeeds is masking the effect of maize share
coef(mdl1)       # It gives a positive effect
aov(mdl1)





## Assessing the effect of Maize share over the total number of species (weeds + no weeds)
occs_maizeShare_1

plot(x = occs_maizeShare_1$cropmap2018_maiz_1km, 
     y = occs_maizeShare_1$N_all, 
     main = "",
     xlab = "Maize share", 
     ylab = "Number of all species (occurrences)", 
     pch = 19
)

abline(lm(occs_maizeShare_1$N_all ~ occs_maizeShare_1$cropmap2018_maiz_1km), col = "red") # regression line (y~x)

modl <- lm(occs_maizeShare_1$N_all ~ occs_maizeShare_1$cropmap2018_maiz_1km)
summary(modl)
coef(modl)

pears_cor_2 <- cor(occs_maizeShare_1$N_all, occs_maizeShare_1$cropmap2018_maiz_1km, method = "pearson") # Eur: -0.042
pears_cor_2




## Sp Richness vs Maize share over Arable land 

occs_maizeShare_1[, cropmap2018_maizArable_1km := round(cropmap2018_maiz_1km / cropmap2018_arabland_1km, 3)]
sum(is.na(occs_maizeShare_1$N))
sum(is.na(occs_maizeShare_1$cropmap2018_maizArable_1km))
sum(is.na(occs_maizeShare_1$cropmap2018_maiz_1km))
sum(is.na(occs_maizeShare_1$cropmap2018_arabland_1km))
occs_maizeShare_1[is.na(occs_maizeShare_1$cropmap2018_maizArable_1km), ]
sum(is.nan(occs_maizeShare_1$cropmap2018_maizArable_1k))
occs_maizeShare_1$cropmap2018_maizArable_1km[is.nan(occs_maizeShare_1$cropmap2018_maizArable_1km)] <- 0
occs_maizeShare_1[occs_maizeShare_1$cells == 14790069, ]
occs_maizeShare_1 <- occs_maizeShare_1[, c(1:4, 6)]


#pdf("scatterplot_SpRichness_maizeArable.pdf")
plot(y = occs_maizeShare_1$N,  # x
     #x = occs_maizeShare_1$EUCROPMAP_2018, # y
     #x = occs_maizeShare_1$cropmap2018_maiz_1km_cat, # y
     x = occs_maizeShare_1$cropmap2018_maizArable_1k, # y
     main = "",
     ylab = "Species richness", 
     xlab = "Maize share / Arable Land share", 
     pch = 19)
abline(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maizArable_1km), col = "red") # regression line (y~x)

pears_cor <- cor(occs_maizeShare_1$N, occs_maizeShare_1$cropmap2018_maizArable_1km, method = "pearson")
mtext(paste0("Pearson's r = ", round(pears_cor, 3)),
      col = "black",
      side = 1, line = 3, 
      adj = 1,
      cex = 0.8)

lregr <- summary(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maizArable_1km))
lregr$adj.r.squared
mtext(paste0("R-squared = ", round(lregr$r.squared, 3)),
      col = "red",
      side = 1, line = 2, 
      adj = 1,
      cex = 0.8)

dev.off()


coef(lm(occs_maizeShare_1$N ~ occs_maizeShare_1$cropmap2018_maizArable_1km))







## Downloading Zea mays occurrences ####

GetBIF(credentials = paste0(gbif_creds, "/gbif_credentials.RData"),
       taxon_list = "Zea mays",
       download_format = "SIMPLE_CSV",
       download_years = c(2000, 2021),
       download_coords = c(-12.69141, 42.71485, 33.4901, 71.9218), #order: xmin, xmax, ymin, ymax
       download_coords_accuracy = c(0, 50),
       rm_dupl = TRUE,
       cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                     "gbifID",
                     "coordinateUncertaintyInMeters",
                     "countryCode", "year", 
                     "datasetKey"
       ),
       out_name = paste0("maize_records_", format(Sys.Date(), "%Y%m%d"))
)


taxon_dir <- "/eos/jeodpp/home/users/rotllxa/exploring_lucas_data"
taxons <- "Zea mays"
data1 <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                  taxons = taxons,
                  cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                                "gbifID",
                                "coordinateUncertaintyInMeters",
                                "countryCode", "year", 
                                #"institutionCode",	"collectionCode",
                                #"ownerInstitutionCode",
                                "datasetKey"
                  ),
                  #cols2keep = "all",
                  rm_dupl = TRUE)

data1




## Finding weeds potentially indicators of low intensification ####
## Grouping by 10 classes of maize share 
fwrite(occs_maizeShare, file = "/eos/jeodpp/home/users/rotllxa/weeds/occs_maizeShare.csv", row.names = FALSE, quote = FALSE)
occs_maizeShare <- fread("/eos/jeodpp/home/users/rotllxa/weeds/occs_maizeShare.csv", header = TRUE)
# Remember that 'occs_maizeShare' has no longer duplicated species in the same cell

occs_maizeShare_kk <- occs_maizeShare
occs_maizeShare <- occs_maizeShare_kk


sum(is.na(cut(occs_maizeShare$cropmap2018_maiz_1km, breaks = seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE)))
sort(unique(cut(occs_maizeShare$cropmap2018_maiz_1km, breaks = seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE)))
table(cut(occs_maizeShare$cropmap2018_maiz_1km, breaks = seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE))

occs_maizeShare$maiz_share_class <- cut(occs_maizeShare$cropmap2018_maiz_1km, breaks = seq(0, 1, 0.1), include.lowest = TRUE, right = FALSE)
occs_maizeShare

sum(is.na(occs_maizeShare$maiz_share_class))
sort(unique(occs_maizeShare$maiz_share_class))
table(occs_maizeShare$maiz_share_class)
# [0,0.1) [0.1,0.2) [0.2,0.3) [0.3,0.4) [0.4,0.5) [0.5,0.6) [0.6,0.7) [0.7,0.8) [0.8,0.9)   [0.9,1] 
#   19842      8153      3838      1519       474       179        47        22         5         0


sp_00 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0,0.1)", "species"]))))
sp_01 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.1,0.2)", "species"]))))
sp_02 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.2,0.3)", "species"]))))
sp_03 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.3,0.4)", "species"]))))
sp_04 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.4,0.5)", "species"]))))
sp_05 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.5,0.6)", "species"]))))
sp_06 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.6,0.7)", "species"]))))
sp_07 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.7,0.8)", "species"]))))
sp_08 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.8,0.9)", "species"]))))
sp_09 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maiz_share_class == "[0.9,1]", "species"]))))   # no species here

Reduce(intersect, list(sp_00,
                       sp_01,
                       sp_02,
                       sp_03,
                       sp_04,
                       sp_05,
                       sp_06,
                       sp_07,
                       sp_08#,
                       #sp_09
))      # These are the plants in all groups: "Abutilon theophrasti" "Phytolacca americana"

sp_08  # plants in group 08: "Abutilon theophrasti" "Chenopodium album" "Convolvulus arvensis" 
#                     "Phytolacca americana" (invasive)
#                     "Portulaca oleracea"
# All very common weeds


sp_00_notOthers <- sp_00[!sp_00 %in% c(sp_01) &
                           !sp_00 %in% c(sp_02) &
                           !sp_00 %in% c(sp_03) &
                           !sp_00 %in% c(sp_04) &
                           !sp_00 %in% c(sp_05) &
                           !sp_00 %in% c(sp_06) &
                           !sp_00 %in% c(sp_07) &
                           !sp_00 %in% c(sp_08) &
                           !sp_00 %in% c(sp_09)
]
sp_00_notOthers  # These are the species that are only found in group 0; therefore, they could be 
# our "indicators of low intensification". How many times?

occs_00 <- occs_maizeShare[occs_maizeShare$maiz_share_class == "[0,0.1)", ]
length(unique(occs_00$species))

occs_00_indicators <- occs_00[occs_00$species %in% sp_00_notOthers, ]
occs_00_indicators
occs_00_indicators <- table(occs_00_indicators$species)
occs_00_indicators <- sort(occs_00_indicators, decreasing = TRUE)
occs_00_indicators
write.csv(occs_00_indicators, file = "sp_indicators.csv", row.names = FALSE)
occs_00_indicators <- fread("sp_indicators.csv", header = TRUE)

# These are the "indicator species" and how many times they appear in pixels with maize (always class [0,0.1)):
#
# Galium tricornutum     Digitaria ischaemum       Xanthium spinosum   Chrozophora tinctoria    Caucalis platycarpos    Chenopodium vulvaria 
#  36                      19                      13                      12                      11                      10 
# Polygonum persicaria      Cyperus rotundus      Fumaria parviflora   Phalaris brachystachys         Eleusine indica      Amaranthus viridis 
#   9                          7                       7                       7                       6                       5 
# Phalaris paradoxa          Phalaris minor      Acalypha virginica   Chenopodium opulifolium        Euphorbia nutans        Ridolfia segetum 
#   5                         4                       3                       3                       3                       3 
# Kickxia lanigera   Solanum physalifolium   Amaranthus graecizans      Diplotaxis virgata    Eragrostis virescens      Pulicaria paludosa 
#   2                       2                       1                       1                       1                       1 
# Xanthium albinum 
#   1 



# For the other species in class 0, not "indicators"
# These appear in many other classes

occs_00_NotIndicators <- occs_00[!occs_00$species %in% sp_00_notOthers, ]
occs_00_NotIndicators <- table(occs_00_NotIndicators$species)
occs_00_NotIndicators <- sort(occs_00_NotIndicators, decreasing = TRUE)
occs_00_NotIndicators



sp_01_notOthers <- sp_01[
  !sp_01 %in% c(sp_00) &
  !sp_01 %in% c(sp_02) &
  !sp_01 %in% c(sp_03) &
  !sp_01 %in% c(sp_04) &
  !sp_01 %in% c(sp_05) &
  !sp_01 %in% c(sp_06) &
  !sp_01 %in% c(sp_07) &
  !sp_01 %in% c(sp_08) &
  !sp_01 %in% c(sp_09)
]
sp_01_notOthers  # This are the species only found in group 1:
# "Amaranthus blitoides" "Fumaria schleicheri" 



sp_01[
  sp_01 %in% c(sp_00) &
    !sp_01 %in% c(sp_02) &
    !sp_01 %in% c(sp_03) &
    !sp_01 %in% c(sp_04) &
    !sp_01 %in% c(sp_05) &
    !sp_01 %in% c(sp_06) &
    !sp_01 %in% c(sp_07) &
    !sp_01 %in% c(sp_08) &
    !sp_01 %in% c(sp_09)
]

# These are the () species that are only found in group 1 and 0: 
#"Atriplex patula"   "Brassica nigra"    "Eclipta prostrata" "Rumex pulcher"     "Senecio vernalis"  "Silene noctiflora"
#"Veronica agrestis"


sum(sp_00 %in% "Amaranthus blitoides")
sum(sp_00 %in% "Atriplex patula")
sum(sp_00 %in% "Silene noctiflora")
sum(sp_00 %in% sp_01_notOthers)   # 9 ??????
sp_01_notOthers[!sp_01_notOthers %in% sp_00]   # "Amaranthus blitoides" and "Fumaria schleicheri" are only in group 1 (not in group 0!)


# for sps in class 0 nand 1
occs_00_01 <- occs_maizeShare[occs_maizeShare$maiz_share_class %in% c("[0,0.1)", "[0.1,0.2)"), ]
length(unique(occs_00$species))

occs_01 <- occs_00_01[occs_00_01$species %in% sp_01_notOthers, ]
occs_01
unique(occs_01$species)
occs_01 <- table(occs_01$species)
occs_01 <- sort(occs_01, decreasing = TRUE)
occs_01
write.csv(occs_01, file = "sp_sensitive_class00_01.csv", row.names = FALSE)
occs_01 <- fread("sp_sensitive_class00_01.csv", header = TRUE)
occs_01$Freq
occs_01$Var1



# for sps in class 0.8-0.9 (the most tolerant; they appear in most of the other classes too, although some species in not all)
occs_08 <- occs_maizeShare[occs_maizeShare$species  %in% sp_08, ]
occs_08
unique(occs_08$species)
sort(unique(occs_08$maiz_share_class))
occs_08 <- table(occs_08$species)
occs_08 <- sort(occs_08, decreasing = TRUE)
occs_08
write.csv(occs_08, file = "sp_tolerant_class00_08.csv", row.names = FALSE)
occs_08 <- fread("sp_tolerant_class00_08.csv", header = TRUE)
occs_08$Freq
occs_08$Var1




## Some plots:

sps_groups <- list(grp00 = sp_00,
                   grp01 = sp_01,
                   grp02 = sp_02,
                   grp03 = sp_03,
                   grp04 = sp_04,
                   grp05 = sp_05,
                   grp06 = sp_06,
                   grp07 = sp_07,
                   grp08 = sp_08#,
                   #grp09 = sp_09
)

grp_names <- as.vector(sort(unique(occs_maizeShare$maiz_share_class)))
names(sps_groups) <- grp_names

sp_01_08 <- c(sp_01,
              sp_02,
              sp_03,
              sp_04,
              sp_05,
              sp_06,
              sp_07,
              sp_08)
sp_01_08 <- unique(sp_01_08)

sps_groups_1 <- list(grp00 = sp_00,
                     grp01_08 = sp_01_08)
sps_groups_1


sp_02_08 <- c(sp_02,
              sp_03,
              sp_04,
              sp_05,
              sp_06,
              sp_07,
              sp_08)
sp_02_08 <- unique(sp_02_08)

sps_groups_2 <- list(grp00 = sp_00,
                     grp01 = sp_01,
                     grp02_08 = sp_02_08)
names(sps_groups_2) <- c(paste0("Maize share = ", grp_names[1]), 
                         paste0("Maize share = ", grp_names[2]), 
                         paste0("Maize share >= 0.2"))



# Barplot
sps_groups_vctr <- sapply(sps_groups, length)
plot(sps_groups_vctr)

pdf("barplot_IndicatorSp.pdf",
    width = 14, height = 8)
par(mar = c(5, 5, 5, 3))
barplot(sps_groups_vctr, 
        space = 0,
        names.arg = grp_names,
        ylab = "Number of species",
        xlab = "Maize share (pooled)",
        ylim = c(0, (max(sps_groups_vctr) + 10)),
        cex.axis = 1.2,
        cex.names = 1.2, 
        cex.lab = 1.5)
dev.off()       


# Venn diagram
# Venn diagram only works with up to 4 vectors

library(ggvenn)
ggvenn(sps_groups_1)

pdf("VennDiagr_3_IndicatorSp.pdf",
    width = 8, height = 8)

ggvenn(sps_groups_2, 
       #show_elements = TRUE)
       show_elements = FALSE,
       text_size = 6,
)

dev.off()




# Heatmap:
library(RVenn)
sps_groups_rvenn <- Venn(sps_groups)


pdf("heatmap_IndicatorSp.pdf",
    width = 15, height = 25)

setmap(sps_groups_rvenn, element_clustering = FALSE, set_clustering = FALSE,
       title = "")

dev.off()



# with ggplot

library(reshape2)
sps_groups_df <- melt(sps_groups)
names(sps_groups_df) <- c("Species", "Maize_share")
head(sps_groups_df)

library(forcats)
library(tidyverse)

pdf("heatmap_IndicatorSp_2.pdf",
    width = 15, height = 30)

#jpeg("heatmap_IndicatorSp_2.jpg",
#     units = "cm",
#     res = 300,
#     width = 15, height = 40)

sps_groups_df %>%
  mutate(Species = fct_reorder(Species, desc(Species))) %>% 
  ggplot(aes(Maize_share, Species)) + 
  scale_x_discrete(position = "top") +
  geom_tile(aes(fill = Maize_share), colour = "white", show.legend = FALSE) 

dev.off()



# UpSet plot:

#If you use an UpSet figure in a publication, please cite the original paper:
#  Alexander Lex, Nils Gehlenborg, Hendrik Strobelt, Romain Vuillemot, Hanspeter Pfister. UpSet: Visualization of Intersecting Sets IEEE Transactions on Visualization and Computer Graphics (InfoVis), 20(12): 1983--1992, doi:10.1109/TVCG.2014.2346248, 2014.
#If you created an UpSet figure with UpSetR, please also cite the UpSetR paper:
#  Jake R. Conway, Alexander Lex, Nils Gehlenborg. UpSetR: An R Package For The Visualization Of Intersecting Sets And Their Properties Bioinformatics, 33(18): 2938-2940, doi:10.1093/bioinformatics/btx364, 2017.

library(UpSetR)

pdf("upset_IndicatorSp.pdf",
    width = 10, height = 7)

upset(fromList(sps_groups),
      nsets = 9, 
      order.by = "freq",
      mainbar.y.label = "Intersections (Number of species)",
      sets.x.label = "Number of species",
      text.scale = c(1.5, 1.5, 1.5, 1.2, 1.5, 1.2),
      queries = list(
        list(query = intersects, params = list("[0,0.1)"), 
             #color = "lightsteelblue3", 
             color = "#8c96c6", 
             active = TRUE)
      )
)
dev.off()


## Indicator species over CropMap (distance) ####
occs_00_indicators
sp_indic_1 <- occs_00_indicators[1, Var1] # best indicator sp (Galium tricornutum)
#sp_indic_1 <- occs_00_indicators[2, Var1] # 2nd best indicator sp (Digitaria ischaemum)
#sp_indic_1 <- occs_00_indicators[3, Var1] # 3rd best indicator sp (Xanthium spinosum)

sp_indic_1 <- "Abutilon theophrasti"  # species very resistant (appearing in all maize share clases)
sp_indic_1 <- occs_00_indicators[8, Var1]  # Cyperus rotundus; another sp. indicator

sp_indic_1 <- occs_00_indicators[, Var1]  # all maize weeds; first, indicator sp
sp_NoIndic <- weeds_maiz_gbib[!weeds_maiz_gbib %in% sp_indic_1]
sp_NoIndic <- sort(table(occs_all_2018_maiz_sf_laea[occs_all_2018_maiz_sf_laea$species %in% sp_NoIndic, ]$species), decreasing = TRUE)
sp_NoIndic <- names(sp_NoIndic)[1:25]  # selecting those 25 not indicator sp with more occurrences
sp_indic_1 <- c(sp_indic_1, sp_NoIndic)  # all maize weeds; first, indicator sp
#sp_indic_1 <- sp_indic_1[38:length(sp_indic_1)]
save(sp_indic_1, file = "sp_indic_1.RData")
#load("sp_indic_1.RData", verbose = TRUE)

#sp_indic_1 <- occs_01$Var1   # sps in classes 0-0.1 and 0.1-0.2 
#sp_indic_1 <- c(sp_indic_1, occs_01$Var1)
#sp_indic_1_kk <- sp_indic_1
#sp_indic_1 <- c(occs_08$Var1)

library(terra)

for (sp in sp_indic_1){
  print(sp)
  occs_all_2018_maiz_sf_laea
  occs_all_2018_maiz_sf_laea_indic1 <- occs_all_2018_maiz_sf_laea[occs_all_2018_maiz_sf_laea$species %in% sp, ]
  occs_all_2018_maiz_sf_laea_indic1
  
  cropmap2018
  
  
  sp_indic_1_crop <- extract(cropmap2018, occs_all_2018_maiz_sf_laea_indic1, sp = TRUE)
  sp_indic_1_crop  
  
  sp_indic_1_crop_df <- data.frame(sp_indic_1_crop)
  sp_indic_1_crop_df
  cropmap_classes
  
  sp_indic_1_crop_df <- merge(sp_indic_1_crop_df, cropmap_classes, by.x = "EUCROPMAP_2018", by.y = "crop_categ", all.x = TRUE)
  table(sp_indic_1_crop_df$crop_names)
  
  
  cropmap2018_buff <- extract(cropmap2018, occs_all_2018_maiz_sf_laea_indic1, buffer = 1000, cellnumbers = TRUE)
  #cropmap2018_buff
  dim(cropmap2018_buff[[1]])
  
  #i <- 1
  set_distances <- c()
  for (i in 1:length(cropmap2018_buff)){
    #for (i in 4:4){
    cropmap2018_buff_i <- as.data.table(cropmap2018_buff[[i]])
    #print(sort(unique(cropmap2018_buff_i$value)))
    #print(sum(unique(cropmap2018_buff_i$value) == 216, na.rm = TRUE))
    if(sum(cropmap2018_buff_i$value == 216, na.rm = TRUE) > 0){
      cropmap2018_buff_i <- cropmap2018_buff_i[cropmap2018_buff_i$value == 216, ]$cell
      maize_cells <- xyFromCell(cropmap2018, cropmap2018_buff_i, spatial = TRUE)
      dis_closest <- pointDistance(occs_all_2018_maiz_sf_laea_indic1[i, ], maize_cells, lonlat = FALSE, allpairs = TRUE)
      dis_closest <- min(dis_closest)
      #pdf("kk.pdf")
      #plot(maize_cells, col = "red")
      #plot(occs_all_2018_maiz_sf_laea_indic1[i, ], add = TRUE)
      #scalebar(200, type = "bar", divs = 4)
      #dev.off()
    }else{
      dis_closest <- NA
    }
    set_distances <- c(set_distances, dis_closest)
    #print(dis_closest)
    
  }
  
  set_distances
  
  summary(set_distances)
  sd(set_distances, na.rm = TRUE)
  
  
  
  occs_all1 <- fread(paste0(getwd(), "/../exploring_lucas_data/D5_FFGRCC_gbif_occ/sp_records_20210709.csv"), header = TRUE)
  occs_all1 <- occs_all1[gbifID %in% occs_all_2018_maiz_sf_laea_indic1$gbifID]
  occs_all1
  
  dist2maize <- occs_all1[, .SD, .SDcols = c("gbifID", "species", "countryCode", "year", "coordinateUncertaintyInMeters")]
  dist2maize[, dist2maize := round(set_distances, 0)]
  dist2maize
  #View(dist2maize)
  
  #write.csv(dist2maize, "distance2maize_Galium_tricornutum.csv", row.names = FALSE)
  write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", sp), ".csv"), row.names = FALSE)
  
}

#dist2maize <- fread("distance2maize_all_50sp.csv", header = TRUE)
dist2maize


sp_indic_1
dist2maize_1 <- fread(paste0("distance2maize_", gsub(" ", "_", sp_indic_1[1]), ".csv"))

for (s in sp_indic_1[-1]) {
  if(file.exists(paste0("distance2maize_", gsub(" ", "_", s), ".csv"))) print(s)
  dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", s), ".csv"))
  dist2maize_1 <- rbind(dist2maize_1, dist2maize)
}

write.csv(dist2maize_1, paste0("distance2maize_", gsub(" ", "_", "all_37sp"), ".csv"), row.names = FALSE)

length(unique(dist2maize_1$species)) 
dist2maize <- dist2maize_1




## Modelling weeds distribution ####


library(ENMeval)
library(raster)
library(dplyr)
library(dismo)
library(data.table)
library(virtualspecies)
library(terra)


### Predictors ####

# see /home/rotllxa/Documents/European_butterflies_SDMs/SDMs_butterflies.R for full scripts

spat_data_dir <- "/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/"

worldclim_all <- stack(paste0(spat_data_dir, "/worldclim_all.tif"))
worldclim_all


lc_dir <- "/eos/jeodpp/home/users/rotllxa/land_cover/"
lc1km_files <- list.files(lc_dir, full.names = TRUE)[grepl("lc1km_", list.files(lc_dir))]
lc1km_files
lc1km_all <- stack(lc1km_files)
lc1km_all


worldclim_all <- stack(worldclim_all, lc1km_all)
worldclim_all
plot(worldclim_all[[5]])

## tests run with butterflies showed that not removing multicollinearity gave better results


### Background data ####

bckgr <- read.csv(paste0(spat_data_dir, "background_points.csv"), header = TRUE)


### Weeds occurrences ####

occs_all
length(unique(occs_all$species)) # 14027 species with name
nrow(occs_all)                   # 22300974 occurrences all years

occs_all_4modelling <- occs_all[occs_all$species %in% weeds_maize$Species, ]

setnames(occs_all_4modelling, c("decimalLongitude", "decimalLatitude"), c("x", "y"))

occs_all_4modelling <- occs_all_4modelling[, .SD, .SDcols = c("species", "x", "y", "sp2")]
occs_all_4modelling

check_sp2 <- unique(occs_all[, .SD, .SDcols = c("species", "sp2")])
#View(check_sp2)
length(unique(check_sp2$species)) == length(unique(check_sp2$sp2))  # if FALSE, there are repeated sp2 codes for several species
check_sp2$sp2[duplicated(check_sp2$sp2)]

sort(unique(occs_all[sp2 == "tar_sub", species]))
occs_all_4modelling[sp2 == "Galium_tricornutum", ]
occs_all[sp2 == "Galium_tricornutum", ]


length(weeds_maize$Species)                   # 204 weeds from the report
length(unique(occs_all_4modelling$species))   # 156 weeds from report with occurrences
nrow(occs_all_4modelling)                     # 2082796 occurrences for modelling (of 156 sps)

sort(table(occs_all_4modelling$species))
summary(as.vector(table(occs_all_4modelling$species))) # number of occurrences per species
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#  1.0    703.5   4308.5  13351.3  15296.2 141388.0 

quantile(as.vector(table(occs_all_4modelling$species)), seq(0, 1, 0.1)) # 10% of species have less than 50 occurrences
quantile(as.vector(table(occs_all_4modelling$species)), 0.05) # around 5% of species have less than 25 occurrences


tbl <- table(occs_all_4modelling$species)
ordr <- names(sort(tbl))
tbl <- data.frame(tbl)
spcies <- data.frame(taxons = unique(occs_all_4modelling$sp2), sps = unique(occs_all_4modelling$species))
spcies <- spcies[match(ordr, spcies$sps),]
spcies <- merge(spcies, tbl, by.x = "sps", by.y = "Var1")
spcies
## Excluding of the modelling those species with less than certain number of occurrences
#  The rule of thumb is usually 10 occurrences per variable; we have like 27 variables
#  but 270 occurrences is too much. The tests run for butterflies were giving good results 
#  with 100 occurrences with the same variables
head(spcies[order(spcies$Freq), ], 30)

#  We can try 50 occurrences as a minimum

spcies <- spcies[spcies$Freq >= 50, ]   # 140 species for modelling
head(spcies)
nrow(spcies)

sps_excluded <- as.vector(tbl$Var1[!tbl$Var1 %in% spcies$sps]) # 16 species excluded
#sps_excluded[sps_excluded %in% names(occs_00_indicators)]      # of which 9 are species indicator (mostly "poor indicators", though)
sps_excluded[sps_excluded %in% occs_00_indicators$Var1]      # of which 11 are species indicator (mostly "poor indicators", though)

taxons <- spcies$taxons
#spcies <- species[-c(1:which(species$taxons == "Phytolacca_americana") - 1), ]
spcies
taxons

### Modelling ####

info_models <- c()

# Threshold to use for converting to presence/absence
# Options: kappa,  spec_sens, no_omission, prevalence, equal_sens_spec, sensitivity

#threshold2use <- "sensitivity"    # deffault 0.9
threshold2use <- "no_omission"    # keeping all presences


for (t in taxons){
  #print(t)
  t0 <- Sys.time()
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running... ", sps))
  
  dir2save <- paste0("models_", t, "/")
  if(!dir.exists(paste0("models_", t))) {
    dir.create(dir2save)
  }
  
  occs_i <- occs_all[occs_all$sp2 %in% t, c("decimalLongitude", "decimalLatitude")]
  occurrences_GBIF <- nrow(occs_i)
  
  occs_i_shp <- SpatialPointsDataFrame(coords = occs_i[, c("decimalLongitude", "decimalLatitude")],
                                       data = data.frame(sp = rep(1, nrow(occs_i))),
                                       proj4string = CRS("+init=EPSG:4326"))
  #names(occs_i_shp) <- t
  occs_i_rstr <- rasterize(occs_i_shp, worldclim_all[[1]], field = "sp", background = 0)
  #names(occs_i_rstr) <- t
  #occs_i_rstr <- occs_i_rstr[[2]]
  occs_i_rstr <- mask(occs_i_rstr, worldclim_all[[1]])
  
  #assign(paste0(t, "_rstr"), occs_i_rstr)
  #print(sum(getValues(occs_i_rstr) == 1, na.rm = T))
  
  
  ## occurrences for training/testing
  sps_data <- stack(occs_i_rstr, worldclim_all) 
  sps_data <- as.data.table(as.data.frame(sps_data))
  sps_data[, raster_position := 1:nrow(sps_data)]
  
  # data set for presences
  sps_data_presences <- sps_data[layer == 1, ]
  sps_data_presences <- sps_data_presences[complete.cases(sps_data_presences), ]
  occurrences_1km <- nrow(sps_data_presences)
  rm(sps_data); gc()
  
  # data set for pseudo-absences
  sps_data_absences <- as.data.frame(raster::extract(worldclim_all, bckgr, cellnumbers = TRUE))
  sps_data_absences <- sps_data_absences[!sps_data_absences$cells %in% sps_data_presences$raster_position, ]
  names(sps_data_absences)
  

  ## Running ENMeval (https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0.0-vignette.html)
  ## Including a tryCatch to avoid stop process if there's an error because of a bug with "H" transformation or something
  
  
  dir_func <- function(sps_data_presences, worldclim_all, sps_data_absences, fc){ # to avoid stop modelling if low number of background points or other errors
    res <- tryCatch(
      {
        modl1 <- ENMevaluate(occs = sps_data_presences[, .SD, .SDcols = names(worldclim_all)], 
                             envs = NULL, 
                             bg = sps_data_absences[, names(sps_data_absences) %in% names(worldclim_all)], 
                             algorithm = 'maxnet', 
                             partitions = 'block', 
                             tune.args = list(
                               #fc = c("L","LQ","LQH","H"),
                               #fc = c("L","LQ","LQH"),  # removed "H" because there is some bug in maxnet that gives error for some species
                               #fc = c("L","LQ"),  # removed "H" and "LQH" because there is some bug in maxnet that gives error for some species
                               #fc = c("L","LQ"),
                               fc = fc,
                               rm = c(1, 2, 5)
                               #rm = 1:2
                               ),
                             parallel = TRUE,
                             #numCores = 7
                             numCores = 15
                             )
        
      },
      error = function(con){
        message(con)
        return(NULL)
      }
    )
    if(exists("modl1")){ return(modl1) }else{ return(NULL) }
  } #end of dir_func
  
  
  fc_opts <- list(c("L","LQ","LQH","H"), c("L","LQ","LQH"), c("L","LQ"), "L")
  
  for(fc in fc_opts){
    modl <- dir_func(sps_data_presences, worldclim_all, sps_data_absences, fc)
    if(!is.null(modl)) break
  }
    
    
  rm(sps_data_absences); gc()
  modl
  modl@results
  #View(modl@results)
  write.csv(modl@results, file = paste0(dir2save, "ENMeval_results_", t, ".csv"))
  save(modl, file = paste0(dir2save, "models_", t, ".RData"))
  #evalplot.stats(e = modl, stats = "or.mtp", color = "fc", x.var = "rm")
  
  
  occurrences_train <- nrow(modl@occs)
  occurrences_test <- nrow(modl@occs.testing)  # none because cross-validation
  background_points <- nrow(modl@bg)
  
  
  # selecting optimal model
  results <- eval.results(modl)
  results
  #View(results)
  optimal <- results %>% filter(delta.AICc == 0)
  optimal
  if(nrow(optimal) > 1) optimal <- optimal[1, ]
  
  modl_args <- eval.models(modl)[[optimal$tune.args]]
  modl_args$betas
  #str(modl_args)
  
  #dev.off()
  pdf(paste0(dir2save, "opt_model_RespCurves_", t, ".pdf"))
  plot(modl_args, type = "cloglog")
  # And these are the marginal response curves for the predictor variables wit non-zero 
  # coefficients in our model. We define the y-axis to be the cloglog transformation, which
  # is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
  # (Phillips et al. 2017).
  dev.off()
  
  modl <- modl@models[[optimal$tune.args]]
  gc()
  
  #save(modl, file = paste0(dir2save, "opt_model_", t, ".RData"))
  
  # making predictions
  #worldclim_all_data <- fread("worldclim_all_data_NoCor.csv", header = TRUE)
  #worldclim_all_data <- fread("worldclim_all_data_NoCor_085.csv", header = TRUE)
  worldclim_all_data <- fread(paste0(spat_data_dir, "worldclim_all_data.csv"), header = TRUE)
  names(worldclim_all_data) <- gsub("wc2.1_30s_bio_", "worldclim_all.", names(worldclim_all_data))
  names(worldclim_all_data) <- gsub("wc2.1_30s_elev", "worldclim_all.20", names(worldclim_all_data))
  #names(worldclim_all_data) <- gsub("worldclim_all", "worldclim_all", names(worldclim_all_data))
  worldclim_all_data <- worldclim_all_data[complete.cases(worldclim_all_data), ]
  sps_predictions_maxent <- predict(object = modl, 
                                    newdata = worldclim_all_data, 
                                    clamp = TRUE,
                                    type = c("cloglog")
  )
  rm(worldclim_all_data); gc()
  sps_predictions_maxent <- as.data.table(sps_predictions_maxent)
  head(sps_predictions_maxent)
  range(sps_predictions_maxent)
  nrow(sps_predictions_maxent)
  
  worldclim_all_data0 <- as.data.table(as.data.frame(worldclim_all[[1]]))
  worldclim_all_data0$raster_position <- 1:nrow(worldclim_all_data0)
  
  worldclim_all_data1 <- worldclim_all_data0
  worldclim_all_data1 <- worldclim_all_data1[complete.cases(worldclim_all_data1), ]
  
  worldclim_all_data0 <- worldclim_all_data0[, .SD, .SDcols = "raster_position"]
  
  worldclim_all_data1[, predictions := sps_predictions_maxent$V1]
  
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions")], 
                               by = "raster_position", all.x = TRUE)
  
  rm(worldclim_all_data1); gc()
  
  sps_preds_rstr <- worldclim_all[[1]]
  sps_preds_rstr <- setValues(sps_preds_rstr, worldclim_all_data0$predictions)
  
  rm(worldclim_all_data0); gc()
  
  
  #pdf("sps_predictions_maxent_kk.pdf", width = 20, height = 15)
  #par(mfrow = c(1, 2))
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #plot(occs_i_shp, add = TRUE, col = "black")
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #dev.off()
  
  
  #BI_mxnt <- ecospat::ecospat.boyce(fit = sps_preds_rstr,
  #                                  obs = linaria_pres_test_coords, 
  #                                  nclass = 0, 
  #                                  window.w = "default", 
  #                                  res = 100, 
  #                                  PEplot = TRUE)
  
  ## Creating presence/absence map
  # Threshold: minimum presence
  
  #threshold1 <- min(extract(sps_preds_rstr, occs_i_shp))
  #threshold1 <- quantile(extract(sps_preds_rstr, occs_i_shp), 0.1)#, na.rm = TRUE) # sensitivity = 0.9
  #threshold1
  
  thresholds <- dismo::threshold(evaluate(extract(sps_preds_rstr, occs_i_shp), extract(sps_preds_rstr, bckgr))) # sensitibity default 0.9
  thresholds
  #thresh2keep <- "sensitivity"     
  thresh2keep <- "no_omission"     # keeping all presences
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr, rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  #plot(sps_preds_rstr_pres_abs)
  
  
  pdf(paste0(dir2save, "sps_predictions_maxent_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr, zlim = c(0, 1), main = "Occurrences (GBIF - 1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr, zlim = c(0, 1), main = "MaxEnt predictions (cloglog)", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, "'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  
  running_time <- as.vector(Sys.time() - t0)
  
  data2save <- (data.frame(species = t, occurrences_GBIF, occurrences_1km, occurrences_train,
                           occurrences_test, background_points, optimal,
                           thresholds, threshold_used))
  rownames(data2save) <- t
  
  info_models <- rbind(info_models, data2save)
  #write.csv(info_models, "info_modelling_all_species.csv", row.names = FALSE)
  #write.csv(info_models, "info_modelling_all_species_085.csv", row.names = FALSE)
  write.csv(info_models, "info_modelling_all_species.csv", row.names = FALSE)
  #info_models <- fread("info_modelling_all_species.csv", header = TRUE)
  
  print(paste0(t, " run in: ", running_time))
}


### Checking results ####

info_models <- read.csv("info_modelling_all_species.csv", header = TRUE)
View(info_models)
info_models

mean(info_models$auc.val.avg)
mean(info_models$auc.train)
mean(info_models$cbi.val.avg)
mean(info_models$cbi.train)




## Species richness adjusted to potential richness ####

length(info_models$species)
taxons <- info_models$species

#t <- taxons[1]


spat_data_dir <- "/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/"
worldclim_all <- raster(paste0(spat_data_dir, "worldclim_all.tif"))
worldclim_all_data <- fread(paste0(spat_data_dir, "worldclim_all_data.csv"), header = TRUE)
worldclim_all_data <- worldclim_all_data[complete.cases(worldclim_all_data), ]
worldclim_all_data


sp_potential_richness_rstr <- worldclim_all[[1]]
names(sp_potential_richness_rstr) <- "potential_richness"
sp_potential_richness_rstr <- setValues(sp_potential_richness_rstr, 0)
sp_potential_richness_rstr


for (t in taxons){
  print(t)
  print(Sys.time())
  sp_dir <- paste0(getwd(), "/models_", t, "/")
  
  load(paste0(sp_dir, "models_", t, ".RData"), verbose = FALSE)
  modl
  
  results <- eval.results(modl)
  results
  #View(results)
  optimal <- results %>% filter(delta.AICc == 0)
  optimal
  if(nrow(optimal) > 1) optimal <- optimal[1, ]
  
  modl <- modl@models[[optimal$tune.args]]
  
  sps_predictions_maxent <- dismo::predict(object = modl, 
                                           newdata = worldclim_all_data, 
                                           clamp = TRUE,
                                           type = c("cloglog")
                                           )
  
  
  worldclim_all_data0 <- as.data.table(as.data.frame(worldclim_all[[1]]))
  worldclim_all_data0$raster_position <- 1:nrow(worldclim_all_data0)
  
  worldclim_all_data1 <- worldclim_all_data0
  worldclim_all_data1 <- worldclim_all_data1[complete.cases(worldclim_all_data1), ]
  
  worldclim_all_data0 <- worldclim_all_data0[, .SD, .SDcols = "raster_position"]
  
  worldclim_all_data1[, predictions := sps_predictions_maxent[, 1]]
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions")], 
                               by = "raster_position", all.x = TRUE)
  
  sps_preds_rstr <- worldclim_all[[1]]
  sps_preds_rstr <- setValues(sps_preds_rstr, worldclim_all_data0$predictions)
  
  
  threshold2 <- info_models[info_models$species == t, "threshold_used"]
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr, rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  
  
  make_plots <- "yes"
  make_plots <- "no"
  if(make_plots == "yes"){
    pdf(paste0(sp_dir, "sps_predictions_maxent_", t, "_kk.pdf"), width = 18, height = 15)
    par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
    par(mfrow = c(2, 2))
    plot(sps_preds_rstr, zlim = c(0, 1), main = "Occurrences (GBIF - 1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
    #plot(occs_i_shp, add = TRUE, col = "black")
    plot(sps_preds_rstr, zlim = c(0, 1), main = "MaxEnt predictions (cloglog)", cex.main = 2, cex.sub = 1.5)
    plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
         sub = paste0("Threshold: '", threshold2, "'"), 
         cex.main = 2, cex.sub = 1.5, legend = FALSE)
    title(list(paste0(t),
               cex = 4), 
          line = 1, outer = TRUE)
    
    dev.off()
    
  }
  
  sp_potential_richness_rstr <- sp_potential_richness_rstr + sps_preds_rstr_pres_abs
}


sp_potential_richness_rstr
writeRaster(sp_potential_richness_rstr, "sp_potential_richness.tif", overwrite = TRUE)

pdf(paste0("sp_potential_richness", ".pdf")
    #, width = 18, height = 15
    )
plot(sp_potential_richness_rstr)
dev.off()
#

#pdf(paste0("cropmap2018_maiz_1km", ".pdf"))
#plot(cropmap2018_maiz_1km)
#dev.off()


sp_potential_richness_rstr <- brick("sp_potential_richness.tif")
sp_potential_richness_rstr



sp_potential_richness_rstr_laea <- projectRaster(sp_potential_richness_rstr, to = cropmap2018_maiz_1km)
writeRaster(sp_potential_richness_rstr_laea, "sp_potential_richness_rstr_laea.tif", overwrite = TRUE)

pdf(paste0("sp_potential_richness_rstr_laea", ".pdf"))
plot(sp_potential_richness_rstr_laea)
dev.off()






length(unique(occs_maizeShare$species)) # 150 sp
sps_excluded     # 16 species excluded of modelling because of less than 50 occurences

sum(unique(occs_maizeShare$species) %in% weeds_maize$Species)
sum(weeds_maize$Species %in% unique(occs_maizeShare$species) )
sum(sps_excluded %in% unique(occs_maizeShare$species))
sum(!unique(occs_maizeShare$species) %in% sps_excluded)

length(unique(occs_all_4modelling$species))

spcies
nrow(spcies)

sum(spcies$sps %in% unique(occs_maizeShare$species))
sum(spcies$Freq >= 50)

sum(table(occs_maizeShare$species) >= 50)
sum(table(occs_maizeShare$species) < 50)

# We modelled the distribution of 140 sp. 
# We have 156 sp with data (2000-2021; coords uncert < 50m), out of the 204 weeds
# Out of the 156, we have 140 sp with >= 50 occurrences, which can be modelled
# Therefore, our richness potential for the whole community would be 140. But acording to the models,
# each pixel might have a lower potential

occs_maizeShare
# For 2018, out of the 156 sp, we have 150 sp with some data, from which 104 with >= 50 occurrences (but this is not relevant now)
# We have a little mismatch here, to keep it mind: in some pixels it could be more sp (150) than the maximum potential (140)

sp_potential_richness_rstr_laea

sp_potential_richness <- as.data.table(extract(sp_potential_richness_rstr_laea, unique(occs_maizeShare$cells)))
sp_potential_richness
sum(is.na(sp_potential_richness))
sum(!is.na(sp_potential_richness))

sp_potential_richness$V1 <- round(sp_potential_richness$V1, 0)
sp_potential_richness[, cells := unique(occs_maizeShare$cells)]
sp_potential_richness
setnames(sp_potential_richness, "V1", "potential_richness")
sp_potential_richness


occs_maizeShare_1 <- occs_maizeShare[, .SD, .SDcols = c("cells", "cropmap2018_maiz_1km")]
#occs_maizeShare_1 <- occs_maizeShare[, 6:9]
occs_maizeShare_1[!duplicated(occs_maizeShare_1$cells), ]

occs_maizeShare_1 <- unique(occs_maizeShare_1, by = "cells")

occs_maizeShare_1 <- merge(occs_maizeShare_1, occs_maizeShare_aggr, by.x = "cells", by.y = "V1", all.x = TRUE)
occs_maizeShare_1

occs_maizeShare_1 <- merge(occs_maizeShare_1, sp_potential_richness, by = "cells", all.x = TRUE)
occs_maizeShare_1

occs_maizeShare_1[, adjusted_richness := (N / potential_richness)]
occs_maizeShare_1

occs_maizeShare_1 <- occs_maizeShare_1[complete.cases(occs_maizeShare_1), ]

summary(occs_maizeShare_1$adjusted_richness)
hist(occs_maizeShare_1$adjusted_richness)


## Assessing correlations

pdf("scatterplot_SpRichness_potential_maize.pdf")
plot(y = occs_maizeShare_1$adjusted_richness,  # x
     #x = occs_maizeShare_1$EUCROPMAP_2018, # y
     #x = occs_maizeShare_1$cropmap2018_maiz_1km_cat, # y
     x = occs_maizeShare_1$cropmap2018_maiz_1km, # y
     main = "",
     ylab = "Adjusted Species Richness", 
     xlab = "Maize share", 
     pch = 19)
abline(lm(occs_maizeShare_1$adjusted_richness ~ occs_maizeShare_1$cropmap2018_maiz_1km), col = "red") # regression line (y~x)

pears_cor <- cor(occs_maizeShare_1$adjusted_richness, occs_maizeShare_1$cropmap2018_maiz_1km, method = "pearson")
mtext(paste0("Pearson's r = ", round(pears_cor, 3)),
      col = "black",
      side = 1, line = 3, 
      adj = 1,
      cex = 0.8)

lregr <- summary(lm(occs_maizeShare_1$adjusted_richness ~ occs_maizeShare_1$cropmap2018_maiz_1km))
lregr$adj.r.squared
mtext(paste0("R-squared = ", round(lregr$r.squared, 3)),
      col = "black",
      side = 1, line = 2, 
      adj = 1,
      cex = 0.8)

dev.off()














## EUCropMap against Agricultural Management Intensity Map (Rega et al., 2020) ####

list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega")  ## Agricultural Management Intensity Map (Rega et al., 2020)

raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Crop_management_systems_dom50_def.tif")

Absolute_intensity_5_clas_Fig3A <- raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Fig3A-Absolute_intensity_5_clas_.tif/Absolute_intensity_5_clas_Fig3A.tif")
Absolute_intensity_5_clas_Fig3A
plot(Absolute_intensity_5_clas_Fig3A)



Energy_input_2008_fille04_no_labour <- raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/Energy_input_2008_fille04_no_labour.tif")
Energy_input_2008_fille04_no_labour


## maize share aggregated at 1km from the Crop Map
cropmap2018_maiz_1km

cropmap2018_maiz_1km_vals <- getValues(cropmap2018_maiz_1km)
summary(cropmap2018_maiz_1km_vals)
rm(cropmap2018_maiz_1km_vals)

# removing pixels with vals < 0.01 (1% of maize in the 10km pixel) to remove some noise
cropmap2018_maiz_1km_clean <- cropmap2018_maiz_1km
cropmap2018_maiz_1km_clean[cropmap2018_maiz_1km < 0.01] <- NA 

plot(cropmap2018_maiz_1km_clean)
cropmap2018_maiz_1km_clean_vals <- getValues(cropmap2018_maiz_1km_clean)
summary(cropmap2018_maiz_1km_clean_vals)
range(cropmap2018_maiz_1km_clean_vals, na.rm = TRUE)
rm(cropmap2018_maiz_1km_clean_vals)


## Comparing maize share (crop Map) with Rega's map (absolute intensity)

cropmap2018_maiz_1km_clean 
cropmap2018_maiz_1km_clean_1 <- crop(cropmap2018_maiz_1km_clean, Absolute_intensity_5_clas_Fig3A)

extent(Absolute_intensity_5_clas_Fig3A) <- extent(cropmap2018_maiz_1km_clean_1)

plot(Absolute_intensity_5_clas_Fig3A)
plot(cropmap2018_maiz_1km_clean_1)


## Plotting correlation (scatterplot)
comp_df <- data.table(data.frame(getValues(Absolute_intensity_5_clas_Fig3A), getValues(cropmap2018_maiz_1km_clean_1)))
comp_df
sum(complete.cases(comp_df))
comp_df <- comp_df[complete.cases(comp_df), 1:2]
comp_df

# Pearson's correlation coefficient
comp_df_pearson <- cor(comp_df, method = "pearson")[2, 1]
comp_df_pearson
comp_df_pearson^2  # if we fit a linear regression (see below), this is R^2 (R squared)

perc_subsample <- 10   # percentage of points for plotting
perc_subsample <- 0.1   # percentage of points for plotting
perc_subsample <- 1   # percentage of points for plotting

num_subsample <- round((nrow(comp_df) * perc_subsample / 100), 0)
comp_df_subsample <- comp_df[sample(nrow(comp_df), num_subsample), ]

library(lattice)
jpeg(paste0("comp_CropMapMaize_Rega3A.jpg"))
xyplot(comp_df_subsample$getValues.Absolute_intensity_5_clas_Fig3A. ~ 
         comp_df_subsample$getValues.cropmap2018_maiz_1km_clean_1., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "Maize share (Crop Map)",
       ylab = "Absolute intensity of agrigultural management (Rega 3A)",
       main = paste0("Pearson's r = ", as.character(round(comp_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()



## maize share aggregated at 1km from the Crop Map
par(mfrow = c(1, 2))
dev.off()

cropmap2018_maiz_1km
plot(cropmap2018_maiz_1km)

cropmap2018_maiz_1km_vals <- getValues(cropmap2018_maiz_1km)
summary(cropmap2018_maiz_1km_vals)
rm(cropmap2018_maiz_1km_vals)

# removing pixels with vals < 0.01 (1% of maize in the 1km pixel) to remove some noise
cropmap2018_maiz_1km_clean <- cropmap2018_maiz_1km
cropmap2018_maiz_1km_clean[cropmap2018_maiz_1km < 0.01] <- NA 

plot(cropmap2018_maiz_1km_clean)
cropmap2018_maiz_1km_clean_vals <- getValues(cropmap2018_maiz_1km_clean)
summary(cropmap2018_maiz_1km_clean_vals)
range(cropmap2018_maiz_1km_clean_vals, na.rm = TRUE)
rm(cropmap2018_maiz_1km_clean_vals)



## Comparing maize share (crop Map) with Rega's map (Energy_input_2008_fille04_no_labour)
#  This data comes from CAPRI and it is calculated as the 2008 baseline

cropmap2018_maiz_1km_clean 
cropmap2018_maiz_1km_clean_1 <- crop(cropmap2018_maiz_1km_clean, Energy_input_2008_fille04_no_labour)
#cropmap2018_maiz_1km_clean_1 <- crop(cropmap2018_maiz_1km, Energy_input_2008_fille04_no_labour)

extent(Energy_input_2008_fille04_no_labour) <- extent(cropmap2018_maiz_1km_clean_1)

plot(Energy_input_2008_fille04_no_labour)
plot(cropmap2018_maiz_1km_clean_1)


## Plotting correlation (scatterplot)
comp_df <- data.table(data.frame(getValues(Energy_input_2008_fille04_no_labour), getValues(cropmap2018_maiz_1km_clean_1)))
comp_df
sum(complete.cases(comp_df))
comp_df <- comp_df[complete.cases(comp_df), 1:2]
comp_df

# Pearson's correlation coefficient
comp_df_pearson <- cor(comp_df, method = "pearson")[2, 1]
comp_df_pearson
comp_df_pearson^2  # if we fit a linear regression (see below), this is R^2 (R squared)

perc_subsample <- 10   # percentage of points for plotting
perc_subsample <- 0.1   # percentage of points for plotting
perc_subsample <- 1   # percentage of points for plotting

num_subsample <- round((nrow(comp_df) * perc_subsample / 100), 0)
comp_df_subsample <- comp_df[sample(nrow(comp_df), num_subsample), ]

library(lattice)
jpeg(paste0("comp_CropMapMaize_Energy_input_2008_fille04_no_labour.jpg"))
xyplot(comp_df_subsample$getValues.Energy_input_2008_fille04_no_labour. ~ 
         comp_df_subsample$getValues.cropmap2018_maiz_1km_clean_1., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "Maize share (Crop Map)",
       ylab = "Energy_input_2008_fille04_no_labour",
       main = paste0("Pearson's r = ", as.character(round(comp_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()





## EUCropMap against N and P fertilizer consumption ####

list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/dataRestoration/N_fert_consumption/")  
NMINSL <- raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/dataRestoration/N_fert_consumption/NMINSL.tif")  # N
NMINSL  # 16km grid


list.files("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/dataRestoration/P_fert_2018/")  
PMINSL_20181 <- raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/dataRestoration/P_fert_2018/PMINSL_20181.tif")  # P
PMINSL_20181   # 14 km grid



cropmap2018_maiz_16km

# removing pixels with vals < 0.01 (1% of maize in the 16km pixel) to remove some noise
cropmap2018_maiz_16km_clean <- cropmap2018_maiz_16km
cropmap2018_maiz_16km_clean[cropmap2018_maiz_16km < 0.01] <- NA 

cropmap2018_maiz_16km_clean_1 <- crop(cropmap2018_maiz_16km_clean, NMINSL)
#cropmap2018_maiz_16km_clean_1 <- crop(cropmap2018_maiz_16km, NMINSL)

cropmap2018_maiz_16km_clean_1 <- resample(cropmap2018_maiz_16km_clean_1, NMINSL, method="bilinear")
#extent(NMINSL) <- extent(cropmap2018_maiz_16km_clean_1)

plot(NMINSL)
plot(cropmap2018_maiz_16km_clean_1)


## Plotting correlation (scatterplot)
comp_df <- data.table(data.frame(getValues(NMINSL), getValues(cropmap2018_maiz_16km_clean_1)))
comp_df
sum(complete.cases(comp_df))
comp_df <- comp_df[complete.cases(comp_df), 1:2]
comp_df

# Pearson's correlation coefficient
comp_df_pearson <- cor(comp_df, method = "pearson")[2, 1]
comp_df_pearson
comp_df_pearson^2  # if we fit a linear regression (see below), this is R^2 (R squared)

perc_subsample <- 10   # percentage of points for plotting
perc_subsample <- 0.1   # percentage of points for plotting
perc_subsample <- 1   # percentage of points for plotting
perc_subsample <- 100   # percentage of points for plotting

num_subsample <- round((nrow(comp_df) * perc_subsample / 100), 0)
comp_df_subsample <- comp_df[sample(nrow(comp_df), num_subsample), ]

library(lattice)
jpeg(paste0("comp_CropMapMaize_NMINSL.jpg"))
xyplot(comp_df_subsample$getValues.NMINSL. ~ 
         comp_df_subsample$getValues.cropmap2018_maiz_16km_clean_1., 
       type = c("p", "r"),
       col.line = "red",
       xlab = "Maize share (Crop Map)",
       ylab = "NMINSL (N fertilizer consumption)",
       main = paste0("Pearson's r = ", as.character(round(comp_df_pearson, 4))),
       sub = paste0("Plotting a random subsample of ", num_subsample, " (", perc_subsample, "%) points")
)
dev.off()







## Share of Arable/Non Arable land in Maize Pixels ####

cropmap2018_maiz_1km
cropmap2018_maiz_1km_clean  # maize share > 0.01 (1%)

cropmap2018_arabland_1km <- raster("cropmap2018_ArableLand_1km.tif")
plot(cropmap2018_arabland_1km)

comp_df <- data.table(data.frame(getValues(cropmap2018_arabland_1km), getValues(cropmap2018_maiz_1km_clean)))
comp_df <- data.table(data.frame(getValues(cropmap2018_arabland_1km), getValues(cropmap2018_maiz_1km)))  # keeping all maize pixels
comp_df
sum(complete.cases(comp_df))
comp_df <- comp_df[complete.cases(comp_df), 1:2]
comp_df


summary(comp_df$getValues.cropmap2018_arabland_1km.)  # maize share > 0.01 (1%)
#   Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.0101  0.1953  0.4241  0.4463  0.6752  1.0000 

# keeping all maize pixels
# Min.   1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.0000  0.0464  0.2522  0.3439  0.5986  1.0000 




## Share of arable land in pixels with indicator species ####
dist2maize <- fread("distance2maize_Galium_tricornutum.csv")
dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".csv"))
#dist2maize <- dist2maize_1

occs_all1 <- fread(paste0(getwd(), "/../exploring_lucas_data/D5_FFGRCC_gbif_occ/sp_records_20210709.csv"), header = TRUE)
occs_all1

#occs_all1_galium_tricornutum <- occs_all1[species == "Galium tricornutum" & year == 2018, ]
occs_all1_galium_tricornutum <- occs_all1[species %in% sp_indic_1 & year == 2018, ]
occs_all1_galium_tricornutum

occs_all1_galium_tricornutum <- st_as_sf(as.data.frame(occs_all1_galium_tricornutum), coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)#, agr = "constant")
occs_all1_galium_tricornutum


occs_all1_galium_tricornutum <- st_transform(occs_all1_galium_tricornutum, crs = crs(cropmap2018_arabland_1km))
occs_all1_galium_tricornutum

share_arable_galium_tricornutum <- as.data.table(extract(cropmap2018_arabland_1km, occs_all1_galium_tricornutum, sp = TRUE))
share_arable_galium_tricornutum  

summary(share_arable_galium_tricornutum$cropmap2018_ArableLand_1km)
#summary(share_arable_galium_tricornutum[species == "Digitaria ischaemum"]$cropmap2018_ArableLand_1km)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     # Galium tricornutum
# 0.01880 0.08445 0.19575 0.26499 0.34490 0.97720 

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's  # Digitaria ischaemum
# 0.0558  0.2130  0.4322  0.4131  0.5777  0.8482       1 

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's   # Xanthium spinosum
# 0.0007  0.0741  0.1677  0.2162  0.3790  0.4564       4 


#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's   # Abutilon hteophrasti (no indicator; appearing in all maize share classes)
#  0.0049  0.2723  0.4601  0.4551  0.6249  0.9641      30 

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's    # Cyperus rotundus (indicator; monocot; 7 occs on maize) 
#  0.1904  0.2915  0.3930  0.3773  0.4211  0.6327       1  



share_arable_galium_tricornutum_1 <- share_arable_galium_tricornutum[, .SD, .SDcols = c("gbifID", "cropmap2018_ArableLand_1km")]
dist2maize <- merge(dist2maize, share_arable_galium_tricornutum_1, by = "gbifID")
dist2maize


# 10 km
share_arable_galium_tricornutum <- as.data.table(extract(cropmap2018_arabland_10km, occs_all1_galium_tricornutum, sp = TRUE))
share_arable_galium_tricornutum  

summary(share_arable_galium_tricornutum$cropmap2018_ArableLand_10km)
#summary(share_arable_galium_tricornutum[species == "Digitaria ischaemum"]$cropmap2018_ArableLand_10km)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.       # Galium tricornutum
# 0.02225 0.10723 0.21178 0.25930 0.34203 0.99267
# Just slightly differences if we compare with shares of Arable land in 1km grid

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    # Digitria ischaemum
# 0.08361 0.29676 0.60073 0.52435 0.73646 0.95515 

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's   # Xanthium spinosum
# 0.02275 0.07621 0.25351 0.22916 0.34817 0.38236       4 


#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's   # Abutilon
# 0.02282 0.28626 0.44538 0.43223 0.56448 0.91718      29 

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     #  Cyperus rotundus (indicator; monocot; 7 occs on maize) 
#  0.2717  0.6396  0.7779  0.6955  0.8217  0.9763 


share_arable_galium_tricornutum_10 <- share_arable_galium_tricornutum[, .SD, .SDcols = c("gbifID", "cropmap2018_ArableLand_10km")]
dist2maize <- merge(dist2maize, share_arable_galium_tricornutum_10, by = "gbifID")
#View(dist2maize)
dist2maize

#write.csv(dist2maize, "distance2maize_Galium_tricornutum.csv", row.names = FALSE)
#write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".csv"), row.names = FALSE)
#write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", "all_50sp"), ".csv"), row.names = FALSE)
#write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", "all_40sp"), ".csv"), row.names = FALSE)
write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", "all_37sp"), ".csv"), row.names = FALSE)



summary(dist2maize$dist2maize)   # distance to the center of the closest maize pixel (1km)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's    # Galium tricornutum
#  33.0   248.5   446.0   477.1   773.5   969.0       3 

# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   # Digitaria 
# 12.00   36.25   59.00   97.65  110.75  626.00 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's   # Xanthium spinosum
# 22.0   120.2   360.0   369.2   518.0   915.0       7 


#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's     # Abutilon theophrasti
#   1.00   16.00    45.00     92.59  103.00    827.00      49 

#   Min.  1st Qu.  Median    Mean 3rd Qu.    Max.    NA's     # Cyperus rotundus (indicator; monocot; 7 occs on maize) 
#  204.0   391.0   413.0   499.6   504.0   986.0       3 


## boxplot
#sp_indic_1 <- occs_00_indicators[1, Var1] # best indicator sp (Galium tricornutum)
#sp_indic_1 <- occs_00_indicators[2, Var1] # 2nd best indicator sp (Digitaria ischaemum)
#sp_indic_1 <- occs_00_indicators[3, Var1] # 3rd best indicator sp (Xanthium spinosum)

dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".csv"))
#dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", "all_50sp"), ".csv"))

#jpeg("dist2maize_maizeShare_Galium_tricornutum.jpg", width = 10, height = 6, units = "cm", res = 300)
jpeg(paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".jpg"), width = 10, height = 6, units = "cm", res = 300)
par(mfrow = c(1, 3))

boxplot(dist2maize$dist2maize,
        ylim = c(0, 1000),
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)")

boxplot(dist2maize$cropmap2018_ArableLand_1km, 
        ylim = c(0, 1),
        main = "",
        xlab = "", 
        ylab = "Share of arable land (1km)")

boxplot(dist2maize$cropmap2018_ArableLand_10km, 
        ylim = c(0, 1),
        main = "",
        xlab = "", 
        ylab = "Share of arable land (10km)")

#title(expression(paste(italic("Galium tricornutum"), " occurrences")), line = - 2, outer = TRUE)
title(bquote(paste(italic(.(sp_indic_1)), " occurrences")), line = - 2, outer = TRUE)

dev.off()




## All sps. together
sps <- c(occs_00_indicators[c(1:3, 8), Var1], "Abutilon theophrasti")
sps <- c(occs_00_indicators[, Var1], "Abutilon theophrasti")
sps <- sp_indic_1
#sps <- c(sp_indic_1_kk, sp_indic_1)
sps

dist2maize_1 <- fread(paste0("distance2maize_", gsub(" ", "_", sps[1]), ".csv"))

for (s in sps[-1]) {
  dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", s), ".csv"))
  dist2maize_1 <- rbind(dist2maize_1, dist2maize)
}

length(unique(dist2maize_1$species))

write.csv(dist2maize_1, paste0("distance2maize_", gsub(" ", "_", "all_37sp"), ".csv"), row.names = FALSE)


dist2maize_1
#dist2maize_1 <- dist2maize
#dist2maize_1 <- fread("distance2maize_all_50sp.csv", header = TRUE)
#length(unique(dist2maize_1$species))
#identical(sort(unique(dist2maize_1$species)), sort(sp_indic_1))

#dist2maize_1$species <- factor(dist2maize_1$species, unique(dist2maize_1$species))
dist2maize_1$species <- factor(dist2maize_1$species, sps)

#library("RColorBrewer")
#display.brewer.pal(n = length(unique(dist2maize_1$species)), name = 'RdBu')

library("viridis")
library("scales")
colrs <- viridis_pal()(length(unique(dist2maize_1$species)))
#colrs <- viridis_pal()(2)
show_col(colrs)
#my_colrs <- ifelse(levels(dist2maize_1$species) %in% sps[1:4], colrs[1],
#                   colrs[2])


#jpeg(paste0("distance2maize_", "all_50sp", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
#jpeg(paste0("distance2maize_", "all_40sp", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
par(mfrow = c(2, 2), mar = c(2, 5, 2, 2))

boxplot(dist2maize_1$dist2maize ~ dist2maize_1$species,
        ylim = c(0, 1000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:12, 14:38),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
axis(1, cex.axis = 0.7, at = c(1:12, 14:38), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
legend("center", 
       #legend = c("Indicator species","Not Indicator sps."), 
       legend = paste0(1:length(sps), "-", levels(dist2maize_1$species)), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch=20 , pt.cex = 3, cex = 0.8, 
       horiz = FALSE)#, 
#inset = - 1)


#boxplot(dist2maize_1$cropmap2018_ArableLand_1km  ~ dist2maize_1$species, 
boxplot(dist2maize_1$cropmap2018_maiz_1km  ~ dist2maize_1$species, 
        ylim = c(0, 0.2),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:12, 14:38),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


boxplot(dist2maize_1$cropmap2018_ArableLand_1km ~ dist2maize_1$species, 
        ylim = c(0, 1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of arable land (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#par(xpd = TRUE)

dev.off()



## Some results for reporting
dist2maize

dist2maize_indicators <- dist2maize[!species %in% sp_NoIndic, ]
unique(dist2maize_indicators$species)

dist2maize_no_indicators <- dist2maize[species %in% sp_NoIndic, ]
unique(dist2maize_no_indicators$species)


names(dist2maize_indicators)

sum(is.na(dist2maize_indicators$dist2maize))
length(dist2maize_indicators$dist2maize)

round(mean(dist2maize_indicators$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_indicators$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_indicators$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(sd(dist2maize_indicators$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(mean(dist2maize_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)

round(mean(dist2maize_no_indicators$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_no_indicators$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_no_indicators$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(sd(dist2maize_no_indicators$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(mean(dist2maize_no_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_no_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)


# first 5 indicators
first_5 <- c("Galium tricornutum", "Digitaria ischaemum", "Xanthium spinosum", "Chrozophora tinctoria", "Caucalis platycarpos")

dist2maize_5 <- dist2maize[species %in% first_5, ]

round(mean(dist2maize_5$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_5$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_5$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(sd(dist2maize_5$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(mean(dist2maize_5$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_5$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)

# Galium
dist2maize_galium <- dist2maize[species %in% c("Galium tricornutum"), ]
round(mean(dist2maize_galium$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_galium$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_galium$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(sd(dist2maize_galium$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(mean(dist2maize_galium$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_galium$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)

# Digitaria ischaemum
dist2maize_digitaria <- dist2maize[species %in% c("Digitaria ischaemum"), ]
round(mean(dist2maize_digitaria$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_digitaria$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_digitaria$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(sd(dist2maize_digitaria$cropmap2018_ArableLand_1km, na.rm = TRUE), 2)
round(mean(dist2maize_digitaria$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_digitaria$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)







## Share of maize in pixels with indicator species ####

dist2maize
sp_indic_1
cropmap2018_maiz_1km
cropmap2018_maiz_10km

occs_all1 <- fread(paste0(getwd(), "/../exploring_lucas_data/D5_FFGRCC_gbif_occ/sp_records_20210709.csv"), header = TRUE)
occs_all1

#occs_all1_galium_tricornutum <- occs_all1[species == "Galium tricornutum" & year == 2018, ]
occs_all1_sp_indic_1 <- occs_all1[species %in% sp_indic_1 & year == 2018, ]
occs_all1_sp_indic_1
unique(occs_all1_sp_indic_1$year)
length(unique(occs_all1_sp_indic_1$species))

occs_all1_sp_indic_1 <- st_as_sf(as.data.frame(occs_all1_sp_indic_1), coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)#, agr = "constant")
occs_all1_sp_indic_1


occs_all1_sp_indic_1 <- st_transform(occs_all1_sp_indic_1, crs = crs(cropmap2018_maiz_1km))
occs_all1_sp_indic_1

share_maize_sp_indic_1 <- as.data.table(extract(cropmap2018_maiz_1km, occs_all1_sp_indic_1, sp = TRUE))
share_maize_sp_indic_1  

summary(share_maize_sp_indic_1$cropmap2018_maiz_1km)
#summary(share_maize_sp_indic_1[species == "Xanthium spinosum"]$cropmap2018_maiz_1km)
#    Min.   1st Qu.  Median    Mean    3rd Qu.    Max.     # Galium tricornutum
# 0.000000 0.000000 0.000300 0.002757 0.001400 0.059100 

#    Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    NA's  # Digitaria ischaemum
# 0.00530 0.00895 0.01570 0.03803  0.06975 0.09740       1

#    Min.   1st Qu.  Median    Mean    3rd Qu.    Max.    NA's   # Xanthium spinosum
# 0.000000 0.000000 0.000100 0.004462  0.001700  0.031800        4 




share_maize_sp_indic_1 <- share_maize_sp_indic_1[, .SD, .SDcols = c("gbifID", "cropmap2018_maiz_1km")]
dist2maize <- merge(dist2maize, share_maize_sp_indic_1, by = "gbifID")
dist2maize
sum(is.na(dist2maize))
dist2maize[is.na(dist2maize), ]
sum(is.na(dist2maize$dist2maize))
sum(is.na(dist2maize$cropmap2018_maiz_1km))
complete.cases()
range(na.omit(dist2maize)$dist2maize)


# 10 km
share_maize_sp_indic_1 <- as.data.table(extract(cropmap2018_maiz_10km, occs_all1_sp_indic_1, sp = TRUE))
share_maize_sp_indic_1  

summary(share_maize_sp_indic_1$cropmap2018_maiz_10km)
#summary(share_maize_sp_indic_1[species == "Galium tricornutum"]$cropmap2018_maiz_10km)
#    Min.   1st Qu.  Median    Mean      3rd Qu.    Max.       # Galium tricornutum
# 0.000003 0.000252  0.000591  0.005258  0.004189   0.051569
# Just small differences if we compare with maize shares in 1km grid

#    Min.   1st Qu.  Median    Mean     3rd Qu.    Max.    # Digitria ischaemum
# 0.001307 0.010580 0.030112 0.028653  0.042224  0.067231  



share_arable_sp_indic_10 <- share_maize_sp_indic_1[, .SD, .SDcols = c("gbifID", "cropmap2018_maiz_10km")]
dist2maize <- merge(dist2maize, share_arable_sp_indic_10, by = "gbifID")
#View(dist2maize)
dist2maize

#write.csv(dist2maize, "distance2maize_Galium_tricornutum.csv", row.names = FALSE)
#write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".csv"), row.names = FALSE)
#write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", "all_50sp"), ".csv"), row.names = FALSE)
write.csv(dist2maize, paste0("distance2maize_", gsub(" ", "_", "all_37sp"), ".csv"), row.names = FALSE)


summary(dist2maize$dist2maize)   # distance to the center of the closest maize pixel (1km)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's    # Galium tricornutum
#  33.0   248.5   446.0   477.1   773.5   969.0       3 

# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   # Digitaria 
# 12.00   36.25   59.00   97.65  110.75  626.00 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's   # Xanthium spinosum
# 22.0   120.2   360.0   369.2   518.0   915.0       7 


#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's     # Abutilon theophrasti
#   1.00   16.00    45.00     92.59  103.00    827.00      49 

#   Min.  1st Qu.  Median    Mean 3rd Qu.    Max.    NA's     # Cyperus rotundus (indicator; monocot; 7 occs on maize) 
#  204.0   391.0   413.0   499.6   504.0   986.0       3 


## boxplot
#sp_indic_1 <- occs_00_indicators[1, Var1] # best indicator sp (Galium tricornutum)
#sp_indic_1 <- occs_00_indicators[2, Var1] # 2nd best indicator sp (Digitaria ischaemum)
#sp_indic_1 <- occs_00_indicators[3, Var1] # 3rd best indicator sp (Xanthium spinosum)

#dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".csv"))
dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", "all_50sp"), ".csv"))

##jpeg("dist2maize_maizeShare_Galium_tricornutum.jpg", width = 10, height = 6, units = "cm", res = 300)
#jpeg(paste0("distance2maize_", gsub(" ", "_", sp_indic_1), ".jpg"), width = 10, height = 6, units = "cm", res = 300)
#par(mfrow = c(1, 3))
#
#boxplot(dist2maize$dist2maize,
#        ylim = c(0, 1000),
#        main = "",
#        xlab = "", 
#        ylab = "Distance to maize (m)")
#
#boxplot(dist2maize$cropmap2018_maiz_1km, 
#        ylim = c(0, 1),
#        main = "",
#        xlab = "", 
#        ylab = "Share of arable land (1km)")
#
#boxplot(dist2maize$cropmap2018_maiz_10km, 
#        ylim = c(0, 1),
#        main = "",
#        xlab = "", 
#        ylab = "Share of arable land (10km)")
#
##title(expression(paste(italic("Galium tricornutum"), " occurrences")), line = - 2, outer = TRUE)
#title(bquote(paste(italic(.(sp_indic_1)), " occurrences")), line = - 2, outer = TRUE)
#
#dev.off()




## All sps. together
sps <- c(occs_00_indicators[c(1:3, 8), Var1], "Abutilon theophrasti")
sps <- c(occs_00_indicators[, Var1], "Abutilon theophrasti")
sps <- sp_indic_1
sps

#dist2maize_1 <- fread(paste0("distance2maize_", gsub(" ", "_", sps[1]), ".csv"))
#
#for (s in sps[-1]) {
#  dist2maize <- fread(paste0("distance2maize_", gsub(" ", "_", s), ".csv"))
#  dist2maize_1 <- rbind(dist2maize_1, dist2maize)
#}
#
#dist2maize_1
dist2maize_1 <- dist2maize
#dist2maize_1 <- fread("distance2maize_all_50sp.csv", header = TRUE)

length(unique(dist2maize_1$species))

#dist2maize_1$species <- factor(dist2maize_1$species, unique(dist2maize_1$species))
dist2maize_1$species <- factor(dist2maize_1$species, sps)

#library("RColorBrewer")
#display.brewer.pal(n = length(unique(dist2maize_1$species)), name = 'RdBu')

library("viridis")
library("scales")
colrs <- viridis_pal()(length(unique(dist2maize_1$species)))
#colrs <- viridis_pal()(2)
show_col(colrs)
save(colrs, file = "colrs.RData")
#my_colrs <- ifelse(levels(dist2maize_1$species) %in% sps[1:4], colrs[1],
#                   colrs[2])

jpeg(paste0("distance2maize_", "all_37sp", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
jpeg(paste0("distance2maize_", "all_50sp_MaizeShare", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
#jpeg(paste0("distance2maize_", "all_40sp", ".jpg"), width = 31, height = 24, units = "cm", res = 300)

par(mfrow = c(2, 2), mar = c(2, 5, 2, 2))

boxplot(dist2maize_1$dist2maize ~ dist2maize_1$species,
        ylim = c(0, 1000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:25, 27:36, 38:42),
        ann = TRUE)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
legend("center", 
       #legend = c("Indicator species","Not Indicator sps."), 
       legend = paste0(1:length(sps), "-", levels(dist2maize_1$species)), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch=20 , pt.cex = 3, cex = 0.8, 
       horiz = FALSE)#, 
#inset = - 1)


boxplot(dist2maize_1$cropmap2018_maiz_1km  ~ dist2maize_1$species, 
        #ylim = c(0, 0.2),
        ylim = c(0, 0.15),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:25, 27:36, 38:42),
        ann = TRUE)
#mtext("Indicator Species                      Non-indicator Species", side = 3)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


boxplot(dist2maize_1$cropmap2018_ArableLand_1km ~ dist2maize_1$species, 
        ylim = c(0, 1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of arable land (scale: 1km)",
        xaxt = 'n', 
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:25, 27:36, 38:42),
        ann = TRUE)
#mtext("Indicator Species                      Non-indicator Species", side = 3)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


#par(xpd = TRUE)

dev.off()



## Some results for reporting
dist2maize

dist2maize_indicators <- dist2maize[!species %in% sp_NoIndic, ]
unique(dist2maize_indicators$species)

dist2maize_no_indicators <- dist2maize[species %in% sp_NoIndic, ]
unique(dist2maize_no_indicators$species)


names(dist2maize_indicators)

sum(is.na(dist2maize_indicators$dist2maize))
length(dist2maize_indicators$dist2maize)

round(mean(dist2maize_indicators$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_indicators$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 3)    # 0.012
round(sd(dist2maize_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 3)      #0.024
round(mean(dist2maize_indicators$cropmap2018_maiz_10km, na.rm = TRUE), 3)  #0.02
round(sd(dist2maize_indicators$cropmap2018_maiz_10km, na.rm = TRUE), 3)    #0.038

round(mean(dist2maize_no_indicators$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_no_indicators$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_no_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 3)   # 0.047
round(sd(dist2maize_no_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 3)     # 0.078
round(mean(dist2maize_no_indicators$cropmap2018_maiz_10km, na.rm = TRUE), 3)   # 0.055
round(sd(dist2maize_no_indicators$cropmap2018_maiz_10km, na.rm = TRUE), 3)     # 0.062


# first 5 indicators
first_5 <- c("Galium tricornutum", "Digitaria ischaemum", "Xanthium spinosum", "Chrozophora tinctoria", "Caucalis platycarpos")

dist2maize_5 <- dist2maize[species %in% first_5, ]

round(mean(dist2maize_5$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_5$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_5$cropmap2018_maiz_1km, na.rm = TRUE), 3)    # 0.009
round(sd(dist2maize_5$cropmap2018_maiz_1km, na.rm = TRUE), 3)      # 0.021
round(mean(dist2maize_5$cropmap2018_maiz_10km, na.rm = TRUE), 3)     # 0.01
round(sd(dist2maize_5$cropmap2018_maiz_10km, na.rm = TRUE), 3)       # 0.018

# Galium
dist2maize_galium <- dist2maize[species %in% c("Galium tricornutum"), ]
round(mean(dist2maize_galium$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_galium$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_galium$cropmap2018_maiz_1km, na.rm = TRUE), 3)  # 0.003
round(sd(dist2maize_galium$cropmap2018_maiz_1km, na.rm = TRUE), 3)    # 0.009
round(mean(dist2maize_galium$cropmap2018_maiz_10km, na.rm = TRUE), 3)  # 0.005
round(sd(dist2maize_galium$cropmap2018_maiz_10km, na.rm = TRUE), 3)    # 0.012

# Digitaria ischaemum
dist2maize_digitaria <- dist2maize[species %in% c("Digitaria ischaemum"), ]
round(mean(dist2maize_digitaria$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_digitaria$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_digitaria$cropmap2018_maiz_1km, na.rm = TRUE), 3)   # 0.038
round(sd(dist2maize_digitaria$cropmap2018_maiz_1km, na.rm = TRUE), 3)     # 0.034
round(mean(dist2maize_digitaria$cropmap2018_maiz_10km, na.rm = TRUE), 3)    # 0.029
round(sd(dist2maize_digitaria$cropmap2018_maiz_10km, na.rm = TRUE), 3)      # 0.021

# Digitaria ischaemum
dist2maize_xanthium <- dist2maize[species %in% c("Xanthium spinosum"), ]
round(mean(dist2maize_xanthium$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_xanthium$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_xanthium$cropmap2018_maiz_1km, na.rm = TRUE), 3)   # 0.004
round(sd(dist2maize_xanthium$cropmap2018_maiz_1km, na.rm = TRUE), 3)     # 0.01
round(mean(dist2maize_xanthium$cropmap2018_maiz_10km, na.rm = TRUE), 3)    # 0.029
round(sd(dist2maize_xanthium$cropmap2018_maiz_10km, na.rm = TRUE), 3)      # 0.021








## Validation with LPIS for FR ####

cropmap2018 <- raster("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/2018/EUCROPMAP_2018.tif")  # at 10m
cropmap2018


fr_coords <-  c(3100000, 4300000, 2200000, 3400000)   # France (LAEA, m) (xmin, xmax, ymin, ymax)
fr_coords <-  c(3232000, 4284000, 2073000, 3134000)   # France (LPIS Martin)




# LPIS 
library(RPostgreSQL)
library(sf)

drv <- dbDriver("PostgreSQL")
connec <- dbConnect(drv, 
                    dbname = "refocus_gsaa_db",
                    host = "jeodb01.cidsn.jrc.it", 
                    port = 54321,
                    user = "refocus_gsaa_user_ro", 
                    password = "K2Xo5RcDPQ")


#maize_fr_2018_db <- dbGetQuery(connec, "SELECT * FROM cropdiversity.maize_fr_2018")
#str(maize_fr_2018_db)


## LPIS. Maize fields

maize_fr_2018 <- st_read(connec, query = "SELECT * FROM cropdiversity.maize_fr_2018")
maize_fr_2018

#pdf("lpis_FR_2018.pdf")
#plot(x)
#dev.off()


## LPIS. Maize aggregated 1km

croparea_maize_fr_1k_2018 <- st_read(connec, query = "SELECT * FROM cropdiversity.croparea_maize_fr_1k_2018")
croparea_maize_fr_1k_2018
range(croparea_maize_fr_1k_2018$area)   # area is maize share in ‰ (per mile; to be divided by 10^4 to get %) 
summary(croparea_maize_fr_1k_2018$area) / 10000 
#    Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#   0.000   2.645   7.458  11.247  16.164  100.000 



# How many occs in FR?
occs_all_2018_maiz_fr <- occs_all_2018_maiz[occs_all_2018_maiz$countryCode == "FR", ]
occs_all_2018_maiz_fr

occs_00_indicators <- fread("sp_indicators.csv", header = TRUE)
occs_00_indicators

occs_all_2018_maiz_fr_indic <- occs_all_2018_maiz_fr[species %in% occs_00_indicators$Var1, ]
table(occs_all_2018_maiz_fr_indic$species)



## Distance to maize field and tolerance to maize share

occs_all_2018_maiz_sf_laea
st_crs(occs_all_2018_maiz_sf_laea)
st_crs(maize_fr_2018)

occs_all_2018_maiz_sf_laea2 <- st_transform(occs_all_2018_maiz_sf_laea, crs = st_crs(maize_fr_2018))
occs_all_2018_maiz_sf_laea2


#sp_indic_1 <- occs_00_indicators$Var1
load("sp_indic_1.RData", verbose = TRUE)
sp_indic_1


occs_all_2018_maiz_sf_laea_indic1 <- occs_all_2018_maiz_sf_laea2[occs_all_2018_maiz_sf_laea2$species %in% sp_indic_1, ]
occs_all_2018_maiz_sf_laea_indic1 <- occs_all_2018_maiz_sf_laea_indic1[occs_all_2018_maiz_sf_laea_indic1$countryCode == "FR", ]
occs_all_2018_maiz_sf_laea_indic1
length(unique(occs_all_2018_maiz_sf_laea_indic1$species))  # 33 sp in FR


set_distances_tol_all <- c()

for (s in sp_indic_1){
  print(s)
  
  occs_all_2018_maiz_sf_laea_indic1_2 <- occs_all_2018_maiz_sf_laea_indic1[occs_all_2018_maiz_sf_laea_indic1$species %in% s, ]
  
  t0 <- Sys.time()
  closest_field <- apply(st_distance(occs_all_2018_maiz_sf_laea_indic1_2, maize_fr_2018), 1, min)
  print(Sys.time() - t0)
  
  head(closest_field)
  length(closest_field)
  
  
  cols2save <- c("species", "gbifID", "countryCode", "year")
  set_distances_tol <- as.data.frame(occs_all_2018_maiz_sf_laea_indic1_2)[, cols2save]
  
  set_distances_tol$closest_field <- round(closest_field, 0)
  
  
  #maize_share <- st_intersection(croparea_maize_fr_1k_2018, occs_all_2018_maiz_sf_laea_indic1_2)
  t0 <- Sys.time()
  maize_share <- st_intersection(croparea_maize_fr_1k_2018, occs_all_2018_maiz_sf_laea_indic1_2)
  print(Sys.time() - t0)
  
  
  maize_share <- as.data.frame(maize_share)[, c("gbifID", "area")]
  maize_share$area <- round(maize_share$area / 1000000, 3)  # this is in share (0-1)
  
  set_distances_tol <- merge(set_distances_tol, maize_share, all.x = TRUE, by = "gbifID")
  set_distances_tol[is.na(set_distances_tol$area), "area"] <- 0
  
  
  names(set_distances_tol) <- gsub("area", "maize_share", names(set_distances_tol))
  set_distances_tol
  
  
  set_distances_tol_all <- rbind(set_distances_tol_all, set_distances_tol)
  write.csv(set_distances_tol_all, paste0("distance2maize_LPIS_FR_all", ".csv"), row.names = FALSE)
  gc()
  
}

#set_distances_tol_all <- read.csv(paste0("distance2maize_LPIS_FR_all", ".csv"), header = TRUE)
set_distances_tol_all
length(unique(set_distances_tol_all$species))
unique(set_distances_tol_all$species)
table(set_distances_tol_all$species)


set_distances_tol <- set_distances_tol_all[set_distances_tol_all$closest_field <= 1000, ]
nrow(set_distances_tol_all)
nrow(set_distances_tol)


summary(set_distances_tol$closest_field)
sd(set_distances_tol$closest_field, na.rm = TRUE)
  
summary(set_distances_tol$maize_share)
sd(set_distances_tol$maize_share, na.rm = TRUE)
  
  


## some checks
#sort(unique(dist2maize$countryCode))
#sort(unique(dist2maize$species))
#sort(unique(dist2maize[countryCode == "FR", ]$species))
#(unique(dist2maize[countryCode == "FR", ]$species))
#
#dist2maize[species == "Galium tricornutum" & countryCode == "FR", ]
#set_distances_tol[set_distances_tol$species == "Galium tricornutum", ]
#
#
#occs_all_2018_maiz_sf_laea2_kk <- occs_all_2018_maiz_sf_laea2[occs_all_2018_maiz_sf_laea2$species == "Galium tricornutum", ]
#st_write(occs_all_2018_maiz_sf_laea2_kk, paste0("occs_all_2018_maiz_sf_laea2_kk", ".shp"), delete_layer = TRUE) 


## Plots

head(set_distances_tol_all)
set_distances_tol_all$species <- factor(set_distances_tol_all$species, sps)
set_distances_tol$species <- factor(set_distances_tol$species, sps)


jpeg(paste0("distance2maize_", "all_LPIS_FR", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
#jpeg(paste0("distance2maize_", "all_below1km_LPIS_FR", ".jpg"), width = 31, height = 24, units = "cm", res = 300)

par(mfrow = c(2, 2), mar = c(2, 5, 2, 2))

boxplot(set_distances_tol_all$closest_field ~ set_distances_tol_all$species,
#boxplot(set_distances_tol$closest_field ~ set_distances_tol$species,
        #ylim = c(0, 1000),
        ylim = c(0, 25000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
legend("center", 
       #legend = c("Indicator species","Not Indicator sps."), 
       legend = paste0(1:length(sps), "-", levels(dist2maize_1$species)), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch=20 , pt.cex = 3, cex = 0.8, 
       horiz = FALSE)#, 
#inset = - 1)


#boxplot(set_distances_tol$maize_share  ~ set_distances_tol$species, 
boxplot(set_distances_tol_all$maize_share  ~ set_distances_tol_all$species, 
        ylim = c(0, 0.2),
        #ylim = c(0, 0.1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#boxplot(set_distances_tol$cropmap2018_ArableLand_1km ~ set_distances_tol$species, 
#boxplot(set_distances_tol_all$cropmap2018_ArableLand_1km ~ set_distances_tol_all$species, 
#        ylim = c(0, 1),
#        col = colrs,
#        main = "",
#        xlab = "", 
#        ylab = "Share of arable land (scale: 1km)",
#        xaxt = 'n', 
#        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
#        ann = TRUE)
#mtext("Indicator Species                      Non-indicator Species", side = 3)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#par(xpd = TRUE)

dev.off()



## Some results for reporting
set_distances_tol_all <- data.table(set_distances_tol_all)
set_distances_tol <- data.table(set_distances_tol)

set_distances_tol_all_indicators <- set_distances_tol_all[!species %in% sp_NoIndic, ]
unique(set_distances_tol_all_indicators$species)

set_distances_tol_all_no_indicators <- set_distances_tol_all[species %in% sp_NoIndic, ]
unique(set_distances_tol_all_no_indicators$species)


names(set_distances_tol_all_indicators)

sum(is.na(set_distances_tol_all_indicators$set_distances_tol_all))
length(set_distances_tol_all_indicators$closest_field)


set_distances_tol_indicators <- set_distances_tol[!species %in% sp_NoIndic, ]
unique(set_distances_tol_indicators$species)

set_distances_tol_no_indicators <- set_distances_tol[species %in% sp_NoIndic, ]
unique(set_distances_tol_no_indicators$species)



# All occs; indicators
round(mean(set_distances_tol_all_indicators$closest_field, na.rm = TRUE), 0)   # 12320
round(sd(set_distances_tol_all_indicators$closest_field, na.rm = TRUE), 0)     # 13184
round(mean(set_distances_tol_all_indicators$maize_share, na.rm = TRUE), 3)    # 0.001
round(sd(set_distances_tol_all_indicators$maize_share, na.rm = TRUE), 3)      # 0.003

# All occs; non-indicators
round(mean(set_distances_tol_all_no_indicators$closest_field, na.rm = TRUE), 0)  # 4928
round(sd(set_distances_tol_all_no_indicators$closest_field, na.rm = TRUE), 0)    # 12380
round(mean(set_distances_tol_all_no_indicators$maize_share, na.rm = TRUE), 3)   # 0.029
round(sd(set_distances_tol_all_no_indicators$maize_share, na.rm = TRUE), 3)     # 0.07


# Occs below 1000m; indicators
nrow(set_distances_tol_indicators)            # only 2 occs 
unique(set_distances_tol_indicators$species)  # of 2 sps
round(mean(set_distances_tol_indicators$closest_field, na.rm = TRUE), 0)   # 295
round(sd(set_distances_tol_indicators$closest_field, na.rm = TRUE), 0)     # 174
round(mean(set_distances_tol_indicators$maize_share, na.rm = TRUE), 3)    #  0.006
round(sd(set_distances_tol_indicators$maize_share, na.rm = TRUE), 3)      #  0.004

# All occs; non-indicators
round(mean(set_distances_tol_no_indicators$closest_field, na.rm = TRUE), 0)  # 398
round(sd(set_distances_tol_no_indicators$closest_field, na.rm = TRUE), 0)    # 282
round(mean(set_distances_tol_no_indicators$maize_share, na.rm = TRUE), 3)   # 0.063
round(sd(set_distances_tol_no_indicators$maize_share, na.rm = TRUE), 3)     # 0.092







## Validation with LPIS for FR - 2019 - 2020 ####
# LPIS 
library(RPostgreSQL)
library(sf)

drv <- dbDriver("PostgreSQL")
connec <- dbConnect(drv, 
                    dbname = "refocus_gsaa_db",
                    host = "jeodb01.cidsn.jrc.it", 
                    port = 54321,
                    user = "refocus_gsaa_user_ro", 
                    password = "K2Xo5RcDPQ")

## LPIS. Maize fields

maize_fr_2019 <- st_read(connec, query = "SELECT * FROM cropdiversity.maize_fr_2019")
maize_fr_2019

maize_fr_2020 <- st_read(connec, query = "SELECT * FROM cropdiversity.maize_fr_2020")
maize_fr_2020


## LPIS. Maize aggregated 1km

croparea_maize_fr_1k_2019 <- st_read(connec, query = "SELECT * FROM cropdiversity.croparea_maize_fr_1k_2019")
croparea_maize_fr_1k_2019
range(croparea_maize_fr_1k_2019$area)   # area is maize share in ‰ (per mile; to be divided by 10^4 to get %) 
summary(croparea_maize_fr_1k_2019$area) / 10000 

croparea_maize_fr_1k_2020 <- st_read(connec, query = "SELECT * FROM cropdiversity.croparea_maize_fr_1k_2020")
croparea_maize_fr_1k_2020
range(croparea_maize_fr_1k_2020$area)   # area is maize share in ‰ (per mile; to be divided by 10^4 to get %) 
summary(croparea_maize_fr_1k_2020$area) / 10000 



### Distance to maize field and tolerance to maize share - 2019 ####


occs_all_2019 <- occs_all[occs_all$year == 2019, ]

occs_2019_specie <- unique(occs_all_2019$species)
length(occs_2019_specie)


weeds_maiz_gbib <- sort(weeds_maize$Species[weeds_maize$Species %in% occs_2019_specie])
length(weeds_maiz_gbib)


occs_all_2019_maiz <- occs_all_2019[occs_all_2019$species %in% weeds_maiz_gbib, ]


setnames(occs_all_2019_maiz, c("decimalLongitude", "decimalLatitude"), c("x", "y"))

occs_all_2019_maiz <- occs_all_2019_maiz[, .SD, .SDcols = c("species", "x", "y", "gbifID", "countryCode", "year")]
occs_all_2019_maiz


# How many occs in FR?
occs_all_2019_maiz_fr <- occs_all_2019_maiz[occs_all_2019_maiz$countryCode == "FR", ]
occs_all_2019_maiz_fr

occs_00_indicators <- fread("sp_indicators.csv", header = TRUE)
occs_00_indicators

occs_all_2019_maiz_fr_indic <- occs_all_2019_maiz_fr[species %in% occs_00_indicators$Var1, ]
table(occs_all_2019_maiz_fr_indic$species)



## Distance to maize field and tolerance to maize share

load("sp_indic_1.RData", verbose = TRUE)
sp_indic_1


occs_all_2019_maiz_sf <- st_as_sf(as.data.frame(occs_all_2019_maiz), coords = c("x", "y"), crs = 4326)#, agr = "constant")
occs_all_2019_maiz_sf

wkt <- sf::st_crs(3035)[[2]]

occs_all_2019_maiz_sf_laea <- st_transform(occs_all_2019_maiz_sf, crs = sp::CRS(wkt))
occs_all_2019_maiz_sf_laea


occs_all_2019_maiz_sf_laea2 <- st_transform(occs_all_2019_maiz_sf_laea, crs = st_crs(maize_fr_2019))
occs_all_2019_maiz_sf_laea2


occs_all_2019_maiz_sf_laea_indic1 <- occs_all_2019_maiz_sf_laea2[occs_all_2019_maiz_sf_laea2$species %in% sp_indic_1, ]
occs_all_2019_maiz_sf_laea_indic1 <- occs_all_2019_maiz_sf_laea_indic1[occs_all_2019_maiz_sf_laea_indic1$countryCode == "FR", ]
occs_all_2019_maiz_sf_laea_indic1
length(unique(occs_all_2019_maiz_sf_laea_indic1$species))  # 33 sp in FR



set_distances_tol_all_2019 <- c()

for (s in sp_indic_1){
  print(s)
  
  occs_all_2019_maiz_sf_laea_indic1_2 <- occs_all_2019_maiz_sf_laea_indic1[occs_all_2019_maiz_sf_laea_indic1$species %in% s, ]
  
  t0 <- Sys.time()
  closest_field <- apply(st_distance(occs_all_2019_maiz_sf_laea_indic1_2, maize_fr_2019), 1, min)
  print(Sys.time() - t0)
  
  head(closest_field)
  length(closest_field)
  
  
  cols2save <- c("species", "gbifID", "countryCode", "year")
  set_distances_tol <- as.data.frame(occs_all_2019_maiz_sf_laea_indic1_2)[, cols2save]
  
  set_distances_tol$closest_field <- round(closest_field, 0)
  
  
  t0 <- Sys.time()
  maize_share <- st_intersection(croparea_maize_fr_1k_2019, occs_all_2019_maiz_sf_laea_indic1_2)
  print(Sys.time() - t0)
  
  
  maize_share <- as.data.frame(maize_share)[, c("gbifID", "area")]
  maize_share$area <- round(maize_share$area / 1000000, 3)  # this is in share (0-1)
  
  set_distances_tol <- merge(set_distances_tol, maize_share, all.x = TRUE, by = "gbifID")
  set_distances_tol[is.na(set_distances_tol$area), "area"] <- 0
  
  
  names(set_distances_tol) <- gsub("area", "maize_share", names(set_distances_tol))
  set_distances_tol
  
  
  set_distances_tol_all_2019 <- rbind(set_distances_tol_all_2019, set_distances_tol)
  write.csv(set_distances_tol_all_2019, paste0("distance2maize_LPIS_FR_all_2019", ".csv"), row.names = FALSE)
  gc()
  
}


#set_distances_tol_all_2019 <- read.csv(paste0("distance2maize_LPIS_FR_all_2019", ".csv"), header = TRUE)
set_distances_tol_all_2019
length(unique(set_distances_tol_all_2019$species))
unique(set_distances_tol_all_2019$species)
table(set_distances_tol_all_2019$species)


set_distances_tol_2019 <- set_distances_tol_all_2019[set_distances_tol_all_2019$closest_field <= 1000, ]
nrow(set_distances_tol_all_2019)
nrow(set_distances_tol_2019)


summary(set_distances_tol_2019$closest_field)
sd(set_distances_tol_2019$closest_field, na.rm = TRUE)

summary(set_distances_tol_2019$maize_share)
sd(set_distances_tol_2019$maize_share, na.rm = TRUE)



## Plots

head(set_distances_tol_all_2019)
set_distances_tol_all_2019$species <- factor(set_distances_tol_all_2019$species, sps)
set_distances_tol_2019$species <- factor(set_distances_tol_2019$species, sps)


#jpeg(paste0("distance2maize_", "all_LPIS_FR_2019", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
jpeg(paste0("distance2maize_", "all_below1km_LPIS_FR_2019", ".jpg"), width = 31, height = 24, units = "cm", res = 300)

par(mfrow = c(2, 2), mar = c(2, 5, 2, 2))

#boxplot(set_distances_tol_all_2019$closest_field ~ set_distances_tol_all_2019$species,
boxplot(set_distances_tol_2019$closest_field ~ set_distances_tol_2019$species,
        ylim = c(0, 1000),
        #ylim = c(0, 25000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
legend("center", 
       #legend = c("Indicator species","Not Indicator sps."), 
       legend = paste0(1:length(sps), "-", levels(dist2maize_1$species)), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch=20 , pt.cex = 3, cex = 0.8, 
       horiz = FALSE)#, 
#inset = - 1)


boxplot(set_distances_tol_2019$maize_share  ~ set_distances_tol_2019$species, 
        #boxplot(set_distances_tol_all_2019$maize_share  ~ set_distances_tol_all_2019$species, 
        ylim = c(0, 0.2),
        #ylim = c(0, 0.1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#boxplot(set_distances_tol_2019$cropmap2018_ArableLand_1km ~ set_distances_tol_2019$species, 
#boxplot(set_distances_tol_all_2019$cropmap2018_ArableLand_1km ~ set_distances_tol_all_2019$species, 
#        ylim = c(0, 1),
#        col = colrs,
#        main = "",
#        xlab = "", 
#        ylab = "Share of arable land (scale: 1km)",
#        xaxt = 'n', 
#        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
#        ann = TRUE)
#mtext("Indicator Species                      Non-indicator Species", side = 3)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#par(xpd = TRUE)

dev.off()



## Some results for reporting
set_distances_tol_all_2019 <- data.table(set_distances_tol_all_2019)
set_distances_tol_2019 <- data.table(set_distances_tol_2019)

set_distances_tol_all_2019_indicators <- set_distances_tol_all_2019[!species %in% sp_NoIndic, ]
unique(set_distances_tol_all_2019_indicators$species)

set_distances_tol_all_2019_no_indicators <- set_distances_tol_all_2019[species %in% sp_NoIndic, ]
unique(set_distances_tol_all_2019_no_indicators$species)


names(set_distances_tol_all_2019_indicators)

sum(is.na(set_distances_tol_all_2019_indicators$closest_field))
length(set_distances_tol_all_2019_indicators$closest_field)


set_distances_tol_2019_indicators <- set_distances_tol_2019[!species %in% sp_NoIndic, ]
unique(set_distances_tol_2019_indicators$species)

set_distances_tol_2019_no_indicators <- set_distances_tol_2019[species %in% sp_NoIndic, ]
unique(set_distances_tol_2019_no_indicators$species)



# All occs; indicators
nrow(set_distances_tol_all_2019_indicators)  # 33 occs
round(mean(set_distances_tol_all_2019_indicators$closest_field, na.rm = TRUE), 0)   # 6781
round(sd(set_distances_tol_all_2019_indicators$closest_field, na.rm = TRUE), 0)     # 9945
round(mean(set_distances_tol_all_2019_indicators$maize_share, na.rm = TRUE), 3)    # 0.014
round(sd(set_distances_tol_all_2019_indicators$maize_share, na.rm = TRUE), 3)      # 0.037

# All occs; non-indicators
round(mean(set_distances_tol_all_2019_no_indicators$closest_field, na.rm = TRUE), 0)  # 4623
round(sd(set_distances_tol_all_2019_no_indicators$closest_field, na.rm = TRUE), 0)    # 11798
round(mean(set_distances_tol_all_2019_no_indicators$maize_share, na.rm = TRUE), 3)   # 0.032
round(sd(set_distances_tol_all_2019_no_indicators$maize_share, na.rm = TRUE), 3)     # 0.075


# Occs below 1000m; indicators
nrow(set_distances_tol_2019_indicators)            #  12 occs 
unique(set_distances_tol_2019_indicators$species)  #  of 5 sps
round(mean(set_distances_tol_2019_indicators$closest_field, na.rm = TRUE), 0)     # 364
round(sd(set_distances_tol_2019_indicators$closest_field, na.rm = TRUE), 0)       # 349
round(mean(set_distances_tol_2019_indicators$maize_share, na.rm = TRUE), 3)       # 0.038
round(sd(set_distances_tol_2019_indicators$maize_share, na.rm = TRUE), 3)         # 0.054

# All occs; non-indicators
round(mean(set_distances_tol_2019_no_indicators$closest_field, na.rm = TRUE), 0)  # 393
round(sd(set_distances_tol_2019_no_indicators$closest_field, na.rm = TRUE), 0)    # 280
round(mean(set_distances_tol_2019_no_indicators$maize_share, na.rm = TRUE), 3)    # 0.067
round(sd(set_distances_tol_2019_no_indicators$maize_share, na.rm = TRUE), 3)      # 0.096





### Distance to maize field and tolerance to maize share - 2020 ####


occs_all_2020 <- occs_all[occs_all$year == 2020, ]

occs_2020_specie <- unique(occs_all_2020$species)
length(occs_2020_specie)


weeds_maiz_gbib <- sort(weeds_maize$Species[weeds_maize$Species %in% occs_2020_specie])
length(weeds_maiz_gbib)


occs_all_2020_maiz <- occs_all_2020[occs_all_2020$species %in% weeds_maiz_gbib, ]


setnames(occs_all_2020_maiz, c("decimalLongitude", "decimalLatitude"), c("x", "y"))

occs_all_2020_maiz <- occs_all_2020_maiz[, .SD, .SDcols = c("species", "x", "y", "gbifID", "countryCode", "year")]
occs_all_2020_maiz


# How many occs in FR?
occs_all_2020_maiz_fr <- occs_all_2020_maiz[occs_all_2020_maiz$countryCode == "FR", ]
occs_all_2020_maiz_fr

occs_00_indicators <- fread("sp_indicators.csv", header = TRUE)
occs_00_indicators

occs_all_2020_maiz_fr_indic <- occs_all_2020_maiz_fr[species %in% occs_00_indicators$Var1, ]
table(occs_all_2020_maiz_fr_indic$species)



## Distance to maize field and tolerance to maize share

load("sp_indic_1.RData", verbose = TRUE)
sp_indic_1

occs_all_2020_maiz_sf <- st_as_sf(as.data.frame(occs_all_2020_maiz), coords = c("x", "y"), crs = 4326)#, agr = "constant")
occs_all_2020_maiz_sf

wkt <- sf::st_crs(3035)[[2]]

occs_all_2020_maiz_sf_laea <- st_transform(occs_all_2020_maiz_sf, crs = sp::CRS(wkt))
occs_all_2020_maiz_sf_laea


occs_all_2020_maiz_sf_laea2 <- st_transform(occs_all_2020_maiz_sf_laea, crs = st_crs(maize_fr_2020))
occs_all_2020_maiz_sf_laea2


occs_all_2020_maiz_sf_laea_indic1 <- occs_all_2020_maiz_sf_laea2[occs_all_2020_maiz_sf_laea2$species %in% sp_indic_1, ]
occs_all_2020_maiz_sf_laea_indic1 <- occs_all_2020_maiz_sf_laea_indic1[occs_all_2020_maiz_sf_laea_indic1$countryCode == "FR", ]
occs_all_2020_maiz_sf_laea_indic1
length(unique(occs_all_2020_maiz_sf_laea_indic1$species))  # 33 sp in FR



set_distances_tol_all_2020 <- c()

for (s in sp_indic_1){
  print(s)
  
  occs_all_2020_maiz_sf_laea_indic1_2 <- occs_all_2020_maiz_sf_laea_indic1[occs_all_2020_maiz_sf_laea_indic1$species %in% s, ]
  
  t0 <- Sys.time()
  closest_field <- apply(st_distance(occs_all_2020_maiz_sf_laea_indic1_2, maize_fr_2020), 1, min)
  print(Sys.time() - t0)
  
  head(closest_field)
  length(closest_field)
  
  
  cols2save <- c("species", "gbifID", "countryCode", "year")
  set_distances_tol <- as.data.frame(occs_all_2020_maiz_sf_laea_indic1_2)[, cols2save]
  
  set_distances_tol$closest_field <- round(closest_field, 0)
  
  
  t0 <- Sys.time()
  maize_share <- st_intersection(croparea_maize_fr_1k_2020, occs_all_2020_maiz_sf_laea_indic1_2)
  print(Sys.time() - t0)
  
  
  maize_share <- as.data.frame(maize_share)[, c("gbifID", "area")]
  maize_share$area <- round(maize_share$area / 1000000, 3)  # this is in share (0-1)
  
  set_distances_tol <- merge(set_distances_tol, maize_share, all.x = TRUE, by = "gbifID")
  set_distances_tol[is.na(set_distances_tol$area), "area"] <- 0
  
  
  names(set_distances_tol) <- gsub("area", "maize_share", names(set_distances_tol))
  set_distances_tol
  
  
  set_distances_tol_all_2020 <- rbind(set_distances_tol_all_2020, set_distances_tol)
  write.csv(set_distances_tol_all_2020, paste0("distance2maize_LPIS_FR_all_2020", ".csv"), row.names = FALSE)
  gc()
  
}


#set_distances_tol_all_2020 <- read.csv(paste0("distance2maize_LPIS_FR_all_2020", ".csv"), header = TRUE)
set_distances_tol_all_2020
length(unique(set_distances_tol_all_2020$species))
unique(set_distances_tol_all_2020$species)
table(set_distances_tol_all_2020$species)


set_distances_tol_2020 <- set_distances_tol_all_2020[set_distances_tol_all_2020$closest_field <= 1000, ]
nrow(set_distances_tol_all_2020)
nrow(set_distances_tol_2020)


summary(set_distances_tol_2020$closest_field)
sd(set_distances_tol_2020$closest_field, na.rm = TRUE)

summary(set_distances_tol_2020$maize_share)
sd(set_distances_tol_2020$maize_share, na.rm = TRUE)



## Plots

head(set_distances_tol_all_2020)
set_distances_tol_all_2020$species <- factor(set_distances_tol_all_2020$species, sps)
set_distances_tol_2020$species <- factor(set_distances_tol_2020$species, sps)


#jpeg(paste0("distance2maize_", "all_LPIS_FR_2020", ".jpg"), width = 31, height = 24, units = "cm", res = 300)
jpeg(paste0("distance2maize_", "all_below1km_LPIS_FR_2020", ".jpg"), width = 31, height = 24, units = "cm", res = 300)

par(mfrow = c(2, 2), mar = c(2, 5, 2, 2))

#boxplot(set_distances_tol_all_2020$closest_field ~ set_distances_tol_all_2020$species,
boxplot(set_distances_tol_2020$closest_field ~ set_distances_tol_2020$species,
        ylim = c(0, 1000),
        #ylim = c(0, 25000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
legend("center", 
       #legend = c("Indicator species","Not Indicator sps."), 
       legend = paste0(1:length(sps), "-", levels(dist2maize_1$species)), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch=20 , pt.cex = 3, cex = 0.8, 
       horiz = FALSE)#, 
#inset = - 1)


boxplot(set_distances_tol_2020$maize_share  ~ set_distances_tol_2020$species, 
        #boxplot(set_distances_tol_all_2020$maize_share  ~ set_distances_tol_all_2020$species, 
        ylim = c(0, 0.2),
        #ylim = c(0, 0.1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#boxplot(set_distances_tol_2020$cropmap2018_ArableLand_1km ~ set_distances_tol_2020$species, 
#boxplot(set_distances_tol_all_2020$cropmap2018_ArableLand_1km ~ set_distances_tol_all_2020$species, 
#        ylim = c(0, 1),
#        col = colrs,
#        main = "",
#        xlab = "", 
#        ylab = "Share of arable land (scale: 1km)",
#        xaxt = 'n', 
#        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
#        ann = TRUE)
#mtext("Indicator Species                      Non-indicator Species", side = 3)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)


#par(xpd = TRUE)

dev.off()



## Some results for reporting
set_distances_tol_all_2020 <- data.table(set_distances_tol_all_2020)
set_distances_tol_2020 <- data.table(set_distances_tol_2020)

set_distances_tol_all_2020_indicators <- set_distances_tol_all_2020[!species %in% sp_NoIndic, ]
unique(set_distances_tol_all_2020_indicators$species)

set_distances_tol_all_2020_no_indicators <- set_distances_tol_all_2020[species %in% sp_NoIndic, ]
unique(set_distances_tol_all_2020_no_indicators$species)


names(set_distances_tol_all_2020_indicators)

sum(is.na(set_distances_tol_all_2020_indicators$closest_field))
length(set_distances_tol_all_2020_indicators$closest_field)


set_distances_tol_2020_indicators <- set_distances_tol_2020[!species %in% sp_NoIndic, ]
unique(set_distances_tol_2020_indicators$species)

set_distances_tol_2020_no_indicators <- set_distances_tol_2020[species %in% sp_NoIndic, ]
unique(set_distances_tol_2020_no_indicators$species)



# All occs; indicators
nrow(set_distances_tol_all_2020_indicators)  #  occs
round(mean(set_distances_tol_all_2020_indicators$closest_field, na.rm = TRUE), 0)   # 
round(sd(set_distances_tol_all_2020_indicators$closest_field, na.rm = TRUE), 0)     # 
round(mean(set_distances_tol_all_2020_indicators$maize_share, na.rm = TRUE), 3)    # 
round(sd(set_distances_tol_all_2020_indicators$maize_share, na.rm = TRUE), 3)      # 

# All occs; non-indicators
round(mean(set_distances_tol_all_2020_no_indicators$closest_field, na.rm = TRUE), 0)  # 
round(sd(set_distances_tol_all_2020_no_indicators$closest_field, na.rm = TRUE), 0)    # 
round(mean(set_distances_tol_all_2020_no_indicators$maize_share, na.rm = TRUE), 3)   # 
round(sd(set_distances_tol_all_2020_no_indicators$maize_share, na.rm = TRUE), 3)     # 


# Occs below 1000m; indicators
nrow(set_distances_tol_2020_indicators)            #   occs 
unique(set_distances_tol_2020_indicators$species)  #  of  sps
round(mean(set_distances_tol_2020_indicators$closest_field, na.rm = TRUE), 0)     # 
round(sd(set_distances_tol_2020_indicators$closest_field, na.rm = TRUE), 0)       # 
round(mean(set_distances_tol_2020_indicators$maize_share, na.rm = TRUE), 3)       # 
round(sd(set_distances_tol_2020_indicators$maize_share, na.rm = TRUE), 3)         # 

# All occs; non-indicators
round(mean(set_distances_tol_2020_no_indicators$closest_field, na.rm = TRUE), 0)  # 
round(sd(set_distances_tol_2020_no_indicators$closest_field, na.rm = TRUE), 0)    # 
round(mean(set_distances_tol_2020_no_indicators$maize_share, na.rm = TRUE), 3)    # 
round(sd(set_distances_tol_2020_no_indicators$maize_share, na.rm = TRUE), 3)      # 



### Reporting together ####

set_distances_tol_all <- read.csv(paste0("distance2maize_LPIS_FR_all", ".csv"), header = TRUE)
set_distances_tol_all_2019 <- read.csv(paste0("distance2maize_LPIS_FR_all_2019", ".csv"), header = TRUE)
set_distances_tol_all_2020 <- read.csv(paste0("distance2maize_LPIS_FR_all_2020", ".csv"), header = TRUE)



# Table

head(set_distances_tol_all)
set_distances_tol_all %>% group_by(species) %>% 
  summarize(closest_field = mean(closest_field, na.rm=TRUE), maize_share = mean(maize_share, na.rm = TRUE), n = n())


load("sp_indic_1.RData", verbose = TRUE)
sp_indic_1

sp_indic_1_df <- as.data.frame(sp_indic_1)
sp_indic_1_df$indicator <- c(rep("indicator", 25), rep("non-indicator", 25))
sp_indic_1_df





set_distances_tol_all_2018_2020 <- rbind(set_distances_tol_all, set_distances_tol_all_2019, set_distances_tol_all_2020)
head(set_distances_tol_all_2018_2020)

set_distances_tol_all_2018_2020 <- merge(set_distances_tol_all_2018_2020, sp_indic_1_df, all.x = TRUE, by.x = "species", by.y = "sp_indic_1")
head(set_distances_tol_all_2018_2020)



set_distances_tolerance_all_2018_2020 <- set_distances_tol_all_2018_2020 %>% group_by(year, indicator) %>% 
  summarize(N = n(),
            closest_field_mean = round(mean(closest_field, na.rm = TRUE), 0), 
            maize_share_mean = round(mean(maize_share, na.rm = TRUE), 3),
            closest_field_sd = round(sd(closest_field, na.rm = TRUE), 0),
            maize_share_sd = round(sd(maize_share, na.rm = TRUE), 3)
            )
set_distances_tolerance_all_2018_2020 <- as.data.frame(set_distances_tolerance_all_2018_2020)

write.csv(set_distances_tolerance_all_2018_2020, "set_distances_tolerance_all_2018_2020.csv", row.names = FALSE)



# Plot

set_distances_tol_all$species <- factor(set_distances_tol_all$species, sps)
#set_distances_tol$species <- factor(set_distances_tol$species, sps)
set_distances_tol_all_2019$species <- factor(set_distances_tol_all_2019$species, sps)
#set_distances_tol_2019$species <- factor(set_distances_tol_2019$species, sps)
set_distances_tol_all_2020$species <- factor(set_distances_tol_all_2020$species, sps)
#set_distances_tol_2020$species <- factor(set_distances_tol_2020$species, sps)

load("colrs.RData", verbose = TRUE)
length(colrs)

load("sp_indic_1.RData", verbose = TRUE)
sp_indic_1
sps <- sp_indic_1



jpeg(paste0("distance2maize_", "all_LPIS_FR_2018_20", ".jpg"), width = 40, height = 24, units = "cm", res = 300)
#jpeg(paste0("distance2maize_", "all_below1km_LPIS_FR_2018_20", ".jpg"), width = 31, height = 24, units = "cm", res = 300)

par(mfcol = c(3, 3), mar = c(2, 5, 3, 2))

# 2018
boxplot(set_distances_tol_all$closest_field ~ set_distances_tol_all$species,
#boxplot(set_distances_tol$closest_field ~ set_distances_tol$species,
        #ylim = c(0, 1000),
        ylim = c(0, 25000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)
mtext("2018", side = 3, line = 1)



#boxplot(set_distances_tol$maize_share  ~ set_distances_tol$species, 
boxplot(set_distances_tol_all$maize_share  ~ set_distances_tol_all$species, 
        ylim = c(0, 0.2),
        #ylim = c(0, 0.1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)

plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)


# 2019
boxplot(set_distances_tol_all_2019$closest_field ~ set_distances_tol_all_2019$species,
#boxplot(set_distances_tol_2019$closest_field ~ set_distances_tol_2019$species,
        #ylim = c(0, 1000),
        ylim = c(0, 25000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("2019", side = 3, line = 1)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)


#boxplot(set_distances_tol_2019$maize_share  ~ set_distances_tol_2019$species, 
boxplot(set_distances_tol_all_2019$maize_share  ~ set_distances_tol_all_2019$species, 
        ylim = c(0, 0.2),
        #ylim = c(0, 0.1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)

mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)

plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
#par(xpd=TRUE)
legend("center", 
       #legend = c("Indicator species","Not Indicator sps."), 
       legend = paste0(1:length(sps), "-", sps), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch=20 , pt.cex = 1.5, 
       cex = 0.9, 
       horiz = FALSE#, 
       #inset = -0.55
       )
#par(xpd=FALSE)

# 2020
boxplot(set_distances_tol_all_2020$closest_field ~ set_distances_tol_all_2020$species,
#boxplot(set_distances_tol_2020$closest_field ~ set_distances_tol_2020$species,
        #ylim = c(0, 1000),
        ylim = c(0, 25000),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Distance to maize (m)",
        xaxt = 'n', 
        #cex.axis = 0.7,
        #at = c(1:25, 27:51),
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        #at = c(1:25, 27:36, 38:42),
        ann = TRUE)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
#axis(1, cex.axis = 0.7, at = c(1:25, 27:36, 38:42), labels = 1:length(sps), las = 2)
mtext("Indicator Species                      Non-indicator Species", side = 3)
#mtext("  Indicator Species                                  Sensitive       Tolerant", side = 3)
mtext("2020", side = 3, line = 1)


#boxplot(set_distances_tol_2020$maize_share  ~ set_distances_tol_2020$species, 
boxplot(set_distances_tol_all_2020$maize_share  ~ set_distances_tol_all_2020$species, 
        ylim = c(0, 0.2),
        #ylim = c(0, 0.1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (scale: 1km)",
        xaxt = 'n', 
        at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        ann = TRUE)
mtext("Indicator Species                      Non-indicator Species", side = 3)
axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)

plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)

#par(xpd = TRUE)

dev.off()



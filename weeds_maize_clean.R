library(raster)
library(sf)

#library(devtools)
#install_github("xavi-rp/PreSPickR", 
#               ref = "v2", 
#               INSTALL_opts = c("--no-multiarch"))  # https://github.com/rstudio/renv/issues/162
library(PreSPickR)

library(data.table)
library(ggplot2)
library(ggpubr)
library(viridis)
library(tidyverse)
require(gridExtra)




#sessionInfo()

if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", 
                              "jeodpp-terminal-jd002-03",
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

cropmap2018 <- raster("/mnt/cidstorage/cidportal/data/OpenData/EUCROPMAP/2018/EUCROPMAP_2018.tif")  # at 10m
cropmap2018

## CropMap classes
crops_categs <- c(100, 211, 212, 213, 214, 215, 216, 217, 218, 219, 221, 222, 223, 230, 231, 232, 233, 240, 250, 290, 300, 500, 600, 800)
crops_names <- c("Artificial", "Common wheat", "Durum wheat", "Barley", "Rye", "Oats", "Maize", "Rice", "Triticale", "Other cereals", "Potatoes", "Sugar beet", "Other root crops", "Other non permanent industrial crops", "Sunflower", "Rape and turnip rape", "Soya", "Dry pulses", "Fodder crops (cereals and leguminous)", "Bare arable land", "Woodland and Shrubland (incl. permanent crops)", "Grasslands", "Bare land", "Wetlands")

cropmap_classes <- data.frame("crop_categ" = crops_categs, "crop_names" = crops_names)
# Maiz: 216



### Aggregating maiz to 1km or 10km ####

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
                                  filename = "cropmap2018_maiz_1km.tif",
                                  overwrite = TRUE)

wkt <- sf::st_crs(3035)[[2]]
crs(cropmap2018_maiz_1km) <- sp::CRS(wkt)
writeRaster(cropmap2018_maiz_1km, "cropmap2018_maiz_1km.tif", overwrite = TRUE)

cropmap2018_maiz_1km <- raster("cropmap2018_maiz_1km.tif")



## 10km
aggr_fun_10km <- function(x, ...) {     # returns share of maize at 10km (0 to 1)
  if (all(is.na(x))){
    mz_share <- NA
  }else{
    mz_share <- sum(x == 216, na.rm = TRUE) / 10^6    # 10^6 10-m pixels in a 10-km pixel
  }
  return(mz_share)
}

cropmap2018_maiz_10km <- aggregate(x = cropmap2018, 
                                   fact = 1000,        # 10km
                                   fun = aggr_fun_10km,  # 10km
                                   expand = TRUE, 
                                   na.rm = TRUE, 
                                   filename = "cropmap2018_maiz_10km.tif",
                                   overwrite = TRUE)

cropmap2018_maiz_10km <- raster("cropmap2018_maiz_10km.tif")




### Aggregating Cropland and Noncropland to 1km or 10km ####

aggr_NonAL_1km <- function(x, ...) {     # returns share of Non-cropland at 1km (0 to 1)
  if (all(is.na(x))){
    nal_share <- NA
  }else{
    nal_share <- sum(x %in% c(500, 600, 800), na.rm = TRUE) / 10000
  }
  
  return(nal_share)
}


aggr_ArabLand_1km <- function(x, ...) {     # returns share of Cropland at 1km (0 to 1)
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


aggr_ArabLand_10km <- function(x, ...) {     # returns share of Cropand at 10km (0 to 1)
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
                                      fact = 100,        # 1km
                                      fun = aggr_ArabLand_1km, 
                                      expand = TRUE, 
                                      na.rm = TRUE, 
                                      filename = "cropmap2018_ArableLand_1km.tif",
                                      overwrite = TRUE)

wkt <- sf::st_crs(3035)[[2]]
crs(cropmap2018_arabland_1km) <- sp::CRS(wkt)


cropmap2018_nal_1km <- raster("cropmap2018_NonAL_1km.tif")
cropmap2018_arabland_1km <- raster("cropmap2018_ArableLand_1km.tif")


## Merging (rasters) maize share and arable land share
cropmap2018_maiz_1km$cropmap2018_arabland_1km <- getValues(cropmap2018_arabland_1km)
cropmap2018_maiz_1km

cropmap2018_arabland_10km <- aggregate(x = cropmap2018,
                                       fact = 1000,        # 10km
                                       fun = aggr_ArabLand_10km, 
                                       expand = TRUE, 
                                       na.rm = TRUE, 
                                       filename = "cropmap2018_ArableLand_10km.tif",
                                       #filename = "",
                                       overwrite = TRUE)

cropmap2018_arabland_10km <- raster("cropmap2018_ArableLand_10km.tif")



## EUCropMap against Agricultural Management Intensity Map (Rega et al., 2020) ####

Absolute_intensity_5_clas_Fig3A <- raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Fig3A-Absolute_intensity_5_clas_.tif/Absolute_intensity_5_clas_Fig3A.tif")
Absolute_intensity_5_clas_Fig3A

#Energy_input_2008_fille04_no_labour <- raster("/eos/jeodpp/data/projects/REFOCUS/data/BIODIVERSITY/Rega/Energy_input_raw data/Energy_input_2008_fille04_no_labour.tif")
#Energy_input_2008_fille04_no_labour


## maize and cropland share aggregated at 1km from the Crop Map
cropmap2018_maiz_1km

cropmap2018_maiz_1km_clean <- cropmap2018_maiz_1km[["cropmap2018_maiz_1km"]]
cropmap2018_arabland_1km_clean <- cropmap2018_maiz_1km[["cropmap2018_arabland_1km"]]


## Comparing maize share (crop Map) with Rega's map (absolute intensity)
cropmap2018_maiz_1km_clean_1 <- crop(cropmap2018_maiz_1km_clean, Absolute_intensity_5_clas_Fig3A)
extent(Absolute_intensity_5_clas_Fig3A) <- extent(cropmap2018_maiz_1km_clean_1)

cropmap2018_arabland_1km_clean_1 <- crop(cropmap2018_arabland_1km_clean, Absolute_intensity_5_clas_Fig3A)
extent(Absolute_intensity_5_clas_Fig3A) <- extent(cropmap2018_arabland_1km_clean_1)



## Plotting correlation (scatterplot)
comp_df <- data.table(data.frame(getValues(Absolute_intensity_5_clas_Fig3A), 
                                 getValues(cropmap2018_maiz_1km_clean_1),
                                 getValues(cropmap2018_arabland_1km_clean_1)))

comp_df <- comp_df[complete.cases(comp_df), 1:3]
comp_df

## Keeping only pixels with >= 20% of cropland... This reduces a lot the correlation!!
#summary(comp_df$cropmap2018_arabland_1km)
#comp_df <- comp_df[getValues.cropmap2018_arabland_1km_clean_1. >= 0.2, ]

# Keeping only pixels with >= 0.2% of maize share, to remove some noise produced by the CropMap
comp_df <- comp_df[getValues.cropmap2018_maiz_1km_clean_1. >= 0.002, ]

comp_df <- comp_df[, .SD, .SDcols = c("getValues.Absolute_intensity_5_clas_Fig3A.", "getValues.cropmap2018_maiz_1km_clean_1.")]

# Pearson's correlation coefficient
comp_df_pearson <- cor(comp_df, method = "pearson")[2, 1]
comp_df_pearson
comp_df_pearson^2  # if we fit a linear regression (see below), this is R^2 (R squared)


## Plotting boxplots, instead of scatterplot

png("comp_CropMapMaize_Rega3A_boxplotsLog_1.png",
    width = 25, height = 15, units = "cm", res = 150)

ggplot(comp_df, 
       aes(x = getValues.Absolute_intensity_5_clas_Fig3A. , y = getValues.cropmap2018_maiz_1km_clean_1., 
           group = getValues.Absolute_intensity_5_clas_Fig3A.)) + 
  geom_boxplot(color = "black", fill = "grey", alpha = 0.6) +
  coord_flip() +
  theme_bw() +
  labs(x = "Absolute Intensity", y = "log(Maize Share)") +
  scale_y_log10() +  # The horizontal boxplot is dominated by the outlier in MaizeShare 
  # A solution is to scale MaizeShare values the x-axis to log-scale
  #coord_trans(y="log10") # this only transform the axis
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)) + 
  annotate("text", x =  0.5, y = 0.5, size = 7,
           label = paste0("Pearson's r = ", as.character(round(comp_df_pearson, 3)))) 

dev.off()




## Maize weeds ####

weeds_maize <- read.csv("weeds_maize_report_2011.csv", header = TRUE)
head(weeds_maize)
nrow(weeds_maize)

## families of the weeds
fams <- c()
for(sp in weeds_maize$Species){
  fam1 <- as.data.frame(rgbif::name_backbone(name =  sp))$family
  fams <- c(fams, fam1)
}
unique(fams)
length(unique(fams))


## GBIF occurrences ####

## Downloading from GBIF

taxa <- weeds_maize$Species

GetBIF(credentials = paste0(gbif_creds, "/gbif_credentials.RData"),
       taxon_list = taxa,
       download_format = "SIMPLE_CSV",
       download_years = c(2000, 2020),
       download_coords = c(-12.69141, 42.71485, 33.4901, 71.9218), #order: xmin, xmax, ymin, ymax
       download_coords_accuracy = c(0, 50),
       rm_dupl = TRUE,
       cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                     "gbifID",
                     "coordinateUncertaintyInMeters",
                     "countryCode", "year", 
                     "datasetKey"),
       out_name = paste0("sp_records_", format(Sys.Date(), "%Y%m%d")))


taxon_dir <- getwd()
occs_all <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                     taxons = taxa,
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


occs_all <- fread(paste0(getwd(), "/../exploring_lucas_data/D5_FFGRCC_gbif_occ/sp_records_20210709.csv"), header = TRUE)
if(nchar(occs_all$sp2[1]) == 7) occs_all[, sp2 := gsub(" ", "_", occs_all$species)]
occs_all
cols_order <- c("species", "decimalLatitude", "decimalLongitude", "gbifID", "countryCode", "year", "sp2")
occs_all <- occs_all[, .SD, .SDcols = cols_order]
occs_all <- occs_all[occs_all$species != "", ]
occs_all_2018 <- occs_all[occs_all$year == 2018, ]  # 1591221 occs for 2018

occs_2018_specie <- unique(occs_all_2018$species)

sum(occs_2018_specie %in% weeds_maize$Species)   # 151 sp
sum(weeds_maize$Species %in% occs_2018_specie)   # 151 sp
sum(!weeds_maize$Species %in% occs_2018_specie)   # 53 sp (maize weeds) which we don't have in 2018


weeds_maiz_gbib <- sort(weeds_maize$Species[weeds_maize$Species %in% occs_2018_specie])
weeds_maiz_not_gbib <- sort(weeds_maize$Species[!weeds_maize$Species %in% occs_2018_specie])


occs_all_2018_maiz <- occs_all_2018[occs_all_2018$species %in% weeds_maiz_gbib, ]

nrow(occs_all_2018_maiz)   # 158427 occurrences for 2018
nrow(occs_all_2018)        # over 1591221 in total for 2018
occs_all_2018_maiz


setnames(occs_all_2018_maiz, c("decimalLongitude", "decimalLatitude"), c("x", "y"))

occs_all_2018_maiz <- occs_all_2018_maiz[, .SD, .SDcols = c("species", "x", "y", "gbifID", "countryCode", "year")]

occs_all_2018_maiz_sf <- st_as_sf(as.data.frame(occs_all_2018_maiz), coords = c("x", "y"), crs = 4326)#, agr = "constant")
occs_all_2018_maiz_sf

wkt <- sf::st_crs(3035)[[2]]
occs_all_2018_maiz_sf_laea <- st_transform(occs_all_2018_maiz_sf, crs = sp::CRS(wkt))
occs_all_2018_maiz_sf_laea

occs_maizeShare <- as.data.table(raster::extract(cropmap2018_maiz_1km, occs_all_2018_maiz_sf_laea, cellnumbers = TRUE))
occs_maizeShare  


# We keep cells with some maize and at least one of the weeds !!! 
occs_all_2018_maiz_sf_laea_dt <- as.data.table(occs_all_2018_maiz_sf_laea)

occs_maizeShare <- cbind(occs_all_2018_maiz_sf_laea_dt, occs_maizeShare)
occs_maizeShare

occs_maizeShare <- na.omit(occs_maizeShare)

setkeyv(occs_maizeShare, "cells")
occs_maizeShare

# Removing repeated occurrences (same sp) in the same cell, because we want to work with Species Richness
occs_maizeShare <- occs_maizeShare[!duplicated(occs_maizeShare[, c("species", "cells")]), ]


## We keep only cells with at least 20% of cropland because we want to be focused on 
#  agricultural areas. 

occs_maizeShare <- occs_maizeShare[cropmap2018_arabland_1km >= 0.2, ]

## We also remove those cells with less than 0.2% of maize share to avoid some noise produced by the CropMap
occs_maizeShare <- occs_maizeShare[cropmap2018_maiz_1km >= 0.002, ]
length(unique(occs_maizeShare$cells))  # cells with one or more weeds: 17557

## Species richness per cell
occs_maizeShare_aggr <- as.data.table(table(occs_maizeShare$cells))
occs_maizeShare_aggr <- occs_maizeShare_aggr[, lapply(.SD, as.numeric)]
table(occs_maizeShare_aggr$N)


occs_maizeShare_1 <- occs_maizeShare[, 6:8]
occs_maizeShare_1 <- unique(occs_maizeShare_1, by = "cells") 

occs_maizeShare_1 <- merge(occs_maizeShare_1, occs_maizeShare_aggr, by.x = "cells", by.y = "V1", all.x = TRUE)
occs_maizeShare_1

summary(occs_maizeShare_1$cropmap2018_maiz_1km)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0020  0.0318  0.0975  0.1255  0.1868  0.8798



## Assessing correlations ####

png("scatterplot_SpRichness_maize_1.png",  
    width = 16, height = 15, units = "cm", res = 150)

ggplot(occs_maizeShare_1, aes(cropmap2018_maiz_1km, N)) +
  geom_hex(bins = 50) +
  geom_smooth(method = "lm", col = "red") +
  stat_cor(method = "pearson", label.x = .50, label.y = 25) +
  labs(x = "Maize share", y = "Species richness") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

dev.off()



# sp richness against cropland (to demonstrate that the change in sp richness 
# is due to the share of maize, thus intensification, and not to the share of arable)

png("scatterplot_SpRichness_Arable_1.png",  
    width = 16, height = 15, units = "cm", res = 150)

ggplot(occs_maizeShare_1, aes(cropmap2018_arabland_1km, N)) +
  geom_hex(bins = 50) +
  geom_smooth(method = "lm", col = "red") +
  stat_cor(method = "pearson", label.x = .50, label.y = 25) +
  labs(x = "Cropland share", y = "Species richness") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

dev.off()




## Sp richness aggregated by maize share class 

occs_maizeShare_1[, maize_share_class := cut(occs_maizeShare_1$cropmap2018_maiz_1km,
                                             breaks = seq(0, 1, 0.1),
                                             include.lowest = TRUE,
                                             right = FALSE)]


png("boxplot_SpRichness_maize.png",
    width = 20, height = 10, units = "cm", res = 150)

ggplot(occs_maizeShare_1, aes(x = maize_share_class, y = N)) + 
  geom_boxplot() +
  labs(x = "Maize share", y = "Species richness") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

dev.off()





## Finding weeds potentially indicators of low intensification ####
## Grouping by 10 classes of maize share 
occs_maizeShare <- merge(occs_maizeShare, occs_maizeShare_1[, .SD, .SDcols = c("cells", "N", "maize_share_class")], by = "cells", all.x = TRUE)
fwrite(occs_maizeShare, file = "/eos/jeodpp/home/users/rotllxa/weeds/occs_maizeShare.csv", row.names = FALSE, quote = TRUE)
occs_maizeShare <- fread("/eos/jeodpp/home/users/rotllxa/weeds/occs_maizeShare.csv", header = TRUE)


sp_00 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0,0.1)", "species"]))))
sp_01 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.1,0.2)", "species"]))))
sp_02 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.2,0.3)", "species"]))))
sp_03 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.3,0.4)", "species"]))))
sp_04 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.4,0.5)", "species"]))))
sp_05 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.5,0.6)", "species"]))))
sp_06 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.6,0.7)", "species"]))))
sp_07 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.7,0.8)", "species"]))))
sp_08 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.8,0.9)", "species"]))))
sp_09 <- sort(as.vector(unlist(unique(occs_maizeShare[occs_maizeShare$maize_share_class == "[0.9,1]", "species"]))))   # no species here

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

occs_00 <- occs_maizeShare[occs_maizeShare$maize_share_class == "[0,0.1)", ]
length(unique(occs_00$species))

occs_00_indicators <- occs_00[occs_00$species %in% sp_00_notOthers, ]
occs_00_indicators
occs_00_indicators <- table(occs_00_indicators$species)
occs_00_indicators <- sort(occs_00_indicators, decreasing = TRUE)
occs_00_indicators
write.csv(occs_00_indicators, file = "sp_indicators.csv", row.names = FALSE)
occs_00_indicators <- fread("sp_indicators.csv", header = TRUE)


# For the other species in class 0, not "indicators". These appear in many other classes
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
#"Atriplex patula"   "Brassica nigra"    "Eclipta prostrata" "Rumex pulcher"   
#"Senecio vernalis"  "Silene noctiflora"  "Veronica agrestis"


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

grp_names <- as.vector(sort(unique(occs_maizeShare$maize_share_class)))
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



png("barplot_IndicatorSp.png",
    width = 22, height = 13, units = "cm", res = 150)
ggplot(data.frame(sps_groups_vctr, names(sps_groups_vctr)), 
       aes(x = names.sps_groups_vctr., y = sps_groups_vctr)) + 
  geom_col() +
  labs(x = "Maize share (pooled)", y = "Number of species") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

dev.off()


# Venn diagram
library(ggvenn)

png("VennDiagr_3_IndicatorSp.png",
    width = 20, height = 20, units = "cm", res = 150)

ggvenn(sps_groups_2, 
       #show_elements = TRUE)
       show_elements = FALSE,
       text_size = 7)

dev.off()


# Heatmap:
library(reshape2)
sps_groups_df <- melt(sps_groups)
names(sps_groups_df) <- c("Species", "Maize_share")
head(sps_groups_df)

library(forcats)
library(tidyverse)

jpeg("heatmap_IndicatorSp_2.jpg",
     units = "cm",
     res = 300,
     width = 15, height = 40)

sps_groups_df %>%
  mutate(Species = fct_reorder(Species, desc(Species))) %>% 
  ggplot(aes(Maize_share, Species)) + 
  scale_x_discrete(position = "top") +
  geom_tile(aes(fill = Maize_share), colour = "white", show.legend = FALSE) 

dev.off()




## Checking and mapping indicator species for timeseries ####

occs_00_indicators
occs_all

occs_all_indic_allYears <- occs_all[species %in% names(occs_00_indicators)]
occs_all_indic_allYears

occs2plot <- occs_all_indic_allYears %>% 
  group_by(species, year) %>%
  summarise(occurrences = n()) %>%
  data.table()

occs2plot
occs2plot$year <- as.factor(occs2plot$year)
occs2plot$species <- factor(occs2plot$species, levels = names(occs_00_indicators))
str(occs2plot)


jpeg("NumOccurrencesIndicAllYears.jpg", width = 20, height = 25, units = "cm", res = 150)

ggplot(occs2plot, 
       aes(x = year, y = occurrences, group = species)) + 
  geom_line() +
  geom_point() + 
  theme_bw() +
  facet_wrap(~ species, nrow = 4, ncol = 3) + 
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "none") +
  labs(x = "Year", y = "Number of occurrences")

dev.off()




occs_all_indic_allYears_country <- occs_all_indic_allYears %>% 
  group_by(species, year, countryCode) %>%
  summarise(occurrences = n()) %>%
  data.table()
occs_all_indic_allYears_country

occs_all_indic_country <- occs_all_indic_allYears %>% 
  group_by(species, countryCode) %>%
  summarise(occurrences = n()) %>%
  data.table()
occs_all_indic_country


occs_all_indic_country$species <- factor(occs_all_indic_country$species, levels = names(occs_00_indicators))

jpeg("NumOccurrencesIndicCountry.jpg", width = 25, height = 25, units = "cm", res = 150)

ggplot(occs_all_indic_country, 
       aes(x = countryCode, y = occurrences, group = 1)) + 
  geom_point() + 
  theme_minimal() +
  facet_wrap(~ species, nrow = 4, ncol = 3) + 
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "none") +
  labs(x = "Country", y = "Number of occurrences")
dev.off()




## Mapping against Agricultural land share

occs_all_indic_allYears
#cropmap2018_arabland_1km <- raster("cropmap2018_ArableLand_1km.tif")
#cropmap2018_arabland_10km <- raster("cropmap2018_ArableLand_10km.tif")

occs_all_indic_allYears_sf <- st_as_sf(as.data.frame(occs_all_indic_allYears), 
                                       coords = c("decimalLongitude", "decimalLatitude"), 
                                       crs = 4326)

occs_all_indic_allYears_sf <- st_transform(occs_all_indic_allYears_sf,
                                           crs = st_crs(cropmap2018_arabland_1km))


occs_all_indic_allYears_arable1km <- as.data.table(raster::extract(cropmap2018_arabland_1km,
                                                                   occs_all_indic_allYears_sf,
                                                                   sp = TRUE))

occs_all_indic_allYears_arable1km


## Arable land: 10 km
occs_all_indic_allYears_arable10km <- as.data.table(raster::extract(cropmap2018_arabland_10km,
                                                                    occs_all_indic_allYears_sf,
                                                                    sp = TRUE))

occs_all_indic_allYears_arable10km


data2plot_10km <- occs_all_indic_allYears_arable10km[cropmap2018_ArableLand_10km >= 0.2] %>% 
  group_by(year, species) %>%
  summarise(occurrences = n()) %>%
  data.table()

data2plot_10km


ggplot(data2plot_10km[year < 2021], 
       aes(x = year, y = occurrences, group = species)) + 
  #geom_bar() 
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_wrap(~ species, nrow = 4, ncol = 3) + 
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "none") +
  labs(x = "Year", y = "Number of occurrences in Arable Land (10km)")


data2plot_10km_1 <- occs_all_indic_allYears_arable10km %>% 
  filter(year < 2021) %>%  
  group_by(year) %>%
  summarise(Occurrences_Cropland = sum(cropmap2018_ArableLand_10km >= 0.2, na.rm = TRUE), 
            Occurrences_NonCropland = sum(cropmap2018_ArableLand_10km < 0.2, na.rm = TRUE)) %>%
  pivot_longer(!year, names_to = "Occurrences") %>%
  data.table()

data2plot_10km_1


#plot1 <- ggplot(data2plot_10km_1, 
#       aes(x = year, y = value, color = Occurrences)) + 
#  geom_point() + 
#  stat_smooth(method = 'lm', se = FALSE) +
#  theme_bw() +
#  scale_color_viridis(option = "viridis", discrete = TRUE, end = 0.7) +
#  theme(strip.text = element_text(face = "italic"),
#        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
#  labs(x = "Year", y = "log(Number of occurrences)")
#
#plot2 <- ggplot(data2plot_10km_1, 
#       aes(x = year, y = value, color = Occurrences)) + 
#  geom_point() + 
#  scale_y_log10() +
#  stat_smooth(method = 'lm', se = FALSE) +
#  theme_bw() +
#  scale_color_viridis(option = "viridis", discrete = TRUE, end = 0.7) +
#  theme(strip.text = element_text(face = "italic"),
#        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
#  labs(x = "Year", y = "log(Number of occurrences)")
#
#
#grid.arrange(plot1, plot2, ncol=2)


#


## For the non-indicator species (25 selected for the previous analysis)
load("sp_indic_1.RData", verbose = TRUE)
sp_indic_1
occs_00_indicators
occs_all

sps_nonIndic <- sp_indic_1[!sp_indic_1 %in% names(occs_00_indicators)]

occs_all_nonIndic_allYears <- occs_all[species %in% sps_nonIndic]

occs_all_nonIndic_allYears_sf <- st_as_sf(as.data.frame(occs_all_nonIndic_allYears), 
                                          coords = c("decimalLongitude", "decimalLatitude"), 
                                          crs = 4326)

occs_all_nonIndic_allYears_sf <- st_transform(occs_all_nonIndic_allYears_sf,
                                              crs = st_crs(cropmap2018_arabland_1km))

occs_all_nonIndic_allYears_arable10km <- as.data.table(raster::extract(cropmap2018_arabland_10km,
                                                                       occs_all_nonIndic_allYears_sf,
                                                                       sp = TRUE))

occs_all_nonIndic_allYears_arable10km

data2plot_10km_1_nonIndic <- occs_all_nonIndic_allYears_arable10km %>% 
  filter(year < 2021) %>%  
  group_by(year) %>%
  summarise(Occurrences_Cropland = sum(cropmap2018_ArableLand_10km >= 0.2, na.rm = TRUE), 
            Occurrences_NonCropland = sum(cropmap2018_ArableLand_10km < 0.2, na.rm = TRUE)) %>%
  pivot_longer(!year, names_to = "Occurrences") %>%
  data.table()

data2plot_10km_1_nonIndic


## Merging with indicator species
data2plot_10km_1_all <- merge(data2plot_10km_1_nonIndic, data2plot_10km_1, all = TRUE,
                              by = c("year", "Occurrences"))

setnames(data2plot_10km_1_all, c("value.x", "value.y"), c("occs_nonIndic", "occs_Indic"))
data2plot_10km_1_all


data2plot_10km_1_all_2plot_2 <- data2plot_10km_1_all %>%
  pivot_longer(!c(year, Occurrences), names_to = "Indic_NonIndic") %>%
  mutate_if(is.character, str_replace_all, pattern = "Occurrences_", replacement = "") %>%
  mutate_if(is.character, str_replace_all, pattern = "occs_", replacement = "") %>%
  mutate_if(is.character, str_replace_all, pattern = "nonIndic", replacement = "Non Sensitive Species") %>%
  mutate_if(is.character, str_replace_all, pattern = "Indic", replacement = "Sensitive Species") %>%
  data.table()

data2plot_10km_1_all_2plot_2



jpeg("NumOccurrences_IndicNonIndic_TimeSeries_tendencyLog_2_R2.jpg", width = 25, height = 25, units = "cm", res = 300)

ggplot(data2plot_10km_1_all_2plot_2, 
       aes(x = year, y = value, color = Occurrences)) + 
  geom_point() + 
  scale_y_log10() +
  ggpmisc::stat_poly_line(se = FALSE) +
  ggpmisc::stat_poly_eq(aes(label = paste(
    after_stat(rr.label),
    after_stat(p.value.label), sep = "*\", \"*"))) +
  theme_bw() +
  scale_color_manual(labels = c("Cropland", "Non-Cropland"), values = viridis(10)[c(1,7)]) +
  facet_wrap(~ Indic_NonIndic, nrow = 2, ncol = 1
             , scales = "free_y") + 
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(color = " ", x = "Year", y = "log(Number of Occurrences)")

dev.off()





## Tolerance to maize and cropland shares ####

sps_nonIndic 
sp_indic_1 <- names(occs_00_indicators)

sp_indic_1 <- c(sp_indic_1, sp_NoIndic)  # all maize weeds; first, indicator sp
save(sp_indic_1, file = "sp_indic_1.RData")
#load("sp_indic_1.RData", verbose = TRUE)


occs_maizeShare4plotting <- occs_maizeShare[species %in% sp_indic_1]

occs_maizeShare4plotting <- occs_maizeShare4plotting %>% 
  mutate(sensitive = ifelse(species %in% sps_nonIndic, "Sensitive_species", 
                            "Non-sensitive_species"))

occs_maizeShare4plotting$species <- factor(occs_maizeShare4plotting$species, levels = sp_indic_1)
occs_maizeShare4plotting


#library("RColorBrewer")
#display.brewer.pal(n = length(unique(dist2maize_1$species)), name = 'RdBu')

library("viridis")
library("scales")
colrs <- viridis_pal()(length(unique(occs_maizeShare4plotting$species)))
#colrs <- viridis_pal()(2)
#show_col(colrs)
#my_colrs <- ifelse(levels(dist2maize_1$species) %in% sps[1:4], colrs[1],
#                   colrs[2])

## Plotting only maize and arable shares (no distance to the closest field)

jpeg(paste0("distance2maize_", "all_37sp_onlyShares_1", ".jpg"), width = 31, height = 27, units = "cm", res = 300)
#par(mfrow = c(2, 2), mar = c(2, 5, 2, 2))
par(mar = c(2, 5, 2, 2))
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE),
       heights = c(2, 1))


boxplot(occs_maizeShare4plotting$cropmap2018_maiz_1km  ~ occs_maizeShare4plotting$species, 
        #ylim = c(0, 0.2),
        ylim = c(0, 0.4),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of maize (1km grid)",
        xaxt = 'n', 
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:12, 14:38),
        cex.lab = 1.4,
        ann = TRUE)
mtext("Sensitive Species           Non-sensitive Species", side = 3, at = 17, cex = 1.2)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
axis(1, cex.axis = 0.9, at = c(1:12, 14:38), labels = 1:length(sp_indic_1), las = 2)


boxplot(occs_maizeShare4plotting$cropmap2018_arabland_1km ~ occs_maizeShare4plotting$species, 
        #ylim = c(0, 1),
        #ylim = c(0, 1),
        col = colrs,
        main = "",
        xlab = "", 
        ylab = "Share of cropland (1km grid)",
        xaxt = 'n', 
        #at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)),
        at = c(1:12, 14:38),
        cex.lab = 1.4,
        ann = TRUE)
mtext("Sensitive Species           Non-sensitive Species", side = 3, at = 17, cex = 1.2)
#axis(1, cex.axis = 0.7, at = c(1:(length(sps)/2), ((length(sps)/2)+2):(length(sps)+1)), labels = 1:length(sps), las = 2)
axis(1, cex.axis = 0.9, at = c(1:12, 14:38), labels = 1:length(sp_indic_1), las = 2)


plot(0, col = "white", bty = "n", axes = FALSE, ann = FALSE)
legend("center", 
       #legend = c("Sensitive species","Non Sensitive sps."), 
       legend = paste0(1:length(sp_indic_1), "-", levels(occs_maizeShare4plotting$species)), 
       col = c(colrs),
       ncol = 3,
       bty = "n", 
       pch = 20 , pt.cex = 3, cex = 1.3, 
       horiz = FALSE)#, 
#inset = - 1)

dev.off()




## Some results for reporting

sp_indic_1_occs <- as.data.table(table(dist2maize$species))
sp_indic_1_occs_1 <- as.data.table(table(occs_all_2018[species %in% sp_indic_1, ]$species))
sp_indic_1_occs <- merge(sp_indic_1_occs, sp_indic_1_occs_1, by = "V1")
names(sp_indic_1_occs) <- c("species", "Used_occurrences", "GBIF_occurrences")
sp_indic_1_occs <- sp_indic_1_occs[sp_indic_1, c(1, 3, 2)]
sp_indic_1_occs
View(sp_indic_1_occs)




dist2maize

sp_NoIndic <- sp_indic_1_occs$species[13:length(sps)]

dist2maize_indicators <- dist2maize[!species %in% sp_NoIndic, ]
unique(dist2maize_indicators$species)

dist2maize_no_indicators <- dist2maize[species %in% sp_NoIndic, ]
unique(dist2maize_no_indicators$species)


names(dist2maize_indicators)

sum(is.na(dist2maize_indicators$dist2maize))
length(dist2maize_indicators$dist2maize)

round(mean(dist2maize_indicators$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_indicators$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(range(dist2maize_indicators$cropmap2018_maiz_1km), 2)
round(mean(dist2maize_indicators$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_indicators$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(mean(dist2maize_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)

round(mean(dist2maize_no_indicators$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_no_indicators$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_no_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_no_indicators$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(range(dist2maize_no_indicators$cropmap2018_maiz_1km), 2)
round(mean(dist2maize_no_indicators$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_no_indicators$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(mean(dist2maize_no_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)
round(sd(dist2maize_no_indicators$cropmap2018_ArableLand_10km, na.rm = TRUE), 2)


# first 5 indicators
#first_5 <- c("Galium tricornutum", "Digitaria ischaemum", "Xanthium spinosum", "Chrozophora tinctoria", "Caucalis platycarpos")
first_5 <- levels(dist2maize_indicators$species)[1:6]

dist2maize_5 <- dist2maize[species %in% first_5, ]

round(mean(dist2maize_5$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_5$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_5$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_5$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(mean(dist2maize_5$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_5$cropmap2018_arabland_1km, na.rm = TRUE), 2)

# Galium
dist2maize_galium <- dist2maize[species %in% c("Galium tricornutum"), ]
round(mean(dist2maize_galium$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_galium$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_galium$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_galium$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(mean(dist2maize_galium$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_galium$cropmap2018_arabland_1km, na.rm = TRUE), 2)

# Tribulus terrestris
dist2maize_tribulus <- dist2maize[species %in% c("Tribulus terrestris"), ]
round(mean(dist2maize_tribulus$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_tribulus$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_tribulus$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_tribulus$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(mean(dist2maize_tribulus$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_tribulus$cropmap2018_arabland_1km, na.rm = TRUE), 2)

# Xanthium spinosum
dist2maize_xanthium <- dist2maize[species %in% c("Xanthium spinosum"), ]
round(mean(dist2maize_xanthium$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_xanthium$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_xanthium$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_xanthium$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(mean(dist2maize_xanthium$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_xanthium$cropmap2018_arabland_1km, na.rm = TRUE), 2)


# Phalaris paradoxa
dist2maize_phal_par <- dist2maize[species %in% c("Phalaris paradoxa"), ]
round(mean(dist2maize_phal_par$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_phal_par$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_phal_par$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_phal_par$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(mean(dist2maize_phal_par$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_phal_par$cropmap2018_arabland_1km, na.rm = TRUE), 2)


# More tolerant
occs_08$Var1

dist2maize_tol <- dist2maize[species %in% occs_08$Var1, ]
round(mean(dist2maize_tol$dist2maize, na.rm = TRUE), 0)
round(sd(dist2maize_tol$dist2maize, na.rm = TRUE), 0)
round(mean(dist2maize_tol$cropmap2018_maiz_1km, na.rm = TRUE), 2)
round(sd(dist2maize_tol$cropmap2018_maiz_1km, na.rm = TRUE), 2)
range(dist2maize_tol$cropmap2018_maiz_1km)
round(mean(dist2maize_tol$cropmap2018_arabland_1km, na.rm = TRUE), 2)
round(sd(dist2maize_tol$cropmap2018_arabland_1km, na.rm = TRUE), 2)
range(dist2maize_tol$cropmap2018_arabland_1km)







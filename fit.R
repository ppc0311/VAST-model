library(tidyverse)
library(dplyr)
library(TMB)
library(VAST)
library(readxl)
library(ggplot2)
library(gridExtra)

setwd("C:\\Users\\jchen\\Desktop\\Thesis\\length bin 2")
load("Data.RData")

data_comb<-rbind(Data10_12,Data12_14,Data14_16,Data16_18,Data18_20,Data20_22,
                 Data22_24,Data24_26,Data26_28,Data28_30,Data30_32,Data32_34,Data34_36,Data36_38,
                 Data38_40,Data40_42,Data42_44,Data44_46,Data46_48,Data48_50,Data50_52)

DF <- select(data_comb, Year, Lat, Lon,  AreaSwept_km2, numbers,length_bin)
Data_Geostat = na.omit(DF)

Data_Geostat[Data_Geostat$Year== "1996"& Data_Geostat$length_bin== "10-12 cm",]$numbers<-NA
Data_Geostat[Data_Geostat$Year== "2007"& Data_Geostat$length_bin== "14-16 cm",]$numbers<-NA
Data_Geostat[Data_Geostat$Year== "2013"& Data_Geostat$length_bin== "14-16 cm",]$numbers<-NA

lengthbin<-as.factor(Data_Geostat[,'length_bin'])

length_bin<-factor(lengthbin, levels = c("10-12 cm", "12-14 cm", "14-16 cm","16-18 cm",
                                         "18-20 cm", "20-22 cm", "22-24 cm","24-26 cm", "26-28 cm", "28-30 cm",
                                         "30-32 cm", "32-34 cm", "34-36 cm","36-38 cm","38-40 cm","40-42 cm","42-44 cm",
                                         "44-46 cm", "46-48 cm", "48-50 cm","50-52 cm"))

settings = make_settings( n_x = 200,
                          Region= "other",
                          purpose = "index2",
                          strata.limits = data.frame(STRATA = "All_areas"),
                          bias.correct = TRUE,
                          Options= c("SD_site_density"=FALSE, "SD_site_logdensity"=FALSE, "Calculate_Range"=FALSE,  
                                     "Calculate_effective_area"=FALSE,"treat_nonencounter_as_zero"=FALSE),
                          FieldConfig= c("Omega1"= "IID", "Epsilon1"= "IID", "Omega2"=  "IID", "Epsilon2"= "IID"),
                          RhoConfig = c("Beta1"=1, "Beta2"=1, "Epsilon1"=1, "Epsilon2"=1),
                          ObsModel=c(2,0),
                          use_anisotropy= FALSE ,
                          n_categories=21,
                          fine_scale = FALSE )

observations_LL<-cbind(Data_Geostat[,'Lon'],Data_Geostat[,'Lat'])
colnames(observations_LL)<-c("Lon","Lat")

setwd("C:\\Users\\jchen\\Desktop\\new knot vast")

DateFile = paste0(getwd(),'/NEW1datacutoff2211#npool#newton2')
dir.create(DateFile)


# Run model
fit = fit_model( settings = settings,
                 Lat_i = Data_Geostat[,'Lat'],
                 Lon_i = Data_Geostat[,'Lon'],
                 t_i = Data_Geostat[,'Year'],
                 b_i = Data_Geostat[,'numbers'],
                 c_i = as.numeric(length_bin)-1 ,
                 a_i = Data_Geostat[,'AreaSwept_km2'],
                 observations_LL = observations_LL,
                 working_dir = paste0(DateFile, "/"),
                 test_fit = FALSE,
                 newtonsteps = 2,
                 silent = TRUE,
                 build_model = TRUE,
                 run_model = TRUE)
#npool=40)

results = plot( fit, check_residuals = FALSE,working_dir = paste0(DateFile, "/") )
save.image(file = "NEW1datacutoff2211#npool#newton2/result.Rdata")

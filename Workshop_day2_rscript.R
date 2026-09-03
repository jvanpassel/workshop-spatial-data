
#1. Load packages
library(rnaturalearth) #to visualize
library(terra) #to manage spatial data
library(sf) #to manage spatial data
library(ggplot2) #to make nice graphs
library(tidyterra) #to make nice graphs of spatial data
library(tsbox) #to work with time series
library(dplyr) #to deal with dataframe manipulation
library(tidyr) #to deal with dataframe manipulation
library(purrr)
library(broom)
library(nlme) #to do linear mixed effect modeling
library(MuMIn) #to get R² values for linear mixed effects

#2. Define your working directory
setwd('C:/Users/jvpassel/OneDrive - UGent/Postdoc/Research stay/Lab/Ideas_workshop/workshop_materials')

#3. Load data from GEE
landsat_mean_2020_2024<-rast('./GEE_outputs/day2_Landsat_2020_2024_mean_Campinas.tif')
monthly_landsat_2020_2024_loc<-read.csv('./GEE_outputs/day2_locations_landsat_NDVI_2020_2024.csv')
locations<-vect('./GEE_outputs/day1_locations_points.shp')
locations_sf<-st_as_sf(locations) 

#visualize data
landsat_mean_2020_2024
plot(landsat_mean_2020_2024)
plotRGB(landsat_mean_2020_2024,r=3,g=2,b=1,stretch='lin')

#4. Calculate extra vegetation indices
#calculate NDWI using green and NIR band
landsat_mean_2020_2024$NDWI_water<-(landsat_mean_2020_2024$green-landsat_mean_2020_2024$NIR)/
  (landsat_mean_2020_2024$green+landsat_mean_2020_2024$NIR)
plot(landsat_mean_2020_2024$NDWI_water,main='NDWI water bodies')

#calculate NDWI using NIR and SWIR band
landsat_mean_2020_2024$NDWI_vegetation1<-(landsat_mean_2020_2024$NIR-landsat_mean_2020_2024$SWIR1)/
  (landsat_mean_2020_2024$NIR+landsat_mean_2020_2024$SWIR1)
landsat_mean_2020_2024$NDWI_vegetation2<-(landsat_mean_2020_2024$NIR-landsat_mean_2020_2024$SWIR2)/
  (landsat_mean_2020_2024$NIR+landsat_mean_2020_2024$SWIR2)
plot(landsat_mean_2020_2024[[10:11]])

#calculate BSI
sw1_r<-(landsat_mean_2020_2024$SWIR1+landsat_mean_2020_2024$red)
nir_b<-(landsat_mean_2020_2024$NIR+landsat_mean_2020_2024$blue)
landsat_mean_2020_2024$BSI1<-(sw1_r-nir_b)/(sw1_r+nir_b)
plot(landsat_mean_2020_2024$BSI1)
writeRaster(landsat_mean_2020_2024,'./GEE_outputs/day2_Landsat_2020_2024_mean_Campinas_VIs.tif')

#5. Visualize NDVI time series over plot locations
colnames(monthly_landsat_2020_2024_loc)
monthly_landsat_2020_2024_df<-monthly_landsat_2020_2024_loc[,c(2,3,5)] #select useful columns
monthly_landsat_2020_2024_df$date<-as.Date(monthly_landsat_2020_2024_df$date) #convert dates to Date format in R

ggplot(monthly_landsat_2020_2024_df)+
  geom_line(mapping = aes(x = date,y = meanNDVI, color = SiteID)) + #plot every SiteID in different colour
  geom_point(mapping = aes(x = date,y = meanNDVI, color = SiteID)) + #plot every SiteID in different colour
  scale_color_manual(values=c("orange","red",'forestgreen','skyblue')) + #manually choose colours per site
  xlab('Date') +
  ylab('NDVI') +
  theme_bw()

summary_ndvi <- monthly_landsat_2020_2024_df %>%
  group_by(SiteID) %>%
  summarise(
    mean_ndvi = mean(meanNDVI, na.rm = T),
    sd_ndvi = sd(meanNDVI,na.rm=T)
  )
summary_ndvi
#site A has highest mean NDVI and lowest variation, while B and D have lowest mean and highest variation

#6. Relate NDVI and precipitation over plot locations
monthly_prec_2020_2024_loc<-read.csv('./GEE_outputs/day1_terraClimate2020_2024_locations.csv')
monthly_prec_2020_2024_df<-monthly_prec_2020_2024_loc[,c(2,3,5)] #select useful columns
monthly_prec_2020_2024_df$date<-as.Date(monthly_prec_2020_2024_df$date) #convert dates to Date format in R

#combine into one dataframe
monthly_ndvi_prec <- left_join(monthly_prec_2020_2024_df, monthly_landsat_2020_2024_df,
                by = c("SiteID", "date"))
number_missingvalues<-monthly_ndvi_prec %>%
  group_by(SiteID) %>%
  summarise(
    n_NA = sum(is.na(meanNDVI))
  )

#plot time series per site
ggplot(monthly_ndvi_prec, aes(date)) +
  geom_col(aes(y = monthlyprec), fill = "skyblue") +
  geom_point(aes(y = meanNDVI * 500), colour = "darkgreen") +
  geom_line(aes(y = meanNDVI * 500), colour = "darkgreen") +
  facet_wrap(~SiteID, ncol = 2) +
  scale_y_continuous(
    name = "Precipitation (mm)",
    sec.axis = sec_axis(~./500, name = "NDVI")
  ) +
  theme_bw()

#plot scatter plot
ggplot(monthly_ndvi_prec, aes(monthlyprec, meanNDVI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()
ggplot(monthly_ndvi_prec, aes(monthlyprec, meanNDVI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~SiteID) +
  theme_bw()

#calculate pearson correlation
corr<-monthly_ndvi_prec %>%
  group_by(SiteID) %>%
  summarise(
    cor = cor(monthlyprec, meanNDVI, use = "complete.obs"),
    p = cor.test(monthlyprec, meanNDVI)$p.value
  )
corr #only site B has significant correlation between NDVI and precipitation

#consider a lag
#NDVI at time t is being related to precipitation at time t − 1
monthly_ndvi_prec <- monthly_ndvi_prec %>%
  arrange(SiteID, date) %>%
  group_by(SiteID) %>%
  mutate(
    prec_lag0 = monthlyprec,
    prec_lag1 = lag(monthlyprec, 1),
    prec_lag2 = lag(monthlyprec, 2),
    prec_lag3 = lag(monthlyprec, 3)
  ) %>%
  ungroup()

corrs<-monthly_ndvi_prec %>%
  group_by(SiteID) %>%
  summarise(
    lag0 = cor(meanNDVI, prec_lag0, use = "complete.obs"),
    p_lag0 = cor.test(meanNDVI, prec_lag0)$p.value,
    lag1 = cor(meanNDVI, prec_lag1, use = "complete.obs"),
    p_lag1 = cor.test(meanNDVI, prec_lag1)$p.value,
    lag2 = cor(meanNDVI, prec_lag2, use = "complete.obs"),
    p_lag2 = cor.test(meanNDVI, prec_lag2)$p.value,
    lag3 = cor(meanNDVI, prec_lag3, use = "complete.obs"),
    p_lag3 = cor.test(meanNDVI, prec_lag3)$p.value
  )
corrs
#significant correlation between NDVI and prec for site B at all lags
#for site C only at one and three month lag
#for site D only at three month lag
#never for site A

#change from wide to long format
monthly_ndvi_prec_long <- monthly_ndvi_prec %>%
  pivot_longer(
    cols = starts_with("prec_lag"),
    names_to = "lag",
    values_to = "precipitation"
  )

ggplot(monthly_ndvi_prec_long,
       aes(precipitation, meanNDVI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(SiteID ~ lag) +
  theme_bw()

ggplot(monthly_ndvi_prec_long,
       aes(precipitation, meanNDVI, colour = lag)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

#linear model
linmodel_lag0 <- lm(meanNDVI ~ prec_lag0, data = monthly_ndvi_prec)
linmodel_lag1 <- lm(meanNDVI ~ prec_lag1, data = monthly_ndvi_prec)
linmodel_lag2 <- lm(meanNDVI ~ prec_lag2, data = monthly_ndvi_prec)
summary(linmodel_lag0)
summary(linmodel_lag1)
summary(linmodel_lag2)
summary(linmodel_lag0)$adj.r.squared
summary(linmodel_lag1)$adj.r.squared
summary(linmodel_lag2)$adj.r.squared
#overall higher R² with one an two month lag in precipitation

#linear mixed effects model
#lme cannot deal with missing values
monthly_ndvi_prec_nona<-monthly_ndvi_prec[complete.cases(monthly_ndvi_prec),]
lmemodel_lag0 <- nlme::lme(meanNDVI ~ prec_lag0,random = ~1|SiteID , data = monthly_ndvi_prec_nona)
lmemodel_lag1 <- nlme::lme(meanNDVI ~ prec_lag1,random = ~1|SiteID , data = monthly_ndvi_prec_nona)
lmemodel_lag2 <- nlme::lme(meanNDVI ~ prec_lag2,random = ~1|SiteID , data = monthly_ndvi_prec_nona)
summary(lmemodel_lag0)
summary(lmemodel_lag1)
summary(lmemodel_lag2)
r.squaredGLMM(lmemodel_lag0)
r.squaredGLMM(lmemodel_lag1)
r.squaredGLMM(lmemodel_lag2)
#overall significant effect of precipitation on NDVI, when accounting for different sites
#higher explanatory power of precipitation when using one or two month lag


#1. Load packages
library(rnaturalearth) #to visualize
library(terra) #to manage spatial data
library(sf) #to manage spatial data
library(ggplot2) #to make nice graphs
library(tidyterra) #to make nice graphs of spatial data
library(tsbox) #to work with time series
library(dplyr) #to deal with dataframe manipulation
library(tidyr) #to deal with dataframe manipulation

#2. Define your working directory
setwd('C:/Users/jvpassel/OneDrive - UGent/Postdoc/Research stay/Lab/Ideas_workshop/workshop_materials')

#3. Load data from GEE
monthly_prec_2020_2024<-rast('./GEE_outputs/day1_terraClimate2020_2024_monthly.tif')
annual_prec_2020_2024<-rast('./GEE_outputs/day1_terraClimate2020_2024_ap.tif')
monthly_prec_2020_2024_loc<-read.csv('./GEE_outputs/day1_terraClimate2020_2024_locations.csv')
locations<-vect('./GEE_outputs/day1_locations_points.shp')
locations_sf<-st_as_sf(locations) 

#4. Visualize spatial data
#create background of country contours
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
southamerica_countries<-rnaturalearth::ne_countries(continent = 'south america', returnclass = "sf")
brazil<-rnaturalearth::ne_countries(country = 'brazil', returnclass = "sf")
plot(southamerica_countries)

#look at GEE data in more detail
annual_prec_2020_2024
plot(annual_prec_2020_2024)
plot(annual_prec_2020_2024,range=c(0,5000)) #range allows to plot with same legend for better comparison
plot(annual_prec_2020_2024[[1:3]]) #only plot first 3 rasterlayers
plot(annual_prec_2020_2024[[1]]) #only plot first rasterlayer (total precipitation 2020)
plot(locations,add=T) #add plot locations
plot(monthly_prec_2020_2024) #notice that raw precipitation data is not masked for land areas, while preprocessed annual data is

#5. Calculate climate metrics
#5.1 Mean annual precipitation from total annual precipitation
map_2020_2024<-app(annual_prec_2020_2024,fun=mean) #use app to apply functions over spatraster
map_2020_2024<-mean(annual_prec_2020_2024)  #for basic functions (mean, sum, max, min), also possible to use shortcut

#5.2 Monthly precipitation anomalies from monthly precipitation
#anomaly = (precipitation - monthly mean precipitation)/ monthly st dev precipitation
#start by masking monthly precipitation to Brazil boundaries, using same crs
brazil_crs<-st_transform(brazil,crs(monthly_prec_2020_2024))
monthly_prec_2020_2024_mask<-mask(monthly_prec_2020_2024,brazil_crs)
#create index with one number per month (1 for January, 2 for February, ...)
months_yr_total<-rep(seq(1:12),5)
#calculate monthly mean and standard deviation of precipitation
#use tapp to apply functions over subset of spatraster layers, using index
monthly_prec_2020_2024_mean<-tapp(monthly_prec_2020_2024_mask,fun=mean, index=months_yr_total)
plot(monthly_prec_2020_2024_mean,range=c(0,900))
monthly_prec_2020_2024_sd<-tapp(monthly_prec_2020_2024_mask,fun=sd, index=months_yr_total)
#build full time series from 2020-2024 for both mean and sd
monthly_prec_2020_2024_mean_full<-monthly_prec_2020_2024_mean[[months_yr_total]]
monthly_prec_2020_2024_sd_full<-monthly_prec_2020_2024_sd[[months_yr_total]]
#calculate anomalies (positive values indicate higher-than-usual rainfall, negative lower-than-usual)
monthly_prec_2020_2024_anomaly<-(monthly_prec_2020_2024_mask-monthly_prec_2020_2024_mean_full)/monthly_prec_2020_2024_sd_full
#if SD is zero, resulting anomaly is NA, so convert NAs to zero and mask again to country
monthly_prec_2020_2024_anomaly[is.na(monthly_prec_2020_2024_anomaly)]<-0
monthly_prec_2020_2024_anomaly<-mask(monthly_prec_2020_2024_anomaly,brazil_crs)

#6. Make nice map of MAP
plot(map_2020_2024)
ggplot() +
  #plot spatraster
  geom_spatraster(data=map_2020_2024) +
  #define pixel colours and legend title
  #low = colour of lowest value, high = colour of highest value, na.value = colour of missing values
  #limits = interval of values that are plotted
  # \n to start writing on a new line in the legend title
  scale_fill_gradient('Mean annual\nprecipitation\n2020-2024 (mm)',
                      low='gold',high='darkblue',na.value=NA,limits=c(0,5000)) +
  #map country boundaries
  geom_sf(data = southamerica_countries,fill = NA, color = "grey17") +
  #map points
  geom_sf(data = locations_sf,color='red',size=1)+
  #define axes and labels
  scale_y_continuous("Latitude",expand = c(0, 0)) +
  scale_x_continuous("Longitude",expand = c(0, 0)) +
  #change layout
  theme_bw()+
  theme(axis.text = element_text(size=7),legend.position = "right")

#7. Plot time series of monthly precipitation over locations
colnames(monthly_prec_2020_2024_loc)
monthly_prec_2020_2024_df<-monthly_prec_2020_2024_loc[,c(2,3,5)] #select useful columns
monthly_prec_2020_2024_df$date<-as.Date(monthly_prec_2020_2024_df$date) #convert dates to Date format in R

#all sites in one plot
ggplot(monthly_prec_2020_2024_df)+
  geom_line(mapping = aes(x = date,y = monthlyprec, color = SiteID)) + #plot every SiteID in different colour
  scale_color_manual(values=c("orange","red",'forestgreen','skyblue')) + #manually choose colours per site
  xlab('Date') +
  ylab('Monthly precipitation (mm)') +
  theme_bw()

#all sites in separate plots
ggplot(monthly_prec_2020_2024_df)+
  geom_line(mapping = aes(x = date,y = monthlyprec, color = SiteID)) + #plot every SiteID in different colour
  scale_color_manual(values=c("orange","red",'forestgreen','skyblue')) + #manually choose colours per site
  xlab('Date') +
  ylab('Monthly precipitation (mm)') +
  facet_wrap(~ SiteID) +
  theme_bw()

#8. Extract and plot precipitation anomaly time series over locations
locations_anomalies<-terra::extract(monthly_prec_2020_2024_anomaly,locations) 
#include the SiteID as row names and remove the ID column
locations_anomalies$SiteID <- locations$SiteID[locations_anomalies$ID]
locations_anomalies<-locations_anomalies[,-1]
#convert to long format for easier plotting with ggplot
locations_anomalies_long <- locations_anomalies %>%
  pivot_longer(
    cols = -SiteID,
    names_to = "date",
    values_to = "anomalies"
  )
#convert date column to actual dates
locations_anomalies_long <- locations_anomalies_long %>%
  mutate(
    date = as.Date(
      paste0(sub("_pr$", "", date), "01"),
      format = "%Y%m%d"
    )
  )
ggplot(locations_anomalies_long)+
  geom_line(mapping = aes(x = date,y = anomalies, color = SiteID)) + #plot every SiteID in different colour
  scale_color_manual(values=c("orange","red",'forestgreen','skyblue')) + #manually choose colours per site
  xlab('Date') +
  ylab('Precipitation anomalies') +
  theme_bw()
ggplot(locations_anomalies_long)+
  geom_line(mapping = aes(x = date,y = anomalies, color = SiteID)) + #plot every SiteID in different colour
  scale_color_manual(values=c("orange","red",'forestgreen','skyblue')) + #manually choose colours per site
  xlab('Date') +
  ylab('Precipitation anomalies') +
  facet_wrap(~SiteID) +
  theme_bw()

#9. Decompose precipitation time series into trend, seasonality and remainder time series
#split into individual dataframes per siteID
siteA<-monthly_prec_2020_2024_df[monthly_prec_2020_2024_df$SiteID=='A',]
siteB<-monthly_prec_2020_2024_df[monthly_prec_2020_2024_df$SiteID=='B',]
siteC<-monthly_prec_2020_2024_df[monthly_prec_2020_2024_df$SiteID=='C',]
siteD<-monthly_prec_2020_2024_df[monthly_prec_2020_2024_df$SiteID=='D',]
#convert to time series object 
siteA_ts<-ts_ts(ts_long(siteA[,c(2,3)]))
siteB_ts<-ts_ts(ts_long(siteB[,c(2,3)]))
siteC_ts<-ts_ts(ts_long(siteC[,c(2,3)]))
siteD_ts<-ts_ts(ts_long(siteD[,c(2,3)]))
plot(siteA_ts)
#decompose using STL decomposition
siteA_stl<-stl(siteA_ts,s.window = 'periodic')
siteB_stl<-stl(siteB_ts,s.window = 'periodic')
siteC_stl<-stl(siteC_ts,s.window = 'periodic')
siteD_stl<-stl(siteD_ts,s.window = 'periodic')
plot(siteA_stl,main='Site A')
plot(siteB_stl,main='Site B')
plot(siteC_stl,main='Site C')
plot(siteD_stl,main='Site D')
#compare trends and seasonality among sites
plot(siteA_stl$time.series[,'trend'],ylim=c(0,200),col='orange')
lines(siteB_stl$time.series[,'trend'],col='red')
lines(siteC_stl$time.series[,'trend'],col='forestgreen')
lines(siteD_stl$time.series[,'trend'],col='skyblue')




#libraries
install.packages('sp')
install.packages('raster')
install.packages('rgdal')
install.packages('dplyr')
install.packages('rgeos')

library (sp)
library(raster)
library (rgdal)
library(dplyr) 
library(rgeos)

# Set working directory
setwd("~/Desktop/Pasteur_workshop")


Corona_data <- read.csv("novel-coronavirus.csv")
View(Corona_data)


names(Corona_data)
summary(Corona_data )
Corona_data[is.na(Corona_data)] <- 0


# Checking on number of cases per day
Cases_data = Corona_data %>% 
  select(country, contains('confirmedcases')) %>%
  group_by(country) %>%
  summarise_all(funs(sum))

View(Cases_data) 
dim (Cases_data) # 23 rows and 22 columns


barplot(colSums(Cases_data[,2:21]))
barplot(colSums(Cases_data[,2:length(Cases_data)]),las=2,main='Corona cases') # making it horz

# Checking on number of death per day
Death_data = Corona_data %>% 
  select(country, contains('death')) %>%
  group_by(country) %>%
  summarise_all(funs(sum))

barplot(colSums(Death_data[,2:21]))
barplot(colSums(Death_data[,2:length(Death_data)]),las=2, main='Corona death') # making it horz
length(Death_data)

#plotting
#Creating spatial points
Corona_locations<- subset(Corona_data, select=c('country','location',"longitude", "latitude"))
coordinates(Corona_locations) <- c("longitude", "latitude")
plot(Corona_locations,col='red', pch=2, cex=0.2,lwd=2)

# World shapefile
World_Countries <- shapefile("World_Countries/World_Countries.shp")
World_Countries 
World_Countries$COUNTRY

plot(World_Countries  , border='black', col='light grey', lwd=0.5)


#removing Antartica
World_Countries<-World_Countries[World_Countries$COUNTRY != "Antarctica",]
plot(World_Countries, border='black', col='light grey', lwd=0.5,main = "Coronavirus cases in the world")

laea <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs") 
# Lambert Azimuthal Equal Are
World_Countries_laea <- spTransform(World_Countries,laea)  # project
plot(World_Countries_laea) # new projection

#Subsetting multiple countries
Asian_countries<- c("China","Taiwan","Japan",'Nepal','South Korea','Singapore',
                    'Thailand','Vietnam','Macau','Hong-Kong','Malaysia')
Asia<-World_Countries[World_Countries$COUNTRY  %in%  Asian_countries,]
Corona_asia<-Corona_locations[Corona_locations$country%in%  Asian_countries,]
plot(Asia, border='black', col='light grey', lwd=0.5,main='Coronavirus cases in Asia')
plot(Corona_asia,col='red', pch=2, cex=0.2,lwd=2, add= TRUE)
text(Corona_asia,Corona_asia$location, pos = 4,cex= 0.6)

# Cropping a shapefile and points
plot(World_Countries  , border='black', col='light grey', lwd=0.5)
drawExtent()

e<-extent(xmin,xmax ,ymin,ymanx)
World_Countries_crop<- crop (World_Countries,e)
Corona_locations_crop<- crop (Corona_locations,e)
plot(World_Countries_crop, border='black', col='light grey', lwd=0.5,main = "Coronavirus cases in this part of the world")
plot(Corona_locations_crop,col='red', pch=2, cex=0.2,lwd=2, add= TRUE)
text(Corona_locations_crop,Corona_locations_crop$location, pos = 4,cex= 0.6)

#Choropleth
World_Countries_cases<- merge(World_Countries, Cases_data, by.x='COUNTRY', by.y='country')
spplot( World_Countries_cases,'confirmedcases_26.01.2020',col.regions= rev(terrain.colors(25) ),main='Chlor. cases by regions')

World_Countries_deaths<- merge(World_Countries, Death_data, by.x='COUNTRY', by.y='country')
spplot( World_Countries_deaths,'deaths_26.01.2020',col.regions= rev(terrain.colors(25) ),main='Chlor. deaths by regions')

#trying it for China

China_shp<- shapefile('CHN_adm/CHN_adm2.shp')

China_shp_Cases<- merge(China_shp, China_Cases, by.x='NAME_1', by.y='location')
spplot( China_shp_Cases,'confirmedcases_26.01.2020',col.regions= rev(terrain.colors(25) ),main='Chlor. cases by regions in China')

China_shp_Deaths<- merge(China_shp, China_Deaths, by.x='NAME_1', by.y='location')
spplot( China_shp_Deaths,'deaths_26.01.2020',col.regions= rev(terrain.colors(25) ),main='Chlor. deaths by regions in China')

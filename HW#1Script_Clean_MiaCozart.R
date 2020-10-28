
#audubon_data clean

audubon_data<- read.csv('audubon_data.csv', row.names=1)

audubon_data_transect<- audubon_data[audubon_data$Survey_Type =="transect",]
#the above will remove anything except the transect data 

#remove N/A data 
complete.cases(audubon_data_transect) 
which(!complete.cases(audubon_data_transect))
NA_Val_adata<- which(!complete.cases(audubon_data_transect))
a_data2 <-audubon_data_transect[-NA_Val_adata,]

a_data2$Longitude<-  gsub("W","-" , a_data2$Longitude)
a_data2$Latitude <- gsub("N", "+", a_data2$Latitude)
head(a_data2)

#dates past 2010-01-01
a_data2$Date <- as.Date(a_data2$Date, format= "%d-%b-%y")
class(a_data2$Date)
head(a_data2)
a_data2 <- subset(a_data2, Date >= "2010-01-01")
head(a_data2)


#___________________________________________________________________________

#gw_data clean

gw_data<- read.csv('gw_data.csv', row.names=1)

gw_data_transect<- gw_data[gw_data$Survey_Type =="transect",]
#above removes all survey_type data that is not "transect" 

complete.cases(gw_data_transect) 
which(!complete.cases(gw_data_transect))
NA_Val_gw_data<- which(!complete.cases(gw_data_transect))
gw_data2 <-gw_data_transect[-NA_Val_gw_data,]
head(gw_data2)
#above turns the N/A into logicals and then removes the false ones 
# or the ones that are ! not TRUE

#clean dates past 2010-01-01
gw_data2$Date <- as.Date(gw_data2$Date, format= "%d-%b-%y")
class(gw_data2$Date)
head(gw_data2)
gw_data2 <- subset(gw_data2, Date >= "2010-01-01")
head(gw_data2)

#clean longitude and latitude 
library(measurements)
gw_data2$Longitude<- gsub('°', ' ', gw_data2$Longitude)
gw_data2$Latitude<- gsub('°', ' ', gw_data2$Latitude)
gw_data2$Longitude<- gsub("'W", '', gw_data2$Longitude)
gw_data2$Latitude<- gsub("'N", '', gw_data2$Latitude)
head(gw_data2)
gw_data2$Longitude<- measurements::conv_unit(gw_data2$Longitude, from= 'deg_dec_min', to= 'dec_deg')
gw_data2$Latitude<- measurements::conv_unit(gw_data2$Latitude, from= 'deg_dec_min', to= 'dec_deg')
gw_data2$Longitude<- as.numeric(gw_data2$Longitude)
gw_data2$Latitude<- as.numeric(gw_data2$Latitude)
gw_data2$Longitude<- gw_data2$Longitude*(-1)
head(gw_data2)


#______________________________________________________________________________
#natgeodata clean

nat_geo_data<-read.csv('nat_geo_data.csv', row.names=1)

natgeo_data_transect<- nat_geo_data[nat_geo_data$Survey_Type =="transect",]
#the above will remove anything except the transect data 

#remove N/A data 
complete.cases(natgeo_data_transect) 
which(!complete.cases(natgeo_data_transect))
NA_Val<- which(!complete.cases(natgeo_data_transect))
natgeo_data2 <-natgeo_data_transect[-NA_Val,]
head(natgeo_data2)

#filter dates past 2010-01-01
natgeo_data2$Date <- as.Date(natgeo_data2$Date, format= "%d-%b-%y")
class(natgeo_data2$Date)
head(natgeo_data2)
natgeo_data2 <- subset(natgeo_data2, Date >= "2010-01-01")
head(natgeo_data2)



#__________________________________________________________________________________________________--
#combine data sets and add map template and neighborhood files 

combined_df<- rbind(a_data2, natgeo_data2, gw_data2 )

setwd("C:/Users/miaco/Documents/DataAnalysisWithR")

write.csv(gw_data2, file= "my_clean_data.csv", row.names=FALSE)

#_________________________________________________________________________________________

#given map code

library(sp)
library(rgdal)

setwd("C:/Users/miaco/Documents/DataAnalysisWithR")

clean_data <- read.csv("my_clean_data.csv")

plotting_data <- SpatialPoints(clean_data[, c("Longitude", "Latitude")])

#Map of DC neighborhoods from maps2.dcgis.dc.gov
dc <- readOGR("Neighborhood_Clusters-shp", "Neighborhood_Clusters")

#Plot the map of DC

par(mar = c(1, 1, 1, 1))

plot(
  dc,
  col = "darkgrey",
  border = "white",
  main = "District of Columbia Bird Sightings"
)
plot(dc[46, ],
     add = TRUE,
     col = "#718BAE80",
     border = "white")


#Add your data

plot(plotting_data,
     add = TRUE,
     pch = 16,
     cex = 0.25)


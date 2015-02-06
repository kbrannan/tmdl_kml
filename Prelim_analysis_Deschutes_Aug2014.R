### 2/6/15

# Dan Sobota

# Making Google Earth layers out of data collected on the Little and Upper Deschutes, August 2014

### Load required packages----

library(RODBC)
library(reshape2)
library(plyr)
library(sp)
library(maptools)
library(foreign)
library(rgdal)
library(plotKML)
library(rgeos)
library(brew)
library(RColorBrewer)

setwd("T:\\R workarounds and fixes\\Custom packages")
library(time.to.numeric)
setwd("E:\\TMDL\\TMDL-Scripts")

### Station list for Little and Upper Deschutes Rivers----

# Name codes
codes<-c("Little Deschutes","Upper Deschutes")

# Little Deschutes
LDStations<-c(37747,12564,12565,25564,25565,25566,25563,37746,10703,35316,10700,10699,
              10698,37749,31837,10697,37750,37751,37752,37753,12567,37754,37755,37756,
              37757,37758,37759,37760)

# Upper Deschutes
UDStations<-c(25836,35380,25837,10689,37761,37762,37763,37764,25165,12560,37765,37766,
              37767,37768,10687,37769,37770,37771,37772,37773,37774,37775,37776,10694,
              10685,37777,37778,37779,37780,29407,10511)

# Making Little Deschutes and Upper Deschutes stations into a list for Google Earth processing
myStations.list<-list(LDStations,UDStations)
names(myStations.list)<-codes

# Converting list of sites into a single vector for the database query
myStations<-c()

for (i in 1:length(myStations.list)){
  if(i==1){
    myStations<-myStations.list[[i]]
  }
  else{
    myStations<-c(myStations,myStations.list[[i]])
  }
}

## Retrival of data collected in August 2014----

options(stringsAsFactors = FALSE)

channel <- odbcConnect("ELEMENT-Repository")

# Grab the names of all the tables in the database
TableNames.EL<- sqlTables(channel,errors = FALSE)

# Create SQL Query

myQuery <- c()

for (i in 1:length(myStations)) {
  qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE Station_ID ='",myStations[i],"' AND Matrix ='River/Stream'")
  myQuery <- append(myQuery, qry)
}

## Retreive data.
for(i in 1:length(myQuery)) {
  print(myQuery[i])
  data <- sqlQuery(channel,myQuery[i],stringsAsFactors = FALSE, na.strings = "NA")
  ifelse(i==1,mydata <- data, mydata <- rbind(mydata,data))
  rm(data)
}

# Select dates in range

start<-time_to_numeric("2014-08-11 00:00:00")
end<-time_to_numeric("2014-08-22 00:00:00")

Des.Aug.data<-mydata[as.numeric(mydata$Sampled)>start & as.numeric(mydata$Sampled)<end,]

# Need to extract geospatial information

# Make a connection to LASAR2 DEV
channel2 <- odbcConnect("LASAR2 DEV")

# Grab the names of all the tables in the database
TableNames<- sqlTables(channel2,errors = FALSE)

# Grab all the stations in the database
StationsAll <- sqlFetch(channel2, "STATION")

# Pull out relevant stations for analysis
Des.StationsAll<-StationsAll[StationsAll$STATION_KEY %in% myStations,]

Des.station.sub<-subset(Des.StationsAll,select=c(STATION_KEY,LOCATION_DESCRIPTION,
                                                 DECIMAL_LAT,DECIMAL_LONG))

# closing channels
close(channel)
close(channel2)

### Getting data into right format----

# Merge spatial data and sample data
Des.Aug.data.2<-merge(Des.station.sub,Des.Aug.data,by.x="STATION_KEY",by.y="Station_ID")

# Cleaning up
Des.Aug.data.final<-subset(Des.Aug.data.2,select=c(STATION_KEY, LOCATION_DESCRIPTION,
                                                   DECIMAL_LAT, DECIMAL_LONG, Sampled,SampleType,
                                                   Analyte,Result,MDL,MRL,Units,CASNum,DQL))

# Need to get ampersand code right for xml coding
Des.Aug.data.final$LOCATION_DESCRIPTION<-gsub("&","&amp;",Des.Aug.data.final$LOCATION_DESCRIPTION)

# Using only field primaries or grab samples here
Des.Aug.data.final<-Des.Aug.data.final[Des.Aug.data.final$SampleType=="Field Primary::FP"|Des.Aug.data.final$SampleType=="Grab Sample::GS",]

# Replacing ND and Voids with zeros
Des.Aug.data.final$Result<-as.numeric(Des.Aug.data.final$Result)
Des.Aug.data.final$Result[is.na(Des.Aug.data.final$Result)]<-0

#Adding column for Analyte and units combination; need to be in UTF-8 or WINDOWS-1252 encrypting
Des.Aug.data.final$Analyte.units<-paste0(Des.Aug.data.final$Analyte," (",Des.Aug.data.final$Units,")")

# Picking out essential columns; can add or modify later
Des.Aug.data.final2<-dcast(Des.Aug.data.final,STATION_KEY+LOCATION_DESCRIPTION+Sampled+
                           DECIMAL_LAT+DECIMAL_LONG~Analyte.units,value.var="Result",fun.aggregate=mean)

# Splitting Little Deschutes and Upper Deschutes into lists
Des.Aug.data.list<-list()
for (i in 1:length(myStations.list)){
  Des.Aug.data.list[[i]]<-Des.Aug.data.final2[Des.Aug.data.final2$STATION_KEY %in% myStations.list[[i]],]
  row.names(Des.Aug.data.list[[i]])<-NULL
}

# Naming the list components to be consistent with station list
names(Des.Aug.data.list)<-codes

### Google Earth files----

# Make shape files
Des.Aug.Sp<-list()
for (i in 1:length(Des.Aug.data.list)){
  Des.Aug.Sp[[i]]<-SpatialPointsDataFrame(coords=Des.Aug.data.list[[i]][,c("DECIMAL_LONG","DECIMAL_LAT")], data=Des.Aug.data.list[[i]], proj4string = CRS("+proj=longlat +datum=NAD83"))
  Des.Aug.Sp[[i]]<-spTransform(Des.Aug.Sp[[i]],CRS("+init=epsg:4326")) #Google Earth Projection
}

# Naming the list components to be consistent with station list
names(Des.Aug.Sp)<-codes  

#### Write KML files for Google Earth----

brew(file=".\\kml_templates\\Des_brew_Aug14.kmlt",output="Deschutes_Aug14.kml")

#### Clean up----
rm(list=ls())

############################################################################################################
############################################################################################################

############# Migration and parturition timing plasticity + repeatability ##################################

############# MP Laforge, M Bonar, E Vander Wal ############################################################

############# February 21st, 2020 ##########################################################################

############# Script 1: cleaning the raw data and exporting for MM #########################################

############################################################################################################
############################################################################################################

## Read in the data

data<-read.csv("Inputs/Locs/AllCaribouDataRaw.csv")

## Packages

library(adehabitatLT)
library(data.table)
library(rgdal)
library(lubridate)

## Keep only needed herds, only GPS data, and only females:

data<-subset(data,HERD=="MIDRIDGE"|HERD=="TOPSAILS"|HERD=="GREY"|HERD=="LAPOILE"|HERD=="BUCHANS")
data<-subset(data,COLLAR_TYPE_CL=="GPS")
data<-subset(data,SEX=="F")

data<-droplevels(data)

## Remove unnecessary columns

data$X<-NULL
data$SPECIES<-NULL
data$SEX<-NULL
data$FIX_ID<-NULL
data$EPSG_CODE<-NULL
data$Map_Quality<-NULL
data$Fix_Time_Delta<-NULL
data$COLLAR_FILE_ID<-NULL
data$COLLAR_ID<-NULL
data$COLLAR_TYPE_CL<-NULL
data$EXCLUDE<-NULL
data$VENDOR_CL<-NULL
data$AGE<-NULL
data$DOP<-NULL
data$NAV<-NULL
data$VALIDATED<-NULL
data$LOCQUAL<-NULL

summary(data)

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
locs<-project(cbind(data$X_COORD, data$Y_COORD),utm21N)
coords<-coordinates(locs)

data<-data.frame(data,locs)
data$date<-as.POSIXct(paste(data$FIX_DATE, data$FIX_TIME, sep = ' '), format="%Y-%m-%d %H:%M:%S", tz="UTC")

data$FIX_DATE<-NULL
data$FIX_TIME<-NULL
data$X_COORD<-NULL
data$Y_COORD<-NULL

colnames(data)<-c("ID","Herd","Easting","Northing","Date")

data<-subset(data,Easting>0 & Easting<800000)
data<-subset(data,Northing>5200000 & Northing<6000000)

## Add in date and year column
data$year<-as.factor(year(data$Date))
data$day<-yday(data$Date)

data<-droplevels(data)

## Add in ID year field
data$IDyear<-paste(data$ID,data$year, sep="_")

## Add POSIXct time field
data$Time<-as.POSIXct(data$Date)

##Dealing with error - non-unique dates for given burst (duplicate rows?)
##Add in a "burst date" field
data$IDyear_time<-paste(data$IDyear,data$Time)

## Remove duplicated ID*year*Time
data<-data[!duplicated(data$IDyear_time),]

data<-droplevels(data)

## Make an LTRAJ object to get net squared displacement (NSD)
Traj<-as.ltraj(xy=data.frame(data$Easting,data$Northing), date=data$Time, 
               id=data$ID, burst=data$IDyear, 
               proj4string = CRS(utm21N))

## Turn the ltraj back to df and add back the year and julian day fields

data2<-ld(Traj)
data2$IDyear_time<-paste(data2$burst,data2$date)

## Re-maerge LTRAJ data with original data
data3<-merge(data,data2,by="IDyear_time")

data<-data3

data$displace<-sqrt(data$R2n)/1000

### A bit more cleaning...get rid of spatial outliers
### First make a displacement/time column (km/h)

data$movRate<-(data$dist/1000)/(data$dt/3600)

data<-subset(data, movRate<12)

data$JDateTime<-data$day+(hour(data$date)+minute(data$date)/60+second(data$date)/3600)/24

## Remove unnecessary columns

data$IDyear_time<-NULL
data$Date<-NULL
data$JDay<-data$day
data$day<-NULL
data$NumYear<-NULL
data$Easting<-NULL
data$Northing<-NULL
data$date<-NULL
data$dx<-NULL
data$dy<-NULL
data$dist<-NULL
data$dt<-NULL
data$R2n<-NULL
data$abs.angle<-NULL
data$rel.angle<-NULL
data$id<-NULL
data$IDyear<-NULL
data$pkey<-NULL
data$displace<-NULL
data$movRate<-NULL
data$MaxDisp<-NULL
data$X<-NULL
data$CalvingDate<-NULL
data$Counts<-NULL
data$PtsPerDay<-NULL
data$dayCount<-NULL
data$IDYearDay<-NULL
data$datDay<-NULL
data$PropDataDays<-NULL
data$Calved<-NULL
data$SLength<-NULL

data$Month<-month(data$Time)
data$Day<-day(data$Time)
data$Hour<-hour(data$Time)
data$Minute<-minute(data$Time)
data$Second<-second(data$Time)

head(data)

saveRDS(data,"Output/Locs/CleanData_March11th.RDS")
write.csv(data,"Output/Locs/CleanDataforMM_March11th.csv")

#########################  END ###############################################




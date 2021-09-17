############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn M. R. Webber, E Vander Wal #################################################

############# August 11th, 2020 ############################################################################

############# Script 2: Appending parturition dates from MB and migration data from MM #####################

############# Script essentially the same as Script 1 from Laforge et al (Ecology, 2020) ###################
############################################################################################################
############################################################################################################

library(data.table)
library(lubridate)

### Read migration start/end times

Mig<-read.csv("Output/MigrationMapper/tabSixOutputs/migtime.csv")

### Need to append herd data to MM output for join. Join using main data:

data<-readRDS("Output/Locs/CleanData_March11th.RDS")
## Make field for joining
data$id_yr<-paste(data$ID,data$year,sep="_")

## Remove individuals that didn't migrate 
Mig<-subset(Mig,springMig==1 & notes!="nm")

### Clean it up a bit:
Mig$nsdYear<-NULL
Mig$startFall<-NULL
Mig$endFall<-NULL
Mig$springMig<-NULL
Mig$fallMig<-NULL
Mig$notes<-NULL
Mig$moveType<-NULL
Mig$fallMigDst<-NULL
Mig$classifiedBy<-NULL
Mig$X<-NULL


### Adding in Julian date versions of all dates
Mig$StartMig<-yday(Mig$startSpring)
Mig$EndMig<-yday(Mig$endSpring)


### Remove original date columns
Mig$startSpring<-NULL
Mig$endSpring<-NULL
Mig$newUid<-NULL

### Truncate migration date. If migration end is after parturition, 
### use parturition date as migration end


data<-merge(data,Mig,by="id_yr")

data$Month<-NULL
data$Day<-NULL
data$Hour<-NULL
data$Minute<-NULL
data$Second<-NULL
data$burst<-data$id_yr
data$id_yr<-NULL
data$burst<-as.factor(data$burst)

summary(data)
##### Add in the parturition dates

Surv<-read.csv("Inputs/Parturition/AllHerdsSurvival2hrMR.csv")
Surv$X<-NULL
Surv$PBMcalf<-yday(Surv$CalvingDate)
Surv$Calved<-NULL
Surv$CalvingDate<-NULL
Surv$LossDate<-NULL

Surv$surv<-ifelse(Surv$Lost==TRUE,0,1)

Part_update<-read.csv("BRNupdate_complete.csv")
Part_up<-data.frame(Part_update$ID.year,Part_update$NewDate)

colnames(Part_up)<-c("IDyear","CalfDateManual")

Surv2<-merge(Surv,Part_up,by="IDyear")
Surv2$Lost<-NULL

Surv2$CalfDateHybrid<-ifelse(Surv2$PBMcalf==142,Surv2$CalfDateManual,Surv2$PBMcalf)

data$IDyear<-paste(data$Herd,data$burst,sep="_")

data2<-merge(data,Surv2,by="IDyear")

### Matches here!

data2$EndCalving<-data2$CalfDateHybrid+21


data2 <- data2[order(data2$ID, data2$Time),]

sub<-subset(data2,ID=="mr2009a14")

data2<-subset(data2,JDay>=StartMig)
IDPre<-unique(droplevels(data2$ID))
data2<-subset(data2,JDay<=EndCalving)### This is where sample size goes from 94 to 92
IDPost<-unique(droplevels(data2$ID))

IDPre  ## mr2009a14 ____ 2011
IDPost

IDPre<-unique(data2$ID)


summary(data2)
data<-data2

data$IDyear<-as.factor(data$IDyear)

## Order the data
data<-data[order(data$Herd,data$ID,data$Time),]

## Save the data
saveRDS(data,"Output/Locs/CleanDataSeasonal_March11th.RDS")

################### END ########################
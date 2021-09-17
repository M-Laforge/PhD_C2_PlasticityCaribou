############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn M. R. Webber, E Vander Wal #################################################

############# August 10th, 2020 ############################################################################

############# Script 3: Determining date of snow melt across each herd's range #############################

############################################################################################################
############################################################################################################

library(raster)
library(adehabitatHR)
library(sp)

### Read in data
data<-readRDS("Output/Locs/CleanDataSeasonal_March11th.RDS")
data<-droplevels(data)

### Set percentiles to estimate using cellstats/quantiles
probs<-seq(1,99, by = 1)/100

### All years of data (run for all herds)
years<-c(seq(2007,2013,1))
### List populations to process (add more as data become available)
Pops<-unique(data$Herd)

### Loops through all years and populations, reads in the relevant day of peak IRG data and crops it to 
### the population's 99% MCP and gets percentile values for pixels reaching peak green-up for all years. 
t<-1
for(j in Pops){
  subData<-subset(data,Herd==j)
  pts<-SpatialPoints(cbind(subData$x,subData$y),
                     proj4string=CRS("+proj=utm +zone=21 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  MCP<-mcp(pts,percent=99)
  MCPrp<-spTransform(MCP,CRSobj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  w<-4
  for(i in years){
    NDSI<-raster("Inputs/Landcover/NDSI-snow-off.tif", band = w)
    NDSI2<-mask(NDSI,MCPrp)
    NDSI3<-crop(NDSI2,MCPrp)
    IRG<-raster(eval(paste(text="Inputs/Landcover/PeakIRG/PeakIRG",i,".tif",sep="")))
    IRG2<-mask(IRG,MCP)
    IRG3<-crop(IRG2,MCP)
    QuantilesSnow<-data.frame(quantile(NDSI3, probs=probs))
    QuantilesGreen<-data.frame(quantile(IRG3, probs=probs))
    colnames(QuantilesSnow)<-c(paste(j,i,sep="_"))
    colnames(QuantilesGreen)<-c(paste(j,i,sep="_"))
    #Quantiles<-cbind(QuantilesSnow,QuantilesGreen)
    if(t==1){
      phenolSnow<-QuantilesSnow
      phenolGreen<-QuantilesGreen
    } else {
      phenolSnow<-cbind(phenolSnow,QuantilesSnow)
      phenolGreen<-cbind(phenolGreen,QuantilesGreen)
    }
    t<-t+1
    w<-w+3
    print(i)
    print(j)
  }
}

phenolSnow
phenolGreen

## Save the output
saveRDS(phenolSnow, "Output/Phenology/SnowOffDates_March11th.RDS")
saveRDS(phenolGreen, "Output/Phenology/GreenUpDates_March11th.RDS")

## Read back in
phenolSnow<-readRDS("Output/Phenology/SnowOffDates.RDS")

### Dates of snow off:

library(lubridate)
format(as.Date(min(phenolSnow[50,]),origin= "1899-12-31"),"%b %d")
format(as.Date(max(phenolSnow[50,]),origin= "1899-12-31"),"%b %d")
format(as.Date(median(as.numeric(phenolSnow[50,])),origin= "1899-12-31"),"%b %d")

###Snow off dates summary and table for Appendix 1
phenolSub<-rbind(phenolSnow[5,],phenolSnow[50,],phenolSnow[95,])

format(as.Date(as.numeric(phenolSnow[50,]),origin= "1899-12-31"),"%b %d")

names<-colnames(phenolSub)

t<-1
for(i in 1:length(names)){
  table<-format(as.Date(phenolSub[,i],origin= "1899-12-31"),"%b %d")
  Dates2<-paste(table[2]," (", table[1], ", ",table[3],")",sep="")
  
  if (t==1){
    NewData<-Dates2
  }else{
    NewData<-c(NewData,Dates2)
  }
  t<-t+1
}

New<-cbind(names,NewData)

Buch<-c(New[1:6,2],"-")
Grey<-c(New[8:13,2],"-")
Lapo<-c(New[15:20,2],"-")
MidR<-c("-","-","-",New[25:28,2])
Tops<-c(New[29:33,2],"-","-")

SnowData<-data.frame(Buch,Grey,Lapo,MidR,Tops)
rownames(SnowData)<-c("2007","2008","2009","2010","2011","2012","2013")
colnames(SnowData)<-c("Buchans","Grey River","Lapoile","Middle Ridge","Topsails")

SnowData

write.csv(SnowData,"Output/Tables/SnowOffDatesByHerd_March11th.csv")

## Read back in
phenolGreen<-readRDS("Output/Phenology/GreenUpDates_March11th.RDS")

### Dates of GreenUp:

format(as.Date(min(phenolGreen[50,]),origin= "1899-12-31"),"%b %d")
format(as.Date(max(phenolGreen[50,]),origin= "1899-12-31"),"%b %d")
format(as.Date(median(as.numeric(phenolGreen[50,])),origin= "1899-12-31"),"%b %d")

###Green up dates summary and table for Appendix 1
phenolSub<-rbind(phenolGreen[5,],phenolGreen[50,],phenolGreen[95,])

format(as.Date(as.numeric(phenolGreen[50,]),origin= "1899-12-31"),"%b %d")

names<-colnames(phenolSub)

t<-1
for(i in 1:length(names)){
  table<-format(as.Date(phenolSub[,i],origin= "1899-12-31"),"%b %d")
  Dates2<-paste(table[2]," (", table[1], ", ",table[3],")",sep="")
  
  if (t==1){
    NewData<-Dates2
  }else{
    NewData<-c(NewData,Dates2)
  }
  t<-t+1
}

New<-cbind(names,NewData)

Buch<-c(New[1:6,2],"-")
Grey<-c(New[8:13,2],"-")
Lapo<-c(New[15:20,2],"-")
MidR<-c("-","-","-",New[25:28,2])
Tops<-c(New[29:33,2],"-","-")

GreenData<-data.frame(Buch,Grey,Lapo,MidR,Tops)
rownames(GreenData)<-c("2007","2008","2009","2010","2011","2012","2013")
colnames(GreenData)<-c("Buchans","Grey River","Lapoile","Middle Ridge","Topsails")

GreenData

write.csv(GreenData,"Output/Tables/GreenUpDatesByHerd_March11th.csv")



##########  Part II - plots of snow melt and green up by herd ##############

## Small dataframe to iterate over. Need to add extra lines when "Pops" increases
is<-seq(from=0,to=length(unique(data$year))*length(unique(data$Herd))-1,by=length(unique(data$year)))
#is<-c(0,7,14,21,28)
list<-data.frame(Pops,is)


## Plotting the spring phenology curves
pdf("Graphics/PhenolSnowPlots.pdf",width=6,height=6)

width<-1.5

for(i in seq(1,5,by=1)){
  plot(seq(1,99,by=1)~phenolSnow[,list[i,2]+1],
       type="l",
       lwd=width,
       xlim=c(min(phenolSnow[,(list[i,2]+1):(list[i,2]+7)]),
              max(phenolSnow[,(list[i,2]+1):(list[i,2]+7)])),
       ylim=c(0,100),
       ylab="Percent pixels snow-free",xlab="Date", main=list[i,1],xaxt="n")
  axis(1,at=c(0,32,61,92,122,153,183,214,245,275,306,336,365),labels=NA)
  months2<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months2,tck=0,padj=-1)
  abline(h=50,lty=2)
  text("Median/timing of snow melt",y=48,
       x=min(phenolSnow[,(list[i,2]+1):(list[i,2]+7)])+
         (0.8*(max(phenolSnow[,(list[i,2]+1):(list[i,2]+7)])-min(phenolSnow[,(list[i,2]+1):(list[i,2]+7)]))),cex=0.7)
  lines(seq(1,99,by=1)~phenolSnow[,list[i,2]+2],lwd=width,col="green")
  lines(seq(1,99,by=1)~phenolSnow[,list[i,2]+3],lwd=width,col="blue")
  lines(seq(1,99,by=1)~phenolSnow[,list[i,2]+4],lwd=width,col="red")
  lines(seq(1,99,by=1)~phenolSnow[,list[i,2]+5],lwd=width,col="orange")
  lines(seq(1,99,by=1)~phenolSnow[,list[i,2]+6],lwd=width,col="purple")
  lines(seq(1,99,by=1)~phenolSnow[,list[i,2]+7],lwd=width,col="cyan")
  legend(c("2007","2008","2009","2010","2011","2012","2013"),lty=1,lwd=1.5,
         col=c("black","green","blue","red","orange","purple","cyan"),
         x=min(phenolSnow[,(list[i,2]+1):(list[i,2]+7)])+(0.02*(max(phenolSnow[,(list[i,2]+1):(list[i,2]+7)])-
                                                                  min(phenolSnow[,(list[i,2]+1):(list[i,2]+7)]))),
         y=102, cex=0.6)
}

dev.off()


## Plotting the spring phenology curves
pdf("Graphics/PhenolGreenPlots.pdf",width=6,height=6)

width<-1.5

for(i in seq(1,5,by=1)){
  plot(seq(1,99,by=1)~phenolGreen[,list[i,2]+1],
       type="l",
       lwd=width,
       xlim=c(min(phenolGreen[,(list[i,2]+1):(list[i,2]+7)]),
              max(phenolGreen[,(list[i,2]+1):(list[i,2]+7)])),
       ylim=c(0,100),
       ylab="Percent pixels snow-free",xlab="Date", main=list[i,1],xaxt="n")
  axis(1,at=c(0,32,61,92,122,153,183,214,245,275,306,336,365),labels=NA)
  months2<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months2,tck=0,padj=-1)
  abline(h=50,lty=2)
  text("Median/timing of green-up",y=48,
       x=min(phenolGreen[,(list[i,2]+1):(list[i,2]+7)])+
         (0.8*(max(phenolGreen[,(list[i,2]+1):(list[i,2]+7)])-min(phenolGreen[,(list[i,2]+1):(list[i,2]+7)]))),cex=0.7)
  lines(seq(1,99,by=1)~phenolGreen[,list[i,2]+2],lwd=width,col="green")
  lines(seq(1,99,by=1)~phenolGreen[,list[i,2]+3],lwd=width,col="blue")
  lines(seq(1,99,by=1)~phenolGreen[,list[i,2]+4],lwd=width,col="red")
  lines(seq(1,99,by=1)~phenolGreen[,list[i,2]+5],lwd=width,col="orange")
  lines(seq(1,99,by=1)~phenolGreen[,list[i,2]+6],lwd=width,col="purple")
  lines(seq(1,99,by=1)~phenolGreen[,list[i,2]+7],lwd=width,col="cyan")
  legend(c("2007","2008","2009","2010","2011","2012","2013"),lty=1,lwd=1.5,
         col=c("black","green","blue","red","orange","purple","cyan"),
         x=min(phenolGreen[,(list[i,2]+1):(list[i,2]+7)])+(0.02*(max(phenolGreen[,(list[i,2]+1):(list[i,2]+7)])-
                                                                   min(phenolGreen[,(list[i,2]+1):(list[i,2]+7)]))),
         y=102, cex=0.6)
}

dev.off()


########## Simpler method - just to get median date

probs<-0.5
t<-1
for(j in Pops){
  subData<-subset(data,Herd==j)
  pts<-SpatialPoints(cbind(subData$x,subData$y),
                     proj4string=CRS("+proj=utm +zone=21 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  MCP<-mcp(pts,percent=99)
  MCPrp<-spTransform(MCP,CRSobj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  w<-4
  for(i in years){
    NDSI<-raster("Inputs/Landcover/NDSI-snow-off.tif", band = w)
    NDSI2<-mask(NDSI,MCPrp)
    NDSI3<-crop(NDSI2,MCPrp)
    IRG<-raster(eval(paste(text="Inputs/Landcover/PeakIRG/PeakIRG",i,".tif",sep="")))
    IRG2<-mask(IRG,MCP)
    IRG3<-crop(IRG2,MCP)
    QuantilesSnow<-quantile(NDSI3, probs=probs)
    QuantilesGreen<-quantile(IRG3, probs=probs)
    QuantSnow<-data.frame(paste(j,i,sep="_"),QuantilesSnow)
    QuantGreen<-data.frame(paste(j,i,sep="_"),QuantilesGreen)
    if(t==1){
      phenol2Snow<-QuantSnow
      phenol2Green<-QuantGreen
    } else {
      phenol2Snow<-rbind(phenol2Snow,QuantSnow)
      phenol2Green<-rbind(phenol2Green,QuantGreen)
    }
    t<-t+1
    w<-w+3
    print(i)
    print(j)
  }
}

phenol2Snow
phenol2Green

rownames(phenol2Snow)<-NULL
colnames(phenol2Snow)<-c("Pop_year","SpringDay")

rownames(phenol2Green)<-NULL
colnames(phenol2Green)<-c("Pop_year","SpringDay")

phenol2Snow
phenol2Green

phenol2<-data.frame(phenol2Snow$Pop_year,phenol2Snow$SpringDay,phenol2Green$SpringDay)

colnames(phenol2)<-c("Pop_year","SnowOffDate","GreenUpDate")

### Quick plot of Snow Off vs Green-up

plot(phenol2$GreenUpDate~phenol2$SnowOffDate)
abline(lm(phenol2$GreenUpDate~phenol2$SnowOffDate))
summary(lm(phenol2$GreenUpDate~phenol2$SnowOffDate))

saveRDS(phenol2, "Output/Phenology/medianDates_March11th.RDS")

data$Pop_year<-paste(data$Herd,data$year,sep="_")


### Merge together - if need all points with phenology, export this!!
dataPhenol<-merge(data,phenol2)

### But we don't, so reduce to one observation per IDyear, remove un-necessary columns
BRN<-dataPhenol[!duplicated(dataPhenol$IDyear),]

BRN$Pop_year<-NULL
BRN$Time<-NULL
BRN$x<-NULL
BRN$y<-NULL
BRN$burst<-NULL
BRN$JDateTime<-NULL
BRN$JDay<-NULL
BRN$Pop_year<-NULL
BRN$EndCalving<-NULL

head(BRN)

saveRDS(BRN,"Output/DataForBNRMarch11th.RDS")


########################   END  #################################


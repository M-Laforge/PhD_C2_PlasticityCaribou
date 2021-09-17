############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn M. R. Webber, E Vander Wal #################################################

############# August 12th, 2020 ############################################################################
############# Updated January 26th, 2020 ###################################################################
############# Cleaned + Updated March 12th, 2021 ###########################################################

############# Script 4: Making BRNs part 1: candidate models ###############################################

############################################################################################################
############################################################################################################


### Packages ----
libs <- c('data.table', 
          'lme4','MCMCglmm',
          'tidyr', 'sqldf', 'gridExtra',
          'ggplot2', 'lmtest','dplyr','standardize')
lapply(libs, require, character.only = TRUE)

### Load in the data
BRN<-readRDS("Output/DataForBNRMarch11th.RDS")

head(BRN)
unique(BRN$ID)

## Some renaming to match analyses
colnames(BRN)[6]<-"MigStart"
colnames(BRN)[7]<-"MigEnd"
colnames(BRN)[9]<-"Surv"
colnames(BRN)[12]<-"SnowOff"
colnames(BRN)[13]<-"GreenUp"
colnames(BRN)[3]<-"Population"

## Add unscaled versions before overwriting with scaled versions
BRN$MigEndus<-BRN$MigEnd
BRN$MigStartus<-BRN$MigStart


## Scaling: variables scaled "by herd"

BRN$SnowOff<-scale_by(SnowOff~Population, BRN)
BRN$GreenUp<-scale_by(GreenUp~Population, BRN)
BRN$MigEnd<-scale_by(MigEnd~Population, BRN)
BRN$Parturition<-scale_by(CalfDateHybrid~Population, BRN)

## Sample size:

length(unique(BRN$ID))  ## 92 animals
nrow(BRN)  ## 212 animal-years

## Set the parameters for the BRNs

iter<-420000
burn<-20000
th<-100

####### Model 1: Null


####### QUINN: Can you check that the priors make sense? I pretty well copied the same format as for AmNat, so hopefully they're good to go





prior1 <- list(R=list(V=diag(10), nu=3))  ## I think I used this simpler version becasue the one above wasn't working

mcmc1MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population,
                         rcov =~ idh(trait:Population):units,
                         family = c("gaussian","gaussian"),
                         prior = prior1,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 2: Herd differences, no plasticity

prior2 <- list(R=list(V=diag(10), nu=3),
               G=list(G1=list(V=diag(2), nu=3,
                              alpha.V=diag(var(BRN$MigEndus),2,2))))

mcmc2MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population,
                         random =~ idh(trait):Population,
                         rcov =~ idh(trait:Population):units,   #### Should this be in there even if fitting random effects for Population?
                         family = c("gaussian","gaussian"),
                         prior = prior2,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 3: Individual differences, no plasticity (random intercept only)

mcmc3MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population,
                         random =~ idh(trait):ID,
                         rcov =~ idh(trait:Population):units,
                         family = c("gaussian","gaussian"),
                         prior = prior2,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 4: Fixed Effect only (no individual differences, but overall plasticity)

mcmc4MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population
                         +trait:SnowOff,
                         rcov =~ idh(trait:Population):units,
                         family = c("gaussian","gaussian"),
                         prior = prior1,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 5: Fixed Effect + overall plasticity, herd differences

mcmc5MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population
                         +trait:SnowOff,
                         rcov =~ idh(trait:Population):units,
                         random =~ idh(trait):Population,
                         family = c("gaussian","gaussian"),
                         prior = prior2,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 6:Individual differences, overall plasticity 
#               (random intercept, fixed effects for spring timing)

mcmc6MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population
                         +trait:SnowOff,
                         rcov =~ idh(trait:Population):units,
                         random =~ idh(trait):ID,
                         family = c("gaussian","gaussian"),
                         prior = prior2,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 7:I X E population-level
#               (random intercept, random slopes by herd)

prior3 <- list(R=list(V=diag(10), nu=4),
               G=list(G1=list(V=diag(4), nu=4,
                              alpha.V=diag(var(BRN$MigEndus),4,4))))

mcmc7MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population
                         +trait:SnowOff,
                         random =~ us(trait + SnowOff:trait):Population,
                         rcov =~ idh(trait:Population):units,   #### Should this be in there even if fitting random effects for Population?
                         family = c("gaussian","gaussian"),
                         prior = prior3,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 8:Individual differences, individual-level plasticity 
#               (I X E interaction, random slopes by individual)

mcmc8MigPart <- MCMCglmm(cbind(MigEnd, Parturition) ~ trait-1+trait:Population
                         +trait:SnowOff,
                         rcov =~ idh(trait:Population):units,
                         random =~ us(trait + SnowOff:trait):ID,
                         family = c("gaussian","gaussian"),
                         prior = prior3,
                         nitt=iter,
                         burnin=burn,
                         thin=th,
                         verbose = TRUE,
                         data = BRN,
                         pr=T,saveX = TRUE,saveZ = TRUE)


dDICsMigPart<-c(mcmc1MigPart$DIC,mcmc2MigPart$DIC,mcmc3MigPart$DIC,mcmc4MigPart$DIC,mcmc5MigPart$DIC,mcmc6MigPart$DIC,mcmc7MigPart$DIC,mcmc8MigPart$DIC)-
  min(mcmc1MigPart$DIC,mcmc2MigPart$DIC,mcmc3MigPart$DIC,mcmc4MigPart$DIC,mcmc5MigPart$DIC,mcmc6MigPart$DIC,mcmc7MigPart$DIC,mcmc8MigPart$DIC)

dDICsMigPart

summary(mcmc8MigPart)
#### Save the models

saveRDS(mcmc1MigPart, "Output/BRN_models/March12Hybrid/mcmc1MigPart.RDS")
saveRDS(mcmc2MigPart, "Output/BRN_models/March12Hybrid/mcmc2MigPart.RDS")
saveRDS(mcmc3MigPart, "Output/BRN_models/March12Hybrid/mcmc3MigPart.RDS")
saveRDS(mcmc4MigPart, "Output/BRN_models/March12Hybrid/mcmc4MigPart.RDS")
saveRDS(mcmc5MigPart, "Output/BRN_models/March12Hybrid/mcmc5MigPart.RDS")
saveRDS(mcmc6MigPart, "Output/BRN_models/March12Hybrid/mcmc6MigPart.RDS")
saveRDS(mcmc7MigPart, "Output/BRN_models/March12Hybrid/mcmc7MigPart.RDS")
saveRDS(mcmc8MigPart, "Output/BRN_models/March12Hybrid/mcmc8MigPart.RDS")


#### Part 2: Parturition and survival

####### Model 1: Null

prior1 <- list(R=list(V=diag(10), nu=3))

mcmc1PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","categorical"),
                          prior = prior1,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 2: Herd differences, no plasticity

prior2 <- list(R=list(V=diag(10), nu=3),
               G=list(G1=list(V=diag(2), nu=3,
                              alpha.V=diag(var(BRN$MigEndus),2,2))))

mcmc2PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population,
                          random =~ idh(trait):Population,
                          rcov =~ idh(trait:Population):units,   #### Should this be in there even if fitting random effects for Population?
                          family = c("gaussian","categorical"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 3: Individual differences, no plasticity (random intercept only)

mcmc3PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population,
                          random =~ idh(trait):ID,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","categorical"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)




######## Model 4: Fixed Effect only (no individual differences, but overall plasticity)

mcmc4PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population
                          +trait:GreenUp,
                          rcov =~ idh(trait:Population):units,
                          family = c("gaussian","categorical"),
                          prior = prior1,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 5: Fixed Effect + overall plasticity, herd differences

mcmc5PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population
                          +trait:GreenUp,
                          rcov =~ idh(trait:Population):units,
                          random =~ idh(trait):Population,
                          family = c("gaussian","categorical"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 6:Individual differences, overall plasticity 
#               (random intercept, fixed effects for spring timing)

mcmc6PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population
                          +trait:GreenUp,
                          rcov =~ idh(trait:Population):units,
                          random =~ idh(trait):ID,
                          family = c("gaussian","categorical"),
                          prior = prior2,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)


######## Model 7:I X E population-level
#               (random intercept, random slopes by herd)

prior3 <- list(R=list(V=diag(10), nu=4),
               G=list(G1=list(V=diag(4), nu=4,
                              alpha.V=diag(var(BRN$MigEndus),4,4))))

mcmc7PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population
                          +trait:GreenUp,
                          random =~ us(trait + GreenUp:trait):Population,
                          rcov =~ idh(trait:Population):units,   #### Should this be in there even if fitting random effects for Population?
                          family = c("gaussian","categorical"),
                          prior = prior3,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)



######## Model 8:Individual differences, individual-level plasticity 
#               (I X E interaction, random slopes by individual)

mcmc8PartSurv <- MCMCglmm(cbind(Parturition, Surv) ~ trait-1+trait:Population
                          +trait:GreenUp,
                          rcov =~ idh(trait:Population):units,
                          random =~ us(trait + GreenUp:trait):ID,
                          family = c("gaussian","categorical"),
                          prior = prior3,
                          nitt=iter,
                          burnin=burn,
                          thin=th,
                          verbose = TRUE,
                          data = BRN,
                          pr=T,saveX = TRUE,saveZ = TRUE)


dDICsPartSurv<-c(mcmc1PartSurv$DIC,mcmc2PartSurv$DIC,mcmc3PartSurv$DIC,mcmc4PartSurv$DIC,
                 mcmc5PartSurv$DIC,mcmc6PartSurv$DIC,mcmc7PartSurv$DIC,mcmc8PartSurv$DIC)-
  min(mcmc1PartSurv$DIC,mcmc2PartSurv$DIC,mcmc3PartSurv$DIC,mcmc4PartSurv$DIC,mcmc5PartSurv$DIC,
      mcmc6PartSurv$DIC,mcmc7PartSurv$DIC,mcmc8PartSurv$DIC)

dDICsPartSurv

summary(mcmc8PartSurv)
#### Save the models

saveRDS(mcmc1PartSurv, "Output/BRN_models/March12Hybrid/mcmc1PartSurv_Green.RDS")
saveRDS(mcmc2PartSurv, "Output/BRN_models/March12Hybrid/mcmc2PartSurv_Green.RDS")
saveRDS(mcmc3PartSurv, "Output/BRN_models/March12Hybrid/mcmc3PartSurv_Green.RDS")
saveRDS(mcmc4PartSurv, "Output/BRN_models/March12Hybrid/mcmc4PartSurv_Green.RDS")
saveRDS(mcmc5PartSurv, "Output/BRN_models/March12Hybrid/mcmc5PartSurv_Green.RDS")
saveRDS(mcmc6PartSurv, "Output/BRN_models/March12Hybrid/mcmc6PartSurv_Green.RDS")
saveRDS(mcmc7PartSurv, "Output/BRN_models/March12Hybrid/mcmc7PartSurv_Green.RDS")
saveRDS(mcmc8PartSurv, "Output/BRN_models/March12Hybrid/mcmc8PartSurv_Green.RDS")


descriptions<-c("Null","Herd differences, no plasticity","Individual differences, no plasticity",
                "Overall plasticity, no population/individual differences","Overall plasticity, population differences",
                "Overall plasticity, individual differences","Random slopes, population X environment interaction",
                "Random slopes, individual X environment interaction")
fixed<-c("Population","Population","Population","Population + Timing of spring","Population + Timing of spring",
         "Population + Timing of spring","Population + Timing of spring","Population + Timing of spring")
random<-c("None","Population","Individual","None","Population","Individual","Population X Timing of spring","Individual X Timing of spring")

summaryData<-data.frame(descriptions,fixed,random,round(dDICsMigPart,1),round(dDICsPartSurv,1))

colnames(summaryData)<-c("Model","Fixed effects","Random effects","dDIC, migration and parturition","dDIC, parturition and survival")

summaryData

write.csv(summaryData, "Output/BRN_models/BirthDate/dDICsMarch12Hybrid.csv")

###############################   END #############################################



############################################################################################################
############################################################################################################

############# Individual differences migration timing green-up selection ###################################

############# MP Laforge, Quinn MR Webber, E Vander Wal #################################################

############# August 12th, 2020 ############################################################################
############# Updated January 26th, 2020 ###################################################################
############# Cleaned + Updated March 12th, 2021 ###########################################################

############# Script 5: Making BRNs part 2: Model summaries, Repeatabilities, covariances, BRN plots, etc ##

############################################################################################################
############################################################################################################

## Packages
library(MCMCglmm)
library(tibble)

## Read in the top models
mcmcMigPart<-readRDS("Output/BRN_models/March12Hybrid/mcmc8MigPart.RDS")
mcmcPartSurv<-readRDS("Output/BRN_models/March12Hybrid/mcmc8PartSurv_Green.RDS")

##### Extract summary table/fixed effects
summary(mcmcMigPart)
summary(mcmcPartSurv)

namesMigPart<-c("Arrival timing","Parturition timing","Snowmelt","Grey River",
                "Lapoile","Middle Ridge","Topsails")

MigPartMeans=round(as.matrix(colMeans(mcmcMigPart$Sol[,1:12])),3)
MigPartLow<-data.frame(round(HPDinterval(mcmcMigPart$Sol, 0.95)[1:12,1],3))
MigPartHigh<-data.frame(round(HPDinterval(mcmcMigPart$Sol, 0.95)[1:12,2],3))

MigEndEsts<-c("",MigPartMeans[2],MigPartMeans[11],MigPartMeans[3],
              MigPartMeans[5],MigPartMeans[7],MigPartMeans[9])
PartEsts<-c(MigPartMeans[1],"",MigPartMeans[12],MigPartMeans[4],
            MigPartMeans[6],MigPartMeans[8],MigPartMeans[10])
MigEndLow<-c("",MigPartLow[2,],MigPartLow[11,],MigPartLow[3,],MigPartLow[5,],
             MigPartLow[7,],MigPartLow[9,])
PartLow<-c(MigPartLow[1,],"",MigPartLow[12,],MigPartLow[4,],
           MigPartLow[6,],MigPartLow[8,],MigPartLow[10,])
MigEndHigh<-c("",MigPartHigh[2,],MigPartHigh[11,],MigPartHigh[3,],MigPartHigh[5,],
              MigPartHigh[7,],MigPartHigh[9,])
PartHigh<-c(MigPartHigh[1,],"",MigPartHigh[12,],MigPartHigh[4,],
            MigPartHigh[6,],MigPartHigh[8,],MigPartHigh[10,])

MigPartMigEndCI<-paste(MigEndEsts," (", MigEndLow, ", ",MigEndHigh,")",sep="")
MigPartPartCI<-paste(PartEsts," (", PartLow, ", ",PartHigh,")",sep="")


namesPartSurv<-c("Parturition timing","Calf survival","Green-up","Grey River",
                 "Lapoile","Middle Ridge","Topsails")

PartSurvMeans=round(as.matrix(colMeans(mcmcPartSurv$Sol[,1:12])),3)
PartSurvLow<-data.frame(round(HPDinterval(mcmcPartSurv$Sol, 0.95)[1:12,1],3))
PartSurvHigh<-data.frame(round(HPDinterval(mcmcPartSurv$Sol, 0.95)[1:12,2],3))

PartEstsGU<-c("",PartSurvMeans[2],PartSurvMeans[11],PartSurvMeans[3],
              PartSurvMeans[5],PartSurvMeans[7],PartSurvMeans[9])
SurvEsts<-c(PartSurvMeans[1],"",PartSurvMeans[12],PartSurvMeans[4],
            PartSurvMeans[6],PartSurvMeans[8],PartSurvMeans[10])
PartLowGU<-c("",PartSurvLow[2,],PartSurvLow[11,],PartSurvLow[3,],PartSurvLow[5,],
             PartSurvLow[7,],PartSurvLow[9,])
SurvLow<-c(PartSurvLow[1,],"",PartSurvLow[12,],PartSurvLow[4,],
           PartSurvLow[6,],PartSurvLow[8,],PartSurvLow[10,])
PartHighGU<-c("",PartSurvHigh[2,],PartSurvHigh[11,],PartSurvHigh[3,],PartSurvHigh[5,],
              PartSurvHigh[7,],PartSurvHigh[9,])
SurvHigh<-c(PartSurvHigh[1,],"",PartSurvHigh[12,],PartSurvHigh[4,],
            PartSurvHigh[6,],PartSurvHigh[8,],PartSurvHigh[10,])

PartSurvPartCI<-paste(PartEstsGU," (", PartLowGU, ", ",PartHighGU,")",sep="")
PartSurvSurvCI<-paste(SurvEsts," (", SurvLow, ", ",SurvHigh,")",sep="")

AllNames<-c("",namesMigPart,"",namesPartSurv)
col2<-c("Snowmelt model - arrival timing", MigPartMigEndCI,
        "Green-up model - parturition timing", PartSurvPartCI)
col3<-c("Snowmelt model - parturition timing", MigPartPartCI,
        "Green-up model - calf survival", PartSurvSurvCI)

Table<-data.frame(AllNames,col2,col3)

Table

write.csv(Table,"Results/CandidateModels_March12Hybrid.csv")





##### Migration and parturition


##################  CORRELATIONS ##################

## CORRELATIONS between Intercept MigEnd and Intercept Parturition:
mcmc_MigEnd_Parturition_ints <- mcmcMigPart$VCV[,"traitMigEnd:traitParturition.ID"]/
  (sqrt(mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"])*
     sqrt(mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"]))

## CORRELATIONS between Slope MigEnd and Intercept MigEnd:
mcmc_MigEnd_int_slope <- mcmcMigPart$VCV[,"traitMigEnd:SnowOff:traitMigEnd.ID"]/
  (sqrt(mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"])*
     sqrt(mcmcMigPart$VCV[,"traitMigEnd:SnowOff:traitMigEnd:SnowOff.ID"]))

## CORRELATIONS between Slope Parturition and Intercept Parturition:
mcmc_Parturition_int_slope <- mcmcMigPart$VCV[,"traitParturition:SnowOff:traitParturition.ID"]/
  (sqrt(mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"])*
     sqrt(mcmcMigPart$VCV[,"traitParturition:SnowOff:traitParturition:SnowOff.ID"]))

## CORRELATIONS between Slope MigEnd and Slope Parturition:
mcmc_Parturition_MigEnd_slope <- mcmcMigPart$VCV[,"traitParturition:SnowOff:traitMigEnd:SnowOff.ID"]/
  (sqrt(mcmcMigPart$VCV[,"traitParturition:SnowOff:traitParturition:SnowOff.ID"])*
     sqrt(mcmcMigPart$VCV[,"traitMigEnd:SnowOff:traitMigEnd:SnowOff.ID"]))

## CORRELATIONS between Slope MigEnd and Int Parturition:
mcmc_Parturitionint_MigEndslope <- mcmcMigPart$VCV[,"traitParturition:traitMigEnd:SnowOff.ID"]/
  (sqrt(mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"])*
     sqrt(mcmcMigPart$VCV[,"traitMigEnd:SnowOff:traitMigEnd:SnowOff.ID"]))

## CORRELATIONS between Slope Parturition and Int MigEnd:
mcmc_Parturitionslope_MigEndint <- mcmcMigPart$VCV[,"traitMigEnd:traitParturition:SnowOff.ID"]/
  (sqrt(mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"])*
     sqrt(mcmcMigPart$VCV[,"traitParturition:SnowOff:traitParturition:SnowOff.ID"]))

perc<-0.95

mcmc_cor_MigPart <- tibble(Traits = c("Migration time intercept, Birth time intercept",
                                      "Migration time intercept, migration time slope",
                                      "Birth time intercept, birth time slope",
                                      "Migration time slope, birth time slope",
                                      "Migration time slope, birth time intercept",
                                      "birth time slope, migration time intercept"),
                           Estimate = c(median(mcmc_MigEnd_Parturition_ints),median(mcmc_MigEnd_int_slope),
                                        median(mcmc_Parturition_int_slope),median(mcmc_Parturition_MigEnd_slope),
                                        median(mcmc_Parturitionint_MigEndslope),median(mcmc_Parturitionslope_MigEndint)),
                           Lower = c(HPDinterval(mcmc_MigEnd_Parturition_ints,prob=perc)[,"lower"],
                                     HPDinterval(mcmc_MigEnd_int_slope,prob=perc)[,"lower"],
                                     HPDinterval(mcmc_Parturition_int_slope,prob=perc)[,"lower"],
                                     HPDinterval(mcmc_Parturition_MigEnd_slope,prob=perc)[,"lower"],
                                     HPDinterval(mcmc_Parturitionint_MigEndslope,prob=perc)[,"lower"],
                                     HPDinterval(mcmc_Parturitionslope_MigEndint,prob=perc)[,"lower"]),
                           Upper = c(HPDinterval(mcmc_MigEnd_Parturition_ints,prob=perc)[,"upper"],
                                     HPDinterval(mcmc_MigEnd_int_slope,prob=perc)[,"upper"],
                                     HPDinterval(mcmc_Parturition_int_slope,prob=perc)[,"upper"],
                                     HPDinterval(mcmc_Parturition_MigEnd_slope,prob=perc)[,"upper"],
                                     HPDinterval(mcmc_Parturitionint_MigEndslope,prob=perc)[,"upper"],
                                     HPDinterval(mcmc_Parturitionslope_MigEndint,prob=perc)[,"upper"]))

mcmc_cor_MigPart

write.csv(mcmc_cor_MigPart, "Results/Correlations_MigPart_March12Hybrid.csv")

######  Repeatabilities

### Snow off

## Intercept Repeatability estimates for each population (Migration timing):
rep_MigEnd_Buch <- mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"]/(
  mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"] +
    mcmcMigPart$VCV[,"traitMigEnd:PopulationBUCHANS.units"])

rep_MigEnd_Grey <- mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"]/(
  mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"] +
    mcmcMigPart$VCV[,"traitMigEnd:PopulationGREY.units"])

rep_MigEnd_Lapo <- mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"]/(
  mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"] +
    mcmcMigPart$VCV[,"traitMigEnd:PopulationLAPOILE.units"])

rep_MigEnd_MidR <- mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"]/(
  mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"] +
    mcmcMigPart$VCV[,"traitMigEnd:PopulationMIDRIDGE.units"])

rep_MigEnd_Tops <- mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"]/(
  mcmcMigPart$VCV[,"traitMigEnd:traitMigEnd.ID"] +
    mcmcMigPart$VCV[,"traitMigEnd:PopulationTOPSAILS.units"])



## Intercept Repeatability estimates for each population (Parturition):
rep_Parturition_Buch <- mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcMigPart$VCV[,"traitParturition:PopulationBUCHANS.units"])

rep_Parturition_Grey <- mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcMigPart$VCV[,"traitParturition:PopulationGREY.units"])

rep_Parturition_Lapo <- mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcMigPart$VCV[,"traitParturition:PopulationLAPOILE.units"])

rep_Parturition_MidR <- mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcMigPart$VCV[,"traitParturition:PopulationMIDRIDGE.units"])

rep_Parturition_Tops <- mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcMigPart$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcMigPart$VCV[,"traitParturition:PopulationTOPSAILS.units"])


repeats_MigPart <- tibble(Traits = c("Buchans",
                                     "Grey River",
                                     "Lapoile",
                                     "Middle Ridge",
                                     "Topsails"),
                          Estimate_MigEnd = c(median(rep_MigEnd_Buch),median(rep_MigEnd_Grey),
                                              median(rep_MigEnd_Lapo),median(rep_MigEnd_MidR),
                                              median(rep_MigEnd_Tops)),
                          Lower_MigEnd = c(HPDinterval(rep_MigEnd_Buch,prob=perc)[,"lower"],
                                           HPDinterval(rep_MigEnd_Grey,prob=perc)[,"lower"],
                                           HPDinterval(rep_MigEnd_Lapo,prob=perc)[,"lower"],
                                           HPDinterval(rep_MigEnd_MidR,prob=perc)[,"lower"],
                                           HPDinterval(rep_MigEnd_Tops,prob=perc)[,"lower"]),
                          Upper_MigEnd = c(HPDinterval(rep_MigEnd_Buch,prob=perc)[,"upper"],
                                           HPDinterval(rep_MigEnd_Grey,prob=perc)[,"upper"],
                                           HPDinterval(rep_MigEnd_Lapo,prob=perc)[,"upper"],
                                           HPDinterval(rep_MigEnd_MidR,prob=perc)[,"upper"],
                                           HPDinterval(rep_MigEnd_Tops,prob=perc)[,"upper"]),
                          Estimate_Parturition = c(median(rep_Parturition_Buch),median(rep_Parturition_Grey),
                                                   median(rep_Parturition_Lapo),median(rep_Parturition_MidR),
                                                   median(rep_Parturition_Tops)),
                          Lower_Parturition = c(HPDinterval(rep_Parturition_Buch,prob=perc)[,"lower"],
                                                HPDinterval(rep_Parturition_Grey,prob=perc)[,"lower"],
                                                HPDinterval(rep_Parturition_Lapo,prob=perc)[,"lower"],
                                                HPDinterval(rep_Parturition_MidR,prob=perc)[,"lower"],
                                                HPDinterval(rep_Parturition_Tops,prob=perc)[,"lower"]),
                          Upper_Parturition = c(HPDinterval(rep_Parturition_Buch,prob=perc)[,"upper"],
                                                HPDinterval(rep_Parturition_Grey,prob=perc)[,"upper"],
                                                HPDinterval(rep_Parturition_Lapo,prob=perc)[,"upper"],
                                                HPDinterval(rep_Parturition_MidR,prob=perc)[,"upper"],
                                                HPDinterval(rep_Parturition_Tops,prob=perc)[,"upper"]),
                          Vres_MigEnd = c(median(mcmcMigPart$VCV[,"traitMigEnd:PopulationBUCHANS.units"]),
                                          median(mcmcMigPart$VCV[,"traitMigEnd:PopulationGREY.units"]),
                                          median(mcmcMigPart$VCV[,"traitMigEnd:PopulationLAPOILE.units"]),
                                          median(mcmcMigPart$VCV[,"traitMigEnd:PopulationMIDRIDGE.units"]),
                                          median(mcmcMigPart$VCV[,"traitMigEnd:PopulationTOPSAILS.units"])),
                          Vres_Parturition = c(median(mcmcMigPart$VCV[,"traitParturition:PopulationBUCHANS.units"]),
                                               median(mcmcMigPart$VCV[,"traitParturition:PopulationGREY.units"]),
                                               median(mcmcMigPart$VCV[,"traitParturition:PopulationLAPOILE.units"]),
                                               median(mcmcMigPart$VCV[,"traitParturition:PopulationMIDRIDGE.units"]),
                                               median(mcmcMigPart$VCV[,"traitParturition:PopulationTOPSAILS.units"])))

#### Parturition and fitness

mcmcPartSurv<-readRDS("Output/BRN_models/March12Hybrid/mcmc8PartSurv_Green.RDS")
summary(mcmcPartSurv)

##################  CORRELATIONS ##################

## CORRELATIONS between Intercept Surv.1 and Intercept Parturition:
mcmc_Surv.1_Parturition_ints <- mcmcPartSurv$VCV[,"traitSurv.1:traitParturition.ID"]/
  (sqrt(mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"])*
     sqrt(mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"]))

## CORRELATIONS between Slope Surv.1 and Intercept Surv.1:
mcmc_Surv.1_int_slope <- mcmcPartSurv$VCV[,"traitSurv.1:GreenUp:traitSurv.1.ID"]/
  (sqrt(mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"])*
     sqrt(mcmcPartSurv$VCV[,"traitSurv.1:GreenUp:traitSurv.1:GreenUp.ID"]))

## CORRELATIONS between Slope Parturition and Intercept Parturition:
mcmc_Parturition_int_slope <- mcmcPartSurv$VCV[,"traitParturition:GreenUp:traitParturition.ID"]/
  (sqrt(mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"])*
     sqrt(mcmcPartSurv$VCV[,"traitParturition:GreenUp:traitParturition:GreenUp.ID"]))

## CORRELATIONS between Slope Surv.1 and Slope Parturition:
mcmc_Parturition_Surv.1_slope <- mcmcPartSurv$VCV[,"traitParturition:GreenUp:traitSurv.1:GreenUp.ID"]/
  (sqrt(mcmcPartSurv$VCV[,"traitParturition:GreenUp:traitParturition:GreenUp.ID"])*
     sqrt(mcmcPartSurv$VCV[,"traitSurv.1:GreenUp:traitSurv.1:GreenUp.ID"]))

## CORRELATIONS between Slope Surv.1 and Int Parturition:
mcmc_Parturitionint_Surv.1slope <- mcmcPartSurv$VCV[,"traitParturition:traitSurv.1:GreenUp.ID"]/
  (sqrt(mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"])*
     sqrt(mcmcPartSurv$VCV[,"traitSurv.1:GreenUp:traitSurv.1:GreenUp.ID"]))

## CORRELATIONS between Slope Parturition and Int Surv.1:
mcmc_Parturitionslope_Surv.1int <- mcmcPartSurv$VCV[,"traitSurv.1:traitParturition:GreenUp.ID"]/
  (sqrt(mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"])*
     sqrt(mcmcPartSurv$VCV[,"traitParturition:GreenUp:traitParturition:GreenUp.ID"]))

mcmc_cor_PartSurv <- tibble(Traits = c("Calf survival intercept, Birth time intercept",
                                       "Calf survival intercept, Calf survival slope",
                                       "Birth time intercept, birth time slope",
                                       "Calf survival slope, birth time slope",
                                       "Calf survival slope, birth time intercept",
                                       "Birth time slope, calf survival intercept"),
                            Estimate = c(median(mcmc_Surv.1_Parturition_ints),median(mcmc_Surv.1_int_slope),
                                         median(mcmc_Parturition_int_slope),median(mcmc_Parturition_Surv.1_slope),
                                         median(mcmc_Parturitionint_Surv.1slope),median(mcmc_Parturitionslope_Surv.1int)),
                            Lower = c(HPDinterval(mcmc_Surv.1_Parturition_ints,prob=perc)[,"lower"],
                                      HPDinterval(mcmc_Surv.1_int_slope,prob=perc)[,"lower"],
                                      HPDinterval(mcmc_Parturition_int_slope,prob=perc)[,"lower"],
                                      HPDinterval(mcmc_Parturition_Surv.1_slope,prob=perc)[,"lower"],
                                      HPDinterval(mcmc_Parturitionint_Surv.1slope,prob=perc)[,"lower"],
                                      HPDinterval(mcmc_Parturitionslope_Surv.1int,prob=perc)[,"lower"]),
                            Upper = c(HPDinterval(mcmc_Surv.1_Parturition_ints,prob=perc)[,"upper"],
                                      HPDinterval(mcmc_Surv.1_int_slope,prob=perc)[,"upper"],
                                      HPDinterval(mcmc_Parturition_int_slope,prob=perc)[,"upper"],
                                      HPDinterval(mcmc_Parturition_Surv.1_slope,prob=perc)[,"upper"],
                                      HPDinterval(mcmc_Parturitionint_Surv.1slope,prob=perc)[,"upper"],
                                      HPDinterval(mcmc_Parturitionslope_Surv.1int,prob=perc)[,"upper"]))

mcmc_cor_PartSurv

write.csv(mcmc_cor_PartSurv, "Results/CorrelationsMarch12Hybrid_PartSurv.csv")

######  Repeatabilities

### Parturition and survival model

## Intercept Repeatability estimates for each population (Survival):
rep_Surv.1_Buch <- mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"]/(
  mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"] +
    mcmcPartSurv$VCV[,"traitSurv.1:PopulationBUCHANS.units"])

rep_Surv.1_Grey <- mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"]/(
  mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"] +
    mcmcPartSurv$VCV[,"traitSurv.1:PopulationGREY.units"])

rep_Surv.1_Lapo <- mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"]/(
  mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"] +
    mcmcPartSurv$VCV[,"traitSurv.1:PopulationLAPOILE.units"])

rep_Surv.1_MidR <- mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"]/(
  mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"] +
    mcmcPartSurv$VCV[,"traitSurv.1:PopulationMIDRIDGE.units"])

rep_Surv.1_Tops <- mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"]/(
  mcmcPartSurv$VCV[,"traitSurv.1:traitSurv.1.ID"] +
    mcmcPartSurv$VCV[,"traitSurv.1:PopulationTOPSAILS.units"])



## Intercept Repeatability estimates for each population (Parturition timing):
rep_Parturition_Buch <- mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcPartSurv$VCV[,"traitParturition:PopulationBUCHANS.units"])

rep_Parturition_Grey <- mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcPartSurv$VCV[,"traitParturition:PopulationGREY.units"])

rep_Parturition_Lapo <- mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcPartSurv$VCV[,"traitParturition:PopulationLAPOILE.units"])

rep_Parturition_MidR <- mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcPartSurv$VCV[,"traitParturition:PopulationMIDRIDGE.units"])

rep_Parturition_Tops <- mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"]/(
  mcmcPartSurv$VCV[,"traitParturition:traitParturition.ID"] +
    mcmcPartSurv$VCV[,"traitParturition:PopulationTOPSAILS.units"])


repeats_PartSurv <- tibble(Traits = c("Buchans",
                                      "Grey River",
                                      "Lapoile",
                                      "Middle Ridge",
                                      "Topsails"),
                           Estimate_Surv.1 = c(median(rep_Surv.1_Buch),median(rep_Surv.1_Grey),
                                               median(rep_Surv.1_Lapo),median(rep_Surv.1_MidR),
                                               median(rep_Surv.1_Tops)),
                           Lower_Surv.1 = c(HPDinterval(rep_Surv.1_Buch,prob=perc)[,"lower"],
                                            HPDinterval(rep_Surv.1_Grey,prob=perc)[,"lower"],
                                            HPDinterval(rep_Surv.1_Lapo,prob=perc)[,"lower"],
                                            HPDinterval(rep_Surv.1_MidR,prob=perc)[,"lower"],
                                            HPDinterval(rep_Surv.1_Tops,prob=perc)[,"lower"]),
                           Upper_Surv.1 = c(HPDinterval(rep_Surv.1_Buch,prob=perc)[,"upper"],
                                            HPDinterval(rep_Surv.1_Grey,prob=perc)[,"upper"],
                                            HPDinterval(rep_Surv.1_Lapo,prob=perc)[,"upper"],
                                            HPDinterval(rep_Surv.1_MidR,prob=perc)[,"upper"],
                                            HPDinterval(rep_Surv.1_Tops,prob=perc)[,"upper"]),
                           Estimate_Parturition = c(median(rep_Parturition_Buch),median(rep_Parturition_Grey),
                                                    median(rep_Parturition_Lapo),median(rep_Parturition_MidR),
                                                    median(rep_Parturition_Tops)),
                           Lower_Parturition = c(HPDinterval(rep_Parturition_Buch,prob=perc)[,"lower"],
                                                 HPDinterval(rep_Parturition_Grey,prob=perc)[,"lower"],
                                                 HPDinterval(rep_Parturition_Lapo,prob=perc)[,"lower"],
                                                 HPDinterval(rep_Parturition_MidR,prob=perc)[,"lower"],
                                                 HPDinterval(rep_Parturition_Tops,prob=perc)[,"lower"]),
                           Upper_Parturition = c(HPDinterval(rep_Parturition_Buch,prob=perc)[,"upper"],
                                                 HPDinterval(rep_Parturition_Grey,prob=perc)[,"upper"],
                                                 HPDinterval(rep_Parturition_Lapo,prob=perc)[,"upper"],
                                                 HPDinterval(rep_Parturition_MidR,prob=perc)[,"upper"],
                                                 HPDinterval(rep_Parturition_Tops,prob=perc)[,"upper"]),
                           Vres_Surv.1 = c(median(mcmcPartSurv$VCV[,"traitSurv.1:PopulationBUCHANS.units"]),
                                           median(mcmcPartSurv$VCV[,"traitSurv.1:PopulationGREY.units"]),
                                           median(mcmcPartSurv$VCV[,"traitSurv.1:PopulationLAPOILE.units"]),
                                           median(mcmcPartSurv$VCV[,"traitSurv.1:PopulationMIDRIDGE.units"]),
                                           median(mcmcPartSurv$VCV[,"traitSurv.1:PopulationTOPSAILS.units"])),
                           Vres_Parturition = c(median(mcmcPartSurv$VCV[,"traitParturition:PopulationBUCHANS.units"]),
                                                median(mcmcPartSurv$VCV[,"traitParturition:PopulationGREY.units"]),
                                                median(mcmcPartSurv$VCV[,"traitParturition:PopulationLAPOILE.units"]),
                                                median(mcmcPartSurv$VCV[,"traitParturition:PopulationMIDRIDGE.units"]),
                                                median(mcmcPartSurv$VCV[,"traitParturition:PopulationTOPSAILS.units"])))



### average repeats across herds

mean(repeats_MigPart$Estimate_MigEnd)
sd(repeats_MigPart$Estimate_MigEnd)

mean(repeats_MigPart$Estimate_Parturition)
sd(repeats_MigPart$Estimate_Parturition)

mean(repeats_PartSurv$Estimate_Surv.1)######## Not presenting this
sd(repeats_PartSurv$Estimate_Surv.1)  ######## Not presenting this

mean(repeats_PartSurv$Estimate_Parturition)
sd(repeats_PartSurv$Estimate_Parturition)

#### Making a plot of the repeatability estimates (Figure 3)

library(wesanderson)

png("Figures/repeats_March12Hybrid_noSurvrep.png",width=5,height=4.5,units="in",res=600)
#pdf("Figures/repeats_March12Hybrid_noSurvrep.pdf",width=4.5,height=4.33)
cols<-wes_palette("Darjeeling1", n =5)
cols[1]
c1<-cols[1]
c2<-cols[2]
c3<-cols[3]
### Set up offsets:
m2<-1.66
m1<-5.66
p2<-1.33
p1<-5.33
par(mfrow=c(1,1),mar=c(3.3,6,1,1))

### Migration end
plot(seq(from=m1, to=m2,by=-1)~repeats_MigPart$Estimate_MigEnd,ylim=c(0.8,6),xlim=c(0,0.7),yaxt='n',
     ylab=NA,pch=20,cex=2.5,col=c1,xlab=NA)
arrows(x0=repeats_MigPart$Lower_MigEnd,x1=repeats_MigPart$Upper_MigEnd,y0=seq(from=m1, to=m2,by=-1),
       y1=seq(from=m1, to=m2,by=-1),angle=90,code=3,length=0.1,col=c1,lwd=3)
### Calving date
points(seq(from=p1, to=p2,by=-1)~repeats_MigPart$Estimate_Parturition,ylim=c(1,9),xlim=c(0,1),yaxt='n',
       ylab=NA,pch=20,cex=2.5,col=c2)
arrows(x0=repeats_MigPart$Lower_Parturition,x1=repeats_MigPart$Upper_Parturition,y0=seq(from=p1, to=p2,by=-1),
       y1=seq(from=p1, to=p2,by=-1),angle=90,code=3,length=0.1,col=c2,lwd=3)
abline(h=seq(from=1,to=6),lty=2,lwd=0.5)
abline(v=c(0.3,0.5),lty=3,lwd=0.5)
axis(2, at=seq(from=1,to=6,by=1),labels=NA)
axis(2, at=seq(from=5.5,to=1.5,by=-1),labels=repeats_PartSurv$Traits, tick=F,las=2)
text("Low",x=0.2,y=0.85)
text("Moderate",x=0.4,y=0.85)
text("High",x=0.62,y=0.85)
mtext("Repeatability ± 95% credible interval", side=1,padj=3.5)
legend(legend=c("Timing of arrival",
                "Timing of birth"),pch=20,pt.cex = 1.5,
       col=cols[1:2],x=0.455,y=4.6,cex=0.75,box.col="transparent",bg="transparent")

dev.off()



pdf("TalkFigs/repeats_ParturitionOnly.pdf",width=4.5,height=3.33)
cols<-wes_palette("Darjeeling1", n =5)
cols[1]
#c1<-cols[1]
c2<-cols[2]
#c3<-cols[3]
### Set up offsets:
p2<-1.5
p1<-5.5
m2<-1.1
m1<-5.1
par(mfrow=c(1,1),mar=c(3.3,6,1,1))

### Migration end
plot(seq(from=m1, to=m2,by=-1)~repeats_MigPart$Estimate_MigEnd,ylim=c(0.8,6),xlim=c(0,0.7),yaxt='n',
     ylab=NA,pch=20,cex=2.5,xlab=NA,col="white")
#arrows(x0=repeats_MigPart$Lower_MigEnd,x1=repeats_MigPart$Upper_MigEnd,y0=seq(from=m1, to=m2,by=-1),
#       y1=seq(from=m1, to=m2,by=-1),angle=90,code=3,length=0.1,col=c1,lwd=3)
### Calving date
points(seq(from=p1, to=p2,by=-1)~repeats_MigPart$Estimate_Parturition,ylim=c(1,9),xlim=c(0,1),yaxt='n',
       ylab=NA,pch=20,cex=2.5,col=c2)
arrows(x0=repeats_MigPart$Lower_Parturition,x1=repeats_MigPart$Upper_Parturition,y0=seq(from=p1, to=p2,by=-1),
       y1=seq(from=p1, to=p2,by=-1),angle=90,code=3,length=0.1,col=c2,lwd=3)
abline(h=seq(from=1,to=6),lty=2,lwd=0.5)
abline(v=c(0.3,0.5),lty=3,lwd=0.5)
axis(2, at=seq(from=1,to=6,by=1),labels=NA)
axis(2, at=seq(from=5.5,to=1.5,by=-1),labels=repeats_PartSurv$Traits, tick=F,las=2)
text("Low",x=0.2,y=0.85)
text("Moderate",x=0.4,y=0.85)
text("High",x=0.62,y=0.85)
mtext("Repeatability of parturition ± 95% CI", side=1,padj=3.5)


dev.off()



############ Figure 4 - Select correlations from both models

##### Set up the data:

#### Model 1:

### Extract the coefficients
Means.MP=as.matrix(colMeans(mcmcMigPart$Sol))
nrow(Means.MP)

### Terms before random effects:
num<-12

n.rand<-(length(Means.MP)-12)/4

### Extracting the random slopes and intercepts

MigInt.ID.MP<-Means.MP[(num+1):(num+n.rand)]
PartInt.ID.MP<-Means.MP[(num+1+n.rand):(num+(n.rand*2))]
MigSlope.ID.MP<-Means.MP[(num+1+(n.rand*2)):(num+(n.rand*3))]
PartSlope.ID.MP<-Means.MP[(num+1+(n.rand*3)):(num+(n.rand*4))]
IDs<-substr(rownames(Means.MP)[(num+1):(num+n.rand)],16,24)

### Individual intercepts and slopes

IntSlopes.MP<-data.frame(IDs,MigInt.ID.MP,PartInt.ID.MP,MigSlope.ID.MP,PartSlope.ID.MP)
BRN<-readRDS("Output/DataForBNRMarch11th.RDS")
BRNid<-BRN[!duplicated(BRN$ID),]
IntSlMigPart<-merge(IntSlopes.MP,BRN[,2:3],by.y="ID",by.x="IDs")
head(IntSlMigPart)
corsMigPart<-data.frame(mcmc_cor_MigPart)

library(wesanderson)

IntSlMigPart$col<-wes_palette("Darjeeling1", n=5)[IntSlMigPart$Herd]

#### Model 2:

### Extract the coefficients
Means.PS=as.matrix(colMeans(mcmcPartSurv$Sol))
nrow(Means.PS)

### Terms before random effects:
num<-12
n.rand<-(length(Means.PS)-12)/4

### Individual intercepts and slopes
PartInt.ID.PS<-Means.PS[(num+1):(num+n.rand)]
SurvInt.ID.PS<-Means.PS[(num+1+n.rand):(num+(n.rand*2))]
PartSlope.ID.PS<-Means.PS[(num+1+(n.rand*2)):(num+(n.rand*3))]
SurvSlope.ID.PS<-Means.PS[(num+1+(n.rand*3)):(num+(n.rand*4))]
IDs<-substr(rownames(Means.PS)[(num+1):(num+n.rand)],21,29)

IntSlopes.PS<-data.frame(IDs,PartInt.ID.PS,SurvInt.ID.PS,PartSlope.ID.PS,SurvSlope.ID.PS)
BRNid<-BRN[!duplicated(BRN$ID),]

IntSlPartSurv<-merge(IntSlopes.PS,BRN[,2:3],by.y="ID",by.x="IDs")

head(IntSlPartSurv)

corsPartSurv<-data.frame(mcmc_cor_PartSurv)
IntSlPartSurv$col<-wes_palette("Darjeeling1", n=5)[IntSlPartSurv$Herd]
cols<-wes_palette("Darjeeling1", n=5)


##### Making the plot:

png("Figures/Correlations.Aug24.png",width=7.09,height=8.66,units="in",
    res=600,pointsize=11.5)
#pdf("Figures/Correlations.March18HybridAug24_bigSym.pdf",width=7.09,height=8.66,pointsize=11.5)

xloc<-0.95
yloc<-0.9
xcor<-0.65
ycor<-0.1

par(mar=c(4.2,4,1,0.5),oma=c(1,1,0,0.5),mfrow=c(2,2),mgp=c(2.75,1,0))

plot(IntSlMigPart$PartInt.ID.MP~IntSlMigPart$MigInt.ID.MP,col=IntSlMigPart$col,
     xlab="Migration timing (average environment)",ylab="Parturition timing (average environment)",pch=16,las=1,cex.lab=1.25,cex=1)
abline(lm(IntSlMigPart$PartInt.ID.MP~IntSlMigPart$MigInt.ID.MP))
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("a)",x=xloc,y=yloc,cex=1.8)
text(paste("Cor: ",round(corsMigPart[1,2],3)," (",round(corsMigPart[1,3],3),
           ", ",round(corsMigPart[1,4],3),")",sep=""),x=xcor,y=ycor)
par(usr = usr) # restore original user coordinates
legend(legend=c("Buchans","Grey River","Lapoile","Middle Ridge","Topsails"),
       col=cols,x=-1.1,y=0.8,ncol=2,pch=16,bty="n", cex=1.2)

plot(IntSlMigPart$PartSlope.ID.MP~IntSlMigPart$MigSlope.ID.MP,col=IntSlMigPart$col,
     xlab="Plasticity in migration time ~ snowmelt",ylab="Plasticity in parturition time ~ snowmelt",pch=16,las=1,cex.lab=1.25,cex=1)
abline(lm(IntSlMigPart$PartSlope.ID.MP~IntSlMigPart$MigSlope.ID.MP))
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("b)",x=xloc,y=yloc,cex=1.8)
text(paste("Cor: ",round(corsMigPart[4,2],3)," (",round(corsMigPart[4,3],3),
           ", ",round(corsMigPart[4,4],3),")",sep=""),,x=xcor,y=ycor)
par(usr = usr) # restore original user coordinates

plot(IntSlPartSurv$SurvInt.ID.PS~IntSlPartSurv$PartInt.ID.PS,col=IntSlPartSurv$col,
     xlab="Parturition timing (average environment)",ylab="Probability of calf survival (average environment)",pch=16,las=1,cex.lab=1.25,cex=1)
abline(lm(IntSlPartSurv$SurvInt.ID.PS~IntSlPartSurv$PartInt.ID.PS))
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("c)",x=xloc,y=yloc,cex=1.8)
text(paste("Cor: ",round(corsPartSurv[1,2],3)," (",format(round(corsPartSurv[1,3],3),nsmall=3),
           ", ",round(corsPartSurv[1,4],3),")",sep=""),,x=xcor,y=ycor)
par(usr = usr) # restore original user coordinates

plot(IntSlPartSurv$SurvInt.ID.PS~IntSlPartSurv$PartSlope.ID.PS,col=IntSlPartSurv$col,
     xlab="Plasticity in parturition time ~ green-up",ylab="Probability of calf survival (average environment)",pch=16,las=1,cex.lab=1.25,cex=1)
abline(lm(IntSlPartSurv$SurvInt.ID.PS~IntSlPartSurv$PartSlope.ID.PS))
usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("d)",x=xloc,y=yloc,cex=1.8)
text(paste("Cor: ",round(corsPartSurv[6,2],3)," (",format(round(corsPartSurv[6,3],3),nsmall=3),
           ", ",round(corsPartSurv[6,4],3),")",sep=""),x=xcor,y=ycor)
par(usr = usr) # restore original user coordinates
dev.off()



###### Part 3: plotting BRNs

### Packages ----
libs <- c('data.table',
          'MCMCglmm',
          'tidyr', 'gridExtra',
          'ggplot2', 'dplyr','standardize')
lapply(libs, require, character.only = TRUE)

### Reaction Norm Figures ----
df_brns <- tibble(ID = BRN$ID,
                  SnowOff = BRN$SnowOff,
                  GreenUp = BRN$GreenUp,
                  MigEnd = BRN$MigEnd,
                  Year = BRN$year,
                  BirthDate = BRN$CalfDateHybrid,
                  BirthDateSc = scale_by(CalfDateHybrid~Population,BRN),
                  Population = BRN$Population,
                  MigEndSc = scale_by(MigEnd~Population,BRN),
                  SnowOffSc = scale_by(SnowOff~Population,BRN),
                  GreenUpSc = scale_by(GreenUp~Population,BRN))

#df_brns<-subset(df_brns,BirthDateSc>-100)

####### Part 1: Arrival on summer ground


## run models for Figure 3 and export for figure script
p.var_MigEnd <- var(df_brns$MigEnd,na.rm=TRUE)

prior_MigEnd <- list(G=list(G1=list(V=diag(2)*(p.var_MigEnd/2), nu=1,
                                    alpha.V=diag(2)*p.var_MigEnd/2)),
                     R=list(V=diag(1)*(p.var_MigEnd/2), nu=1))

mcmcMigEnd <- MCMCglmm(MigEndSc ~ BirthDateSc 
                       + Population + SnowOffSc,
                       random =~ us(1 + SnowOffSc):ID,
                       rcov = ~units,
                       family = "gaussian",
                       prior = prior_MigEnd,
                       nitt=420000,
                       burnin=20000,
                       thin=100,
                       verbose = TRUE,
                       data = df_brns,
                       pr=TRUE,
                       saveX = TRUE,
                       saveZ = TRUE)


saveRDS(mcmcMigEnd, "Output/BRN_models/BRNs_plots/MigEnd_birthDateMarch12Hybrid.RDS")

### Reaction Norm for Birth date ----
p.var_BirthDate <- var(df_brns$BirthDate, na.rm = TRUE)

prior_BirthDate <- list(G=list(G1=list(V=diag(2)*(p.var_BirthDate/2), nu=1,
                                       alpha.V=diag(2)*p.var_BirthDate/2)),
                        R=list(V=diag(1)*(p.var_BirthDate/2), nu=1))

mcmcBirthDate <- MCMCglmm(BirthDateSc ~ MigEndSc + Population +
                            SnowOffSc,
                          random =~ us(1 + SnowOffSc):ID,
                          rcov = ~units,
                          family = "gaussian",
                          prior = prior_BirthDate,
                          nitt=420000,
                          burnin=20000,
                          thin=100,
                          verbose = TRUE,
                          data = df_brns,
                          pr=TRUE,
                          saveX = TRUE,
                          saveZ = TRUE)

saveRDS(mcmcBirthDate, "Output/BRN_models/BRNs_plots/BirthDateMarch12Hybrid.RDS")


### Reaction Norm for Birth date - green-up ----
p.var_BirthDate <- var(df_brns$BirthDate, na.rm = TRUE)

prior_BirthDate <- list(G=list(G1=list(V=diag(2)*(p.var_BirthDate/2), nu=1,
                                       alpha.V=diag(2)*p.var_BirthDate/2)),
                        R=list(V=diag(1)*(p.var_BirthDate/2), nu=1))

mcmcBirthDate_gu <- MCMCglmm(BirthDateSc ~ MigEndSc + Population +
                               SnowOffSc,
                             random =~ us(1 + GreenUpSc):ID,
                             rcov = ~units,
                             family = "gaussian",
                             prior = prior_BirthDate,
                             nitt=420000,
                             burnin=20000,
                             thin=100,
                             verbose = TRUE,
                             data = df_brns,
                             pr=TRUE,
                             saveX = TRUE,
                             saveZ = TRUE)

saveRDS(mcmcBirthDate_gu, "Output/BRN_models/BRNs_plots/BirthDateguMarch12Hybrid.RDS")



df_MigEnd <- cbind(df_brns,
                   fit = predict(mcmcMigEnd, marginal = NULL)) %>%
  group_by(ID, Population, SnowOffSc) %>%
  summarise(fit = mean(fit),
            MigEnd = mean(MigEnd)) %>%
  gather(Type, Value,
         fit:MigEnd)

df_fit_MigEnd = setDT(df_MigEnd)[Type == "fit"]

saveRDS(df_fit_MigEnd, "Output/BRN_models/BRNs_plots/df_model_fit_MigEnd_BirthDateMarch12Hybrid.RDS")




## Birthdate - snow
df_BirthDate <- cbind(df_brns,
                      fit = predict(mcmcBirthDate, marginal = NULL)) %>%
  group_by(ID, Population, SnowOffSc) %>%
  summarise(fit = mean(fit),
            BirthDate = mean(BirthDate)) %>%
  gather(Type, Value,
         fit:BirthDate)

df_fit_BirthDate = setDT(df_BirthDate)[Type == "fit"]

saveRDS(df_fit_BirthDate, "Output/BRN_models/BRNs_plots/df_model_fit_BirthDateMarch12Hybrid.RDS")


## Birthdate - green
df_BirthDate_gu <- cbind(df_brns,
                         fit = predict(mcmcBirthDate_gu, marginal = NULL)) %>%
  group_by(ID, Population, GreenUpSc) %>%
  summarise(fit = mean(fit),
            BirthDate = mean(BirthDate)) %>%
  gather(Type, Value,
         fit:BirthDate)

df_fit_BirthDate_gu = setDT(df_BirthDate_gu)[Type == "fit"]

saveRDS(df_fit_BirthDate_gu, "Output/BRN_models/BRNs_plots/df_model_fit_BirthDate_guMarch12Hybrid.RDS")


#### Make plots

df_fit_MigEnd<-readRDS("Output/BRN_models/BRNs_plots/df_model_fit_MigEnd_BirthDateMarch12Hybrid.RDS")
df_fit_BirthDate<-readRDS("Output/BRN_models/BRNs_plots/df_model_fit_BirthDateMarch12Hybrid.RDS")
df_fit_BirthDate_gu<-readRDS("Output/BRN_models/BRNs_plots/df_model_fit_BirthDate_guMarch12Hybrid.RDS")


library(wesanderson)

df_fit_MigEnd$col<-wes_palette("Darjeeling1", n =5)[df_fit_MigEnd$Population]
df_fit_BirthDate$col<-wes_palette("Darjeeling1", n =5)[df_fit_BirthDate$Population]
df_fit_BirthDate_gu$col<-wes_palette("Darjeeling1", n =5)[df_fit_BirthDate_gu$Population]

#df_fit_BirthDate$col_ID<-paste(df_fit_BirthDate$col,df_fit_BirthDate$ID,sep="")
#coloursPlot<-substr(unique(df_fit_BirthDate$col_ID),1,7)

# extract slopes and intercepts for population level
#intMigEnd = lm(Value ~ SnowOffSc, data = df_fit_MigEnd)$coefficients[1]
#slopeMigEnd = lm(Value ~ SnowOffSc, data = df_fit_MigEnd)$coefficients[2]
#intBirthDate = lm(Value ~ SnowOffSc, data = df_fit_BirthDate)$coefficients[1]
#slopeBirthDate = lm(Value ~ SnowOffSc, data = df_fit_BirthDate)$coefficients[2]


cols<-wes_palette("Darjeeling1", n=5)
### Figure ----

str(df_fit_MigEnd)
df_fit_MigEnd$SnowOffSc<-as.numeric(df_fit_MigEnd$SnowOffSc)
df_fit_BirthDate$SnowOffSc<-as.numeric(df_fit_BirthDate$SnowOffSc)
df_fit_BirthDate_gu$GreenUpSc<-as.numeric(df_fit_BirthDate_gu$GreenUpSc)


pdf("Figures/BRNs_March12HybridAug24.pdf", height = 7.1, width = 4.33, pointsize=12)
#png("Figures/BRNs_March12Hybrid.png", height = 7, width= 4, units="in",res=600)

xloc<-0.95
yloc<-0.9

par(mfrow=c(3,1),mar=c(4,4,1,1),oma=c(1,0,0,0),mgp=c(2.75,1,0))
plot(df_fit_MigEnd$Value~df_fit_MigEnd$SnowOffSc,type="l",col="white",xlab="Timing of snowmelt (scaled)",
     ylab="Timing of migration (scaled)",xlim=c(-2,2),las=1,cex.lab=1.3)
for(i in unique(df_fit_MigEnd$ID)){
  sub<-subset(df_fit_MigEnd,ID==i)
  if(nrow(sub)>1){
    fmod <- lm(Value~SnowOffSc,data=sub)
    Xvec <- seq(min(sub$SnowOffSc), max(sub$SnowOffSc),by=0.1)
    nd <- data.frame("SnowOffSc" = Xvec)
    Y <- predict(fmod, newdata=nd)
    lines(x=Xvec, y=Y,col=sub$col[1],lwd=0.7)
  }else{}
}

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("a)",x=xloc,y=yloc, cex=1.5)
par(usr = usr) # restore original user coordinates



plot(df_fit_BirthDate$Value~df_fit_BirthDate$SnowOffSc,type="l",col="white",
     ylab="Timing of parturition (scaled)",xlab="Timing of snowmelt (scaled)",
     xlim=c(-2,2),las=1,cex.lab=1.3)
for(i in unique(df_fit_BirthDate$ID)){
  sub<-subset(df_fit_BirthDate,ID==i)
  if(nrow(sub)>1){
    fmod <- lm(Value~SnowOffSc,data=sub)
    Xvec <- seq(min(sub$SnowOffSc), max(sub$SnowOffSc),by=0.1)
    nd <- data.frame("SnowOffSc" = Xvec)
    Y <- predict(fmod, newdata=nd)
    lines(x=Xvec, y=Y,col=sub$col[1],lwd=0.7)
  }else{}
}

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("b)",x=xloc,y=yloc, cex=1.5)
par(usr = usr) # restore original user coordinates

plot(df_fit_BirthDate_gu$Value~df_fit_BirthDate_gu$GreenUpSc,type="l",col="white",
     ylab="Timing of parturition (scaled)",xlab="Timing of green-up (scaled)",
     xlim=c(-2,2),las=1,cex.lab=1.3)
for(i in unique(df_fit_BirthDate_gu$ID)){
  sub<-subset(df_fit_BirthDate_gu,ID==i)
  if(nrow(sub)>1){
    fmod <- lm(Value~GreenUpSc,data=sub)
    Xvec <- seq(min(sub$GreenUpSc), max(sub$GreenUpSc),by=0.1)
    nd <- data.frame("GreenUpSc" = Xvec)
    Y <- predict(fmod, newdata=nd)
    lines(x=Xvec, y=Y,col=sub$col[1],lwd=0.7)
  }else{}
}

legend(legend=c("Buchans","Grey River","Lapoile","Middle Ridge","Topsails"),
       col=cols,x=-1.6,y=1.5,ncol=3,bty="n",lty=1,lwd=2.5)

usr <- par("usr")   # get user coordinates
par(usr = c(0, 1, 0, 1)) # new relative user coordinates
text("c)",x=xloc,y=yloc, cex=1.5)
par(usr = usr) # restore original user coordinates

dev.off()

############################################################################

###################   END   ################################################

############################################################################

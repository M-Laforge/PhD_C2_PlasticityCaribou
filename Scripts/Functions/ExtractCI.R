######## Function to extract CIs from a model output #########

## Arguments:

##model = the name of the model to extract estimates and CIs from
##digits = the number of digits to round down to. Default = 2.
##random = Does the model have random terms? If True, will extract parameter estimates using "fixef",
##       otherwise extracts using "coef". Default = FALSE

##------------------------------------------------------------------------------##

##################################################################################

CI95extract<-function(
  model,
  digits=2,
  random=F
)
{
  if(random==F){
    CI<-paste(round(coef(model),digits)," (",round(coef(model)-(sqrt(diag(vcov(model)))*1.96),digits), ", ",
              round(coef(model)+(sqrt(diag(vcov(model)))*1.96),digits), ")",sep="")
    names(CI)<-names(coefficients(model))
  }else{
    CI<-paste(round(fixef(model),digits)," (",round(fixef(model)-(sqrt(diag(vcov(model)))*1.96),digits), ", ",
              round(fixef(model)+(sqrt(diag(vcov(model)))*1.96),digits), ")",sep="")
    names(CI)<-names(fixef(model))
  }
    CI2<-data.frame(CI)
    colnames(CI2)<-"Estimate + 95% CI"
    CI2
  }



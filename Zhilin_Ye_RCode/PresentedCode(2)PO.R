################################################
############## ISYE 6414MSA ####################
########### Final Data Analysis ################
####### Presented Code(1) PO Model #############

## Author: Zhilin(Brandon Ye) 
## Date:   12/09/2016
## Org:    Georgia Institute of Technology

############ PO Model ###############
#### Data preparation ####
rm(list=ls())
data <- read.csv('DataADULT_Version1.csv')
attach(data)
PO.Cost.Scale <- PO.Cost/PMPM # Scale the PO cost
#####################################

########## Quick and Dirty Model ##########
POData <- data[,-c(1,3,4,5)] 
# GEOID should be excluded because it is just a name
# (Maybe there is certain information within? I don't know.)
# ED.Cost, PO.Cost and PMPM are excluded because
# we only use the scaled PO.Cost as response,
# and ED and PO model should not interfere each other.
POData$PO.Cost.Scale <- PO.Cost.Scale
QuickModPO <- lm(PO.Cost.Scale~., data=POData)
summary(QuickModPO)
plot(QuickModED)
### We notice a long tail here, so we consider ##
### to do a log transformation on Y.           ##
POData <- data[,-c(1,3,4,5)]
POData$log.PO.Cost.Scale <- log(PO.Cost.Scale)
log.PO.Cost.Scale <- log(PO.Cost.Scale)
QuickModPO <- lm(log.PO.Cost.Scale~., data=POData)
summary(QuickModOPO)
plot(QuickModPO)
### Now everything looks much better. So we'll ##
### move on to exploratory analysis.           ##
############################################

########## Exploratory Analysis ############
### In order to get some inspiration on how to ##
### transform the predictors, at first we do a ##
### Generalized Addictive Model.               ##
######### GAM Analysis ###########
library(mgcv)
gamobj<-gam(log.PO.Cost.Scale~as.factor(Urbanicity)+
              as.factor(RankingsFood)+as.factor(State)+
              s(RankingsHousing)+s(EDs)+s(HOs)+s(POs)+
              s(WhitePop)+s(BlackPop)+s(OtherPop)+
              s(HealthyPop)+s(ChronicPop)+s(ComplexPop)+
              s(Unemployment)+s(Income)+s(Poverty)+
              s(Education)+s(TotalChild)+
              s(TotalAdult)+s(Accessibility)+
              s(Availability)+s(RankingsPCP)+
              s(RankingsExercise)+s(RankingsSocial)+
              s(ProvDensity),data=POData)
plot(gamobj)
### Not much information revealed                ##
###################################
##### Preparation of Transformation ######
### To prepare for transformation, we may need to##
### import some packages and define functions.   ##
library(boot) # Contains inverse logit.
# To define a function to rescale data into (0,1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
##########################################
#### Exploratory Analysis ####
boxplot(log.PO.Cost.Scale~State)
boxplot(log.PO.Cost.Scale~Urbanicity) 
boxplot(log.PO.Cost.Scale~RankingsFood)
boxplot(log.PO.Cost.Scale~RankingsHousing)
plot(log(EDs),log.PO.Cost.Scale)   # log transformation looks good
plot(log(HOs),log.PO.Cost.Scale)   # log transformation looks good
plot(log(POs),log.PO.Cost.Scale)   # log transformation looks good 
plot(WhitePop,log.PO.Cost.Scale)
plot(BlackPop,log.PO.Cost.Scale)
plot(log(OtherPop),log.PO.Cost.Scale) # Maybe log?
plot(HealthyPop,log.PO.Cost.Scale)
plot(ChronicPop,log.PO.Cost.Scale)
plot(log(ComplexPop),log.PO.Cost.Scale) # Maybe log?
plot(Unemployment,log.PO.Cost.Scale)
plot(log(Income),log.PO.Cost.Scale)     # Maybe log?
plot(Poverty,log.PO.Cost.Scale)
plot(Education,log.PO.Cost.Scale)
plot(TotalChild,log.PO.Cost.Scale)
plot(log(TotalAdult),log.PO.Cost.Scale) # Maybe log?
plot(Availability,log.PO.Cost.Scale)
hist(Availability)                  
# A lot of number less than 0.15 in availability here.
# Transform it as Low Congestion -- Availability<=0.15;
# Middle Congestion -- 0.15<Availability<=0.5;
# High Congestion -- Availability>0.5.
Availability.New <- rep(0,dim(data)[1])
Availability.New[Availability<=0.15] <- 'Low'
Availability.New[Availability<=0.5 & Availability>0.15] <- 'Middle'
Availability.New[Availability>0.5] <- 'High'
boxplot(log.PO.Cost.Scale~Availability.New) # Not significant?
plot(Accessibility,log.PO.Cost.Scale)
hist(Accessibility) 
# Same for Accessibility as Availability. Do the same.
Accessibility.New <- rep(0,dim(data)[1])
Accessibility.New[Accessibility<=4] <- 'Low'
Accessibility.New[Accessibility<=10 & Availability>4] <- 'Middle'
Accessibility.New[Accessibility>10] <- 'High'
boxplot(log.PO.Cost.Scale~Accessibility.New)
POData$Availability.New <- Availability.New
POData$Accessibility.New <- Accessibility.New
plot(RankingsPCP,log.PO.Cost.Scale)
plot(RankingsExercise,log.PO.Cost.Scale)
plot(RankingsSocial,log.PO.Cost.Scale)
plot(ProvDensity,log.PO.Cost.Scale)
####################################
############################################
### The transformation suggested here is : ##
### EDs: log; POs: log;                    ##
### OtherPop: log;                         ##
### Income: log.                           ##
### Availability, Accessibility: categorical. 

########## Stepwise Regression ############
### Put the maximum amount of variables, and ##
### let the algorithm select.(Textbook P440) ##
LargestMod <- lm(log.PO.Cost.Scale~.+log(EDs)+log(POs)+
                   log(OtherPop),data=POData)
summary(LargestMod)
stepFirMod <- step(LargestMod,direction="both")
summary(stepFirMod)
fit <- fitted(stepFirMod)
rstand <- rstandard(stepFirMod)
resid <- resid(stepFirMod)
plot(fit,rstand)
qqnorm(rstandard(stepFirMod))
abline(0,1)
### Model With interaction
InteractMod <- lm(log.PO.Cost.Scale~.+log(EDs)+log(POs)+
                    log(OtherPop)+State*(EDs+POs+HOs)+
                    State*(HealthyPop+ChronicPop
                           +WhitePop+BlackPop)+
                    State*(Unemployment+Income),data=
                    POData[-c(4736,4737,4677,1396,2355,2123,2833,
                              2348,2246,3982,2542,2381),])
summary(InteractMod)
stepIntMod <- step(InteractMod,direction="both")
summary(stepIntMod)
fit <- fitted(stepIntMod)
rstand <- rstandard(stepIntMod)
resid <- resid(stepIntMod)
par(mfrow=(c(1,1)))
plot(fit,resid)
qqnorm(rstand)
abline(0,1)
plot(stepIntMod)
### The pattern looks much better. Let's now detect some ##
### outliers with cook's distance                        ##
I <- influence.measures(stepIntMod)
colnames(I$infmat)
cook = I$infmat[,53]
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance",
     main="Cook's Distance After Removing Outlier",
     ylim=c(-0.02,0.55))
which(cook == max(cook))
## We detect a potentially outlier here.                ##
## c(4736,4737,4677,1396,2355,2123,2833,2348,2246,3982,2542,2381)


###################################################

############ Lasso Regression ################
### Now try another method to do model selection ###
### At first we need to scale the data to (0,1)
library(glmnet)
EDs.scale <- range01(EDs)
log.EDs.scale <- range01(log(EDs))
HOs.scale <- range01(HOs)
POs.scale <- range01(POs)
log.POs.scale <- range01(log(POs))
WhitePop.scale <- WhitePop/100
BlackPop.scale <- BlackPop/100
OtherPop.scale <- OtherPop/100
log.OtherPop.scale <- range01(log(OtherPop))
HealthyPop.scale <- HealthyPop/100
ChronicPop.scale <- ChronicPop/100
ComplexPop.scale <- ComplexPop/100
Unemployment.scale <- range01(Unemployment) 
Income.scale <- range01(Income)
Poverty.scale <- Poverty/100
Education.scale <- Education/100
TotalChild.scale <- range01(TotalChild)
TotalAdult.scale <- range01(TotalAdult)
Accessibility.scale <- range01(Accessibility)
Availability.scale <- range01(Availability)
RankingsPCP.scale <- range01(RankingsPCP)
RankingsFood.scale <- range01(RankingsFood)
RankingsHousing.scale <- RankingsHousing/100
RankingsExercise.scale <- RankingsExercise/100
RankingsSocial.scale <- range01(RankingsSocial)
ProvDensity.scale <- range01(ProvDensity)
#### Now do the lasso ###
predictors <- cbind(EDs.scale,log.EDs.scale,HOs.scale,POs.scale,log.POs.scale,
                    WhitePop.scale,BlackPop.scale,OtherPop.scale,log.OtherPop.scale,
                    HealthyPop.scale, ChronicPop.scale,ComplexPop.scale,
                    Unemployment.scale,Income.scale,
                    Poverty.scale, Education.scale, TotalChild.scale,
                    TotalAdult.scale,Accessibility.scale,Availability.scale,
                    RankingsPCP.scale,RankingsHousing.scale,RankingsExercise.scale,
                    RankingsSocial.scale,ProvDensity.scale,RankingsFood.scale)
fullVars <- data.frame(predictors,State,Urbanicity,Accessibility.New,
                       Availability.New,log.PO.Cost.Scale)
lassomatrix <- model.matrix(log.PO.Cost.Scale~.+State*(EDs.scale+POs.scale+HOs.scale)+
                              State*(ChronicPop.scale+WhitePop.scale),data=fullVars)
cvglmobj <- cv.glmnet(lassomatrix,log.PO.Cost.Scale)
model=glmnet(lassomatrix,log.PO.Cost.Scale,lambda=cvglmobj$lambda.1se)
model$beta
#### Also can do group lasso here. A potential improvement
################################################

############ Logistic Lasso Regression ################
### Controlling the States effect, we might be able devide ###
### the tracts in a certain state as 'ReachedStateAverage' ###
### or not. This may give valuable information of how to   ###
### make the cost of a certain tract below state average.  ###
###### Control the State #######
ControlState <- lm(log.PO.Cost.Scale~State,data=POData)
summary(ControlState)
StateControlled=resid(ControlState)
plot(StateControlled)
###### Do Logistic Regression ######
POControlledData <- POData
POControlledData$ReachedAverage <- rep(TRUE,dim(POData)[1])
POControlledData$ReachedAverage[StateControlled<0.0] <- FALSE
POControlledData$log.PO.Cost.Scale <- NULL
POControlledData$State <- NULL
StateControlledMod <- glm(ReachedAverage~.,data=POControlledData,family='binomial')
summary(StateControlledMod)
stepControlledMod <- step(StateControlledMod,direction="both")
summary(stepControlledMod)
### Cross-Validation ###
Cost = function(y,pi){
  err = mean(abs(y-pi)>0.5)
  return(err)
}
library(boot)
ERR = cv.glm(POControlledData, glmfit=stepControlledMod, 
             K=10, cost=Cost)$delta[2]
print(ERR)
##### Rescale Data #####
POControlledData.scale <- data.frame('1'=rep(0,5019))
POControlledData.scale$EDs.scale <- range01(EDs)
POControlledData.scale$log.EDs.scale <- range01(log(EDs))
POControlledData.scale$HOs.scale <- range01(HOs)
POControlledData.scale$POs.scale <- range01(POs)
POControlledData.scale$log.POs.scale <- range01(log(POs))
POControlledData.scale$WhitePop.scale <- WhitePop/100
POControlledData.scale$BlackPop.scale <- BlackPop/100
POControlledData.scale$OtherPop.scale <- OtherPop/100
POControlledData.scale$HealthyPop.scale <- HealthyPop/100
POControlledData.scale$ChronicPop.scale <- ChronicPop/100
POControlledData.scale$ComplexPop.scale <- ComplexPop/100
POControlledData.scale$Unemployment.scale <- range01(Unemployment) 
POControlledData.scale$Income.scale <- range01(Income)
POControlledData.scale$Poverty.scale <- Poverty/100
POControlledData.scale$Education.scale <- Education/100
POControlledData.scale$TotalChild.scale <- range01(TotalChild)
POControlledData.scale$TotalAdult.scale <- range01(TotalAdult)
POControlledData.scale$Accessibility.scale <- range01(Accessibility)
POControlledData.scale$Availability.scale <- range01(Availability)
POControlledData.scale$RankingsPCP.scale <- range01(RankingsPCP)
POControlledData.scale$RankingsFood.scale <- range01(RankingsFood)
POControlledData.scale$RankingsHousing.scale <- RankingsHousing/100
POControlledData.scale$RankingsExercise.scale <- RankingsExercise/100
POControlledData.scale$RankingsSocial.scale <- range01(RankingsSocial)
POControlledData.scale$ProvDensity.scale <- range01(ProvDensity)
#### Lasso ####
POControlledData.scale$ReachedAverage <- POControlledData$ReachedAverage
POControlledData.scale$Urbanicity <- POControlledData$Urbanicity
POControlledData.scale$X1 <- NULL
logisticmatrix <- model.matrix(ReachedAverage~.,data=POControlledData.scale)
ReachedAverage <- POControlledData$ReachedAverage
cvglmobj <- cv.glmnet(logisticmatrix,ReachedAverage,family='binomial')
glmmodel=glmnet(logisticmatrix,ReachedAverage,family='binomial',lambda=cvglmobj$lambda.1se)
glmmodel$beta
prediction <- predict(glmmodel,newx=logisticmatrix) >= 0.5
observation <- ReachedAverage>=0.5
Accuracy <- sum(prediction == observation)/dim(data)[1]
predictTRUE <- sum(prediction)/dim(data)[1]
observeTRUE <- sum(observation)/dim(data)[1]
WorseBound = 1.25*(predictTRUE*observeTRUE + (1-predictTRUE)*(1-observeTRUE))
#########################################################

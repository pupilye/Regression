################################################
############## ISYE 6414MSA ####################
########### Final Data Analysis ################
####### Presented Code(1) ED Model #############

## Author: Zhilin(Brandon Ye) 
## Date:   12/09/2016
## Org:    Georgia Institute of Technology

############ ED Model ###############
#### Data preparation ####
rm(list=ls())
data <- read.csv('DataADULT_Version1.csv')
attach(data)
ED.Cost.Scale <- ED.Cost/PMPM # Scale the ED cost
#####################################

########## Quick and Dirty Model ##########
EDData <- data[,-c(1,3,4,5)] 
  # GEOID should be excluded because it is just a name
    # (Maybe there is certain information within? I don't know.)
  # ED.Cost, PO.Cost and PMPM are excluded because
    # we only use the scaled ED.Cost as response,
    # and ED and PO model should not interfere each other.
EDData$ED.Cost.Scale <- ED.Cost.Scale
QuickModED <- lm(ED.Cost.Scale~., data=EDData)
summary(QuickModED)
plot(QuickModED)
### We notice a long tail here, so we consider ##
### to do a log transformation on Y.           ##
EDData <- data[,-c(1,3,4,5)]
EDData$log.ED.Cost.Scale <- log(ED.Cost.Scale)
log.ED.Cost.Scale <- log(ED.Cost.Scale)
QuickModED <- lm(log.ED.Cost.Scale~., data=EDData)
summary(QuickModED)
plot(QuickModED)
### Now everything looks much better. So we'll ##
### move on to exploratory analysis.           ##
############################################

########## Exploratory Analysis ############
### In order to get some inspiration on how to ##
### transform the predictors, at first we do a ##
### Generalized Addictive Model.               ##
######### GAM Analysis ###########
library(mgcv)
gamobj<-gam(log.ED.Cost.Scale~as.factor(Urbanicity)+
              as.factor(RankingsFood)+as.factor(State)+
              s(RankingsHousing)+s(EDs)+s(HOs)+s(POs)+
              s(WhitePop)+s(BlackPop)+s(OtherPop)+
              s(HealthyPop)+s(ChronicPop)+s(ComplexPop)+
              s(Unemployment)+s(Income)+s(Poverty)+
              s(Education)+s(TotalChild)+
              s(TotalAdult)+s(Accessibility)+
              s(Availability)+s(RankingsPCP)+
              s(RankingsExercise)+s(RankingsSocial)+
              s(ProvDensity),data=EDData)
plot(gamobj)
### From the gam plot, we can notice that, there ##
### may be a log form for EDs, and an inverse    ##
### logit form for ComplexPop. We keep this in   ##
### mind and do the exploratory analysis.        ##
###################################
##### Preparation of Transformation ######
### To prepare for transformation, we may need to##
### import some packages and define functions.   ##
library(boot) # Contains inverse logit.
  # To define a function to rescale data into (0,1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
##########################################
#### Exploratory Analysis ####
boxplot(log.ED.Cost.Scale~State)
boxplot(log.ED.Cost.Scale~Urbanicity) # Not significant?
boxplot(log.ED.Cost.Scale~RankingsFood)
boxplot(log.ED.Cost.Scale~RankingsHousing)
plot(log(EDs),log.ED.Cost.Scale)   # log transformation looks good
plot(HOs,log.ED.Cost.Scale)
plot(log(POs),log.ED.Cost.Scale)   # log transformation looks good 
plot(WhitePop,log.ED.Cost.Scale)
plot(BlackPop,log.ED.Cost.Scale)
plot(log(OtherPop),log.ED.Cost.Scale) # Maybe log?
plot(HealthyPop,log.ED.Cost.Scale)
plot(ChronicPop,log.ED.Cost.Scale)
plot(ComplexPop,log.ED.Cost.Scale)
plot(inv.logit(range01(ComplexPop)),log.ED.Cost.Scale)  
                                    # inv.logit really doesn't seem
                                    # to improve anything, but we'll see.
plot(Unemployment,log.ED.Cost.Scale)
plot(Income,log.ED.Cost.Scale)
plot(Poverty,log.ED.Cost.Scale)
plot(Education,log.ED.Cost.Scale)
plot(TotalChild,log.ED.Cost.Scale)
plot(TotalAdult,log.ED.Cost.Scale)
plot(Availability,log.ED.Cost.Scale)
hist(Availability)                  
  # A lot of number less than 0.15 in availability here.
  # Transform it as Low Congestion -- Availability<=0.15;
  # Middle Congestion -- 0.15<Availability<=0.5;
  # High Congestion -- Availability>0.5.
Availability.New <- rep(0,dim(data)[1])
Availability.New[Availability<=0.15] <- 'Low'
Availability.New[Availability<=0.5 & Availability>0.15] <- 'Middle'
Availability.New[Availability>0.5] <- 'High'
boxplot(log.ED.Cost.Scale~Availability.New) # Not significant?
plot(Accessibility,log.ED.Cost.Scale)
hist(Accessibility) 
  # Same for Accessibility as Availability. Do the same.
Accessibility.New <- rep(0,dim(data)[1])
Accessibility.New[Accessibility<=4] <- 'Low'
Accessibility.New[Accessibility<=10 & Availability>4] <- 'Middle'
Accessibility.New[Accessibility>10] <- 'High'
boxplot(log.ED.Cost.Scale~Accessibility.New)
EDData$Availability.New <- Availability.New
EDData$Accessibility.New <- Accessibility.New
plot(RankingsPCP,log.ED.Cost.Scale)
plot(RankingsExercise,log.ED.Cost.Scale)
plot(RankingsSocial,log.ED.Cost.Scale)
plot(ProvDensity,log.ED.Cost.Scale)
####################################
############################################
### The transformation suggested here is : ##
### EDs: log; POs: log; OtherPop: log;     ##
### ComplexPop:inv.logit;                  ##
### Availability, Accessibility: categorical. 

########## Stepwise Regression ############
### Put the maximum amount of variables, and ##
### let the algorithm select.(Textbook P440) ##
LargestMod <- lm(log.ED.Cost.Scale~.+log(EDs)+log(POs)+
                   inv.logit(range01(ComplexPop))+
                   log(OtherPop),data=EDData)
summary(LargestMod)
stepFirMod <- step(LargestMod,direction="both")
summary(stepFirMod)
fit <- fitted(stepFirMod)
rstand <- rstandard(stepFirMod)
resid <- resid(stepFirMod)
plot(fit,rstand)
qqnorm(rstandard(stepFirMod))
abline(0,1)
### The residual plot over fitted value really doesn't #
### look good, because there is a huge gap between    ##
### fitted value 3.0 and 3.5, which is just a small   ##
### gap in the scatter plot. See this two plot:       ##
par(mfrow=c(1,2))
plot(fit,main='Scatter Plot for Fitted Value')
plot(log.ED.Cost.Scale,main='Scatter Plot for Response')
par(mfrow=c(1,1))
### Therefore, we need to figure out what happens in  ##
### the tract where log.ED.Cost.Scale lies in range   ##
### (2,3). And it is for state AL
AL <- EDData[EDData$State=='AL',]
AR <- EDData[EDData$State=='AR',]
LA <- EDData[EDData$State=='LA',]
NC <- EDData[EDData$State=='NC',]
### Let's do some trellis display here to see if there##
### is some interaction effect.                       ##
library(lattice)
ChronicPopSlice <- ChronicPop
hist(ChronicPop)
ChronicPopSlice[ChronicPop<10] = 1
ChronicPopSlice[ChronicPop<15 & ChronicPop>=10] = 2
ChronicPopSlice[ChronicPop<20 & ChronicPop>=15] = 3
ChronicPopSlice[ChronicPop>=20] = 4
bwplot(log.ED.Cost.Scale~ChronicPop|State, 
       main="Scatterplots by State", 
       ylab="log.ED.Cost.Scale", xlab="ChronicPop")
### For a certain state, with different chronicPop,   ##
### they did have some difference, especially for AL. ##
### So now try to add some interaction term in the model.
InteractMod <- lm(log.ED.Cost.Scale~.+log(EDs)+log(POs)+
                   inv.logit(range01(ComplexPop))+
                   log(OtherPop)+State*(EDs+POs+HOs)+
                    State*(HealthyPop+ChronicPop
                    +WhitePop+BlackPop)+
                    State*(Unemployment+Income),data=EDData)
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
par(mfrow=c(1,2))
plot(fit,main='Scatter Plot for Fitted Value After Adding Interaction')
plot(log.ED.Cost.Scale,main='Scatter Plot for Response')
### The pattern looks much better. Let's now detect some ##
### outliers with cook's distance                        ##
I <- influence.measures(stepIntMod)
colnames(I$infmat)
cook = I$infmat[,58]
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance",
     main="Cook's Distance Before Removing Outlier",
     ylim=c(-0.02,0.55))
which(cook == max(cook))
## We detect a potentially outlier here. which is 1396. ##
## Remove that and redo the model.                      ##
InteractMod <- lm(log.ED.Cost.Scale~.+log(EDs)+log(POs)+
                    inv.logit(range01(ComplexPop))+
                    log(OtherPop)+State*(EDs+POs+HOs)+
                    State*(HealthyPop+ChronicPop+WhitePop)+
                    State*(Unemployment),data=EDData[-1396,])
summary(InteractMod)
stepIntMod <- step(InteractMod,direction="both")
summary(stepIntMod)
fit <- fitted(stepIntMod)
rstand <- rstandard(stepIntMod)
resid <- resid(stepIntMod)
plot(fit,resid)
plot(stepIntMod)
### The pattern looks much better. Let's now detect some ##
### outliers with cook's distance                        ##
I <- influence.measures(stepIntMod)
colnames(I$infmat)
cook = I$infmat[,50]
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance",
     main="Cook's Distance After Removing Outlier",
     ylim=c(-0.02,0.55))
which(cook == max(cook))
### Now we should consider no outliers. Looks good!!    ##
par(mfrow=(c(1,2)))
plot(stepIntMod)

### It seems like Availability.New,Accessibility.New, ##
### log(EDs), log(POs), inv.logit(range01(ComplexPop))##
### are statistically significant.                    ##
### Income must be kept ther as a base.               ##
### But I expect RankingsExercise should be significant#
### since more exercises intuitively mean better health#
### So do a partial F-test here to confirm this guess.##
### do a partial F-test to figure out that.           ##
#### Partial F-test on RankingsExercises ####
fullFormula <- log.ED.Cost.Scale~State+EDs+HOs+POs+WhitePop+
  HealthyPop+ChronicPop+Unemployment+
  Income+TotalChild+TotalAdult+Accessibility+
  Accessibility.New+Availability.New+RankingsPCP+
  RankingsFood+RankingsHousing+RankingsSocial+
  log(EDs)+log(POs)+inv.logit(range01(ComplexPop))+
  RankingsExercise+
  State*(EDs+POs+HOs+HealthyPop+ChronicPop+
           WhitePop+Unemployment)
full <- lm(fullFormula,data=EDData[-1396,])
reducedFormula <- log.ED.Cost.Scale~State+EDs+POs+HOs+WhitePop+
  HealthyPop+ChronicPop+Unemployment+
  Income+TotalChild+TotalAdult+Accessibility+
  Accessibility.New+Availability.New+RankingsPCP+
  RankingsFood+RankingsHousing+RankingsSocial+
  log(EDs)+log(POs)+inv.logit(range01(ComplexPop))+
  State*(EDs+POs+HOs+HealthyPop+ChronicPop+
           WhitePop+Unemployment)
reduced <- lm(reducedFormula,data=EDData[-1396,])
anova(reduced,full)
 # Hmm, they are not significantly different.
 # Alright, maybe exercise is not as relevant as others.
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
inv.logit.ComplexPop.scale <- inv.logit(range01(ComplexPop))
Unemployment.scale <- range01(Unemployment) 
log.Unemployment <- log(Unemployment)
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
                    inv.logit.ComplexPop.scale,Unemployment.scale,Income.scale,
                    Poverty.scale, Education.scale, TotalChild.scale,
                    TotalAdult.scale,Accessibility.scale,Availability.scale,
                    RankingsPCP.scale,RankingsHousing.scale,RankingsExercise.scale,
                    RankingsSocial.scale,ProvDensity.scale,RankingsFood.scale)
fullVars <- data.frame(predictors,State,Urbanicity,Accessibility.New,
                       Availability.New,log.ED.Cost.Scale)
lassomatrix <- model.matrix(log.ED.Cost.Scale~.+State*(EDs.scale+POs.scale+HOs.scale)+
                              State*(ChronicPop.scale+WhitePop.scale),data=fullVars)
cvglmobj <- cv.glmnet(lassomatrix,log.ED.Cost.Scale)
model=glmnet(lassomatrix,log.ED.Cost.Scale,lambda=cvglmobj$lambda.1se)
model$beta
#### Also can do group lasso here. A potential improvement
################################################

############ Logistic Lasso Regression ################
### Controlling the States effect, we might be able devide ###
### the tracts in a certain state as 'ReachedStateAverage' ###
### or not. This may give valuable information of how to   ###
### make the cost of a certain tract below state average.  ###
###### Control the State #######
ControlState <- lm(log.ED.Cost.Scale~State,data=EDData)
summary(ControlState)
StateControlled=resid(ControlState)
plot(StateControlled)
###### Do Logistic Regression ######
EDControlledData <- EDData
EDControlledData$ReachedAverage <- rep(TRUE,dim(EDData)[1])
EDControlledData$ReachedAverage[StateControlled<0.0] <- FALSE
EDControlledData$log.ED.Cost.Scale <- NULL
EDControlledData$State <- NULL
StateControlledMod <- glm(ReachedAverage~.,data=EDControlledData,family='binomial')
summary(StateControlledMod)
stepControlledMod <- step(StateControlledMod,direction="both")
summary(stepControlledMod)
### Cross-Validation ###
Cost = function(y,pi){
  err = mean(abs(y-pi)>0.5)
  return(err)
}
library(boot)
ERR = cv.glm(EDControlledData, glmfit=stepControlledMod, 
            K=10, cost=Cost)$delta[2]
print(ERR)
##### Rescale Data #####
EDControlledData.scale <- data.frame('1'=rep(0,5019))
EDControlledData.scale$EDs.scale <- range01(EDs)
EDControlledData.scale$log.EDs.scale <- range01(log(EDs))
EDControlledData.scale$HOs.scale <- range01(HOs)
EDControlledData.scale$POs.scale <- range01(POs)
EDControlledData.scale$log.POs.scale <- range01(log(POs))
EDControlledData.scale$WhitePop.scale <- WhitePop/100
EDControlledData.scale$BlackPop.scale <- BlackPop/100
EDControlledData.scale$OtherPop.scale <- OtherPop/100
EDControlledData.scale$HealthyPop.scale <- HealthyPop/100
EDControlledData.scale$ChronicPop.scale <- ChronicPop/100
EDControlledData.scale$ComplexPop.scale <- ComplexPop/100
EDControlledData.scale$inv.logit.ComplexPop.scale <- inv.logit(range01(ComplexPop))
EDControlledData.scale$Unemployment.scale <- range01(Unemployment) 
EDControlledData.scale$Income.scale <- range01(Income)
EDControlledData.scale$Poverty.scale <- Poverty/100
EDControlledData.scale$Education.scale <- Education/100
EDControlledData.scale$TotalChild.scale <- range01(TotalChild)
EDControlledData.scale$TotalAdult.scale <- range01(TotalAdult)
EDControlledData.scale$Accessibility.scale <- range01(Accessibility)
EDControlledData.scale$Availability.scale <- range01(Availability)
EDControlledData.scale$RankingsPCP.scale <- range01(RankingsPCP)
EDControlledData.scale$RankingsFood.scale <- range01(RankingsFood)
EDControlledData.scale$RankingsHousing.scale <- RankingsHousing/100
EDControlledData.scale$RankingsExercise.scale <- RankingsExercise/100
EDControlledData.scale$RankingsSocial.scale <- range01(RankingsSocial)
EDControlledData.scale$ProvDensity.scale <- range01(ProvDensity)
#### Lasso ####
EDControlledData.scale$ReachedAverage <- EDControlledData$ReachedAverage
EDControlledData.scale$Urbanicity <- EDControlledData$Urbanicity
EDControlledData.scale$X1 <- NULL
logisticmatrix <- model.matrix(ReachedAverage~.,data=EDControlledData.scale)
ReachedAverage <- EDControlledData$ReachedAverage
cvglmobj <- cv.glmnet(logisticmatrix,ReachedAverage,family='binomial')
glmmodel=glmnet(logisticmatrix,ReachedAverage,family='binomial',lambda=cvglmobj$lambda.1se)
glmmodel$beta
prediction <- predict(glmmodel,newx=logisticmatrix) >= 0.5
observation <- ReachedAverage>=0.5
Accuracy <- sum(prediction == observation)/dim(data)[1]
predictTRUE <- sum(prediction)/dim(data)[1]
observeTRUE <- sum(observation)/dim(data)[1]
WorseBound = 1.25*(predictTRUE*observeTRUE + (1-predictTRUE)*(1-observeTRUE))
### Housing and EDs has super huge number. Hmm interesting.
#########################################################
EDControlledData$log.ED.Cost.Scale <- log.ED.Cost.Scale
EDControlledData$ReachedAverage <- NULL
LMStateControlledMod <- lm(log.ED.Cost.Scale~.,data=EDControlledData)
summary(LMStateControlledMod)
stepLMStateControlledMod <- step(LMStateControlledMod,direction="both")
summary(stepLMStateControlledMod)
####
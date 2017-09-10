#Read car assignment file.Require conversion factor to numeric.So "stringsAsFactors=F" not used

carpricem<-read.csv("CarPrice_Assignment.csv")
str(carpricem)



#Data undestanding and Data cleansing

#Check Blank value
#Check NA
#Check Duplicate


sapply(carpricem, function(x) length(which(x == "")))
sum(is.na(carpricem))
sum(duplicated(carpricem$car_ID))

#Values above 95% quantile look to be outliers for car price


summary(carpricem$price)
quantile(carpricem$price,probs = seq(0.01,1,0.01))
boxplot(carpricem[carpricem$price<=quantile(carpricem$price,probs=0.95),]$price)

#There is a variable named CarName which is comprised of two parts - the first word is the name of 'car company' and the second is the 'car model'.Need to consider only company name as the independent variable for the model building

#option1

install.packages("tidyr")
library(tidyr)
carpricem1 <- separate(carpricem,CarName, into=c("Carcompany", "CarModel"), sep = " ")
carpricem1$Carcompany<-toupper(carpricem1$Carcompany)
carpricem1$Carcompany <- replace(carpricem1$Carcompany, carpricem1$Carcompany== "VOKSWAGEN","VOLKSWAGEN")
carpricem1$Carcompany <- replace(carpricem1$Carcompany, carpricem1$Carcompany== "VW","VOLKSWAGEN")
carpricem1$Carcompany <- replace(carpricem1$Carcompany, carpricem1$Carcompany== "TOYOUTA","TOYOTA")
carpricem1$Carcompany <- replace(carpricem1$Carcompany, carpricem1$Carcompany== "MAXDA","MAZDA")
carpricem1$Carcompany <- replace(carpricem1$Carcompany, carpricem1$Carcompany== "PORCSHCE","PORSCHE")



##option2
#install.packages("stringr")
#library(stringr)
#carpricem1$Carcompany <- toupper(sapply(strsplit(as.character(carpricem1$CarName),"\\ "),fun<- function(x) x[1]))

# Create the dummy variable for Carcompany variable

dummy_1 <- data.frame(model.matrix( ~Carcompany, data = carpricem1))
View(dummy_1)
dummy_1 <- dummy_1[,-1]
carpricem1_1 <- cbind(carpricem1[,-3], dummy_1)
View(carpricem1_1)

#convert factors with 2 levels to numerical variables
#fueltype
#aspiration
#doornumber
#enginelocation

carpricem1_2<-carpricem1_1
levels(carpricem1_2$fueltype)<-c(1,0)
carpricem1_2$fueltype<- as.numeric(levels(carpricem1_2$fueltype))[carpricem1_2$fueltype]

levels(carpricem1_2$aspiration)<-c(1,0)
carpricem1_2$aspiration<- as.numeric(levels(carpricem1_2$aspiration))[carpricem1_2$aspiration]

levels(carpricem1_2$doornumber)<-c(1,0)
carpricem1_2$doornumber<- as.numeric(levels(carpricem1_2$doornumber))[carpricem1_2$doornumber]

levels(carpricem1_2$enginelocation)<-c(1,0)
carpricem1_2$enginelocation<- as.numeric(levels(carpricem1_2$enginelocation))[carpricem1_2$enginelocation]

# Create the dummy variable for below independent variable 
#carbody
#drivewheel
#enginetype
#cyclindernumber
#fuelsystem

dummy_2 <- data.frame(model.matrix( ~carbody, data = carpricem1))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
carpricem1_3 <- cbind(carpricem1_2[,-7], dummy_2)

dummy_3 <- data.frame(model.matrix( ~drivewheel, data = carpricem1))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
carpricem1_4 <- cbind(carpricem1_3[,-7], dummy_3)


dummy_4 <- data.frame(model.matrix( ~enginetype, data = carpricem1))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
carpricem1_5 <- cbind(carpricem1_4[,-13], dummy_4)

dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = carpricem1))
View(dummy_5)
dummy_5 <- dummy_5[,-1]
carpricem1_6 <- cbind(carpricem1_5[,-13], dummy_5)

dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = carpricem1))
View(dummy_6)
dummy_6 <- dummy_6[,-1]
carpricem1_7 <- cbind(carpricem1_6[,-14], dummy_6)

#Remove car_ID and CarModel from the data set
carpricem1_8<-carpricem1_7
carpricem1_8<-carpricem1_8[,-1]
carpricem1_8<-carpricem1_8[,-2]


#derive matrics:Vehicle Horsepower to Weight Ratio is commonly applied to engines and mobile power sources, to compare one unit or design to the other.It is used to measure the vehicle's performance(accelaration) with engine power output, divided by its mass to find the size of the vehicle.Vehicle hp to weight ratio is usually calculated using curb weight or wet weight.

carpricem1_8$hptowt<-carpricem1_8$horsepower/carpricem1_8$curbweight
str(carpricem1_8)


# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carpricem1_8), 0.7*nrow(carpricem1_8))
train = carpricem1_8[trainindices,]
test = carpricem1_8[-trainindices,]


# Build model 1 containing all variables :Adjusted R-Squard:97% and hptowt(derive metrics has significant to price with negative coefficents)

model_1 <-lm(price~.,data=train)
summary(model_1)

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

install.packages("MASS")
library(MASS)

# We have a total of 66 variables considered into the model 

step <- stepAIC(model_1, direction="both")
step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables - symboling, fueltype,doornumber etc. 


model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + curbweight + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + CarcompanySAAB + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
    fuelsystemmpfi + fuelsystemspdi + hptowt, data = train)

summary(model_2)

# Let us check for multicollinearity (VIF)

install.packages("car")
library(car)

vif(model_2)

#Adjusted R-squared:  0.9764 .hptowt and horsepower  has higher VIF but p value is low.Whereas cylindernumbersix  is VIF=29.408340 but high p value 0.096701. so #eliminate cylindernumbersix from the model


model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + curbweight + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + CarcompanySAAB + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    cylindernumberfour + fuelsystem2bbl + 
    fuelsystemmpfi + fuelsystemspdi + hptowt, data = train)

summary(model_3)
vif(model_3)

#adjusted R-squard 97%,curbweight has VIF:80.869978 but has very high p value 0.122952 .so remove curbweight from next model.

model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + CarcompanySAAB + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    cylindernumberfour + fuelsystem2bbl + 
    fuelsystemmpfi + fuelsystemspdi + hptowt, data = train)

summary(model_4)
vif(model_4)

#adjusted R-squard 97%,cylindernumberfour has VIF:21.987993 but has very high p value 0.080802 .so remove cylindernumberfour from next model.

model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + CarcompanySAAB + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + fuelsystemspdi + hptowt, data = train)

summary(model_5)

vif(model_5)


#adjusted R-squard 97%,fuelsystemspdi has VIF: 2.765693 but has very high p value 0.083985 .so remove fuelsystemspdi from next model.

model_6 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + CarcompanySAAB + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_6)

vif(model_6)


#adjusted R-squard 97%,enginetypeohc has VIF: 7.105885 but has very high p value 0.084111 .so remove enginetypeohc from next model.

model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + CarcompanySAAB + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd +  enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_7)

vif(model_7)


#adjusted R-squard 97%,CarcompanySAAB has VIF: 2.007900 slightly higher than 2 but has very high p value 0.420588.so remove CarcompanySAAB from next model.

model_8 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK + CarcompanyCHEVROLET + CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd +  enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_8)

vif(model_8)

#adjusted R-squard 97%,CarcompanyCHEVROLET has VIF less than 2 slightly  but has very high p value 0.532356.so remove CarcompanyCHEVROLET from next model.

model_9 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyHONDA + CarcompanyISUZU + CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd +  enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_9)

vif(model_9)


#adjusted R-squard 97%,CarcompanyISUZU has VIF less than 2 slightly  but has very high p value 0.051894.so remove CarcompanyISUZU from next model.

model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + horsepower + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyHONDA +  CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd +  enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_10)

vif(model_10)

#adjusted R-squard 97%,horsepower has   VIF with significant p value .so remove horsepower from next model.

model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyHONDA +  CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    drivewheelrwd +  enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_11)
vif(model_11)

#adjusted R-squard 97%,drivewheelrwd has VIF= 3.696685  with p value less significant compare to other variable .so remove drivewheelrwd from next model.

model_12 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyHONDA +  CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor + cylindernumberfive + 
    fuelsystem2bbl + 
    fuelsystemmpfi + hptowt, data = train)

summary(model_12)
vif(model_12)


#adjusted R-squard 97%, cylindernumberfive has VIF within range  but p value less significant compare to other variable .so remove cylindernumberfive from next model.

model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyHONDA +  CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_13)
vif(model_13)

#adjusted R-squard 97%, CarcompanyHONDA has VIF =3.488510  but p value less significant compare to other variable .so remove CarcompanyHONDA from next model.

model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanySUBARU + CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_14)
vif(model_14)

#adjusted R-squard 96.8%, CarcompanySUBARU p value less significant compare to other variable .so remove CarcompanySUBARU from next model.

model_15 <- lm(formula = price ~ aspiration + enginelocation + carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_15)
vif(model_15)

#adjusted R-squard 96.7%, enginelocation has VIF=5.946759 but p is less significant compare to other variable .so remove enginelocation from next model.

model_16 <- lm(formula = price ~ aspiration +  carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMAZDA + CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_16)
vif(model_16)

#adjusted R-squard 96.6%, CarcompanyMAZDA p value  is less significant compare to other variable .so remove CarcompanyMAZDA from next model.

model_17 <- lm(formula = price ~ aspiration +  carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN + CarcompanyPEUGEOT + CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_17)
vif(model_17)

#adjusted R-squard 96%, CarcompanyPEUGEOT p value  is less significant compare to other variable .so remove CarcompanyPEUGEOT from next model.

model_18 <- lm(formula = price ~ aspiration +  carwidth  + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_18)
vif(model_18)

#adjusted R-squard 95.9%, carwidth  p value  is less significant compare to other variable .so remove carwidth  from next model.

model_19 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_19)
vif(model_19)

#adjusted R-squard 95.9%, carbodysedan  p value  is less significant compare to other variable .so remove carbodysedan  from next model.

model_20 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback + carbodywagon + 
    enginetyperotor  +fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_20)
vif(model_20)





#adjusted R-squard 95.8%, carbodywagon  p value  is less significant compare to other variable .so remove carbodywagon  from next model.

model_21 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + carbodyhatchback  + 
    enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_21)

vif(model_21)

#adjusted R-squard 95.8%, carbodyhatchback  p value  is less significant compare to other variable .so remove carbodyhatchback   from next model.

model_22 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    carbodyhardtop + enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_22)

vif(model_22)

#adjusted R-squard 95.8%, carbodyhardtop  p value  is less significant compare to other variable .so remove carbodyhardtop   from next model.

model_23 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA + CarcompanyVOLKSWAGEN + 
    enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_23)

vif(model_23)

#adjusted R-squard 95.8%, CarcompanyVOLKSWAGEN  p value  is less significant compare to other variable .so remove CarcompanyVOLKSWAGEN   from next model.

model_24 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyNISSAN +  CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA  + enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_24)

vif(model_24)

#adjusted R-squard 95.6%, CarcompanyNISSAN  p value  is less significant compare to other variable .so remove CarcompanyNISSAN   from next model.

model_25 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    CarcompanyTOYOTA  + enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_25)

vif(model_25)

#adjusted R-squard 95.3%, CarcompanyTOYOTA  p value  is less significant compare to other variable .so remove CarcompanyTOYOTA   from next model.

model_26 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyPLYMOUTH + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
     enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_26)

vif(model_26)

#adjusted R-squard 95.1%, CarcompanyPLYMOUTH  p value  is less significant compare to other variable .so remove CarcompanyPLYMOUTH   from next model.

model_27 <- lm(formula = price ~ aspiration   + 
    enginesize + boreratio + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
     enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_27)

vif(model_27)

#adjusted R-squard 94.8%, boreratio  p value  is less significant compare to other variable .so remove boreratio  from next model.

model_28 <- lm(formula = price ~ aspiration   + 
    enginesize + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyMITSUBISHI + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
     enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_28)

vif(model_28)

#adjusted R-squard 94.5%, CarcompanyMITSUBISHI p value  is less significant compare to other variable .so remove CarcompanyMITSUBISHI  from next model.

model_29 <- lm(formula = price ~ aspiration   + 
    enginesize + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +  CarcompanyDODGE + 
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_29)

vif(model_29)

#adjusted R-squard 94.3%, CarcompanyDODGE p value  is less significant compare to other variable .so remove CarcompanyDODGE  from next model.

model_30 <- lm(formula = price ~ aspiration + 
    enginesize + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_30)

vif(model_30)

#adjusted R-squard 94%, CarcompanyDODGE p value  is less significant compare to other variable .so remove CarcompanyDODGE  from next model.

model_30 <- lm(formula = price ~ aspiration + 
    enginesize + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    enginetyperotor  + fuelsystem2bbl +  
    fuelsystemmpfi + hptowt, data = train)

summary(model_30)

vif(model_30)



#adjusted R-squard 94%, fuelsystem2bbl p value  is less significant compare to other variable .so remove fuelsystem2bbl  from next model.

model_31 <- lm(formula = price ~ aspiration + 
    enginesize + peakrpm + CarcompanyBMW + 
    CarcompanyBUICK +
    CarcompanyPORSCHE + CarcompanyRENAULT + 
    enginetyperotor +  fuelsystemmpfi + hptowt, data = train)

summary(model_31)

vif(model_31)

#Model_31: adjusted R-squard 93%, p value for independent variable within model_31  is quite significant(less than 0.001) .some of the variable has negative coefficient but based on key performance metrics

# Predict the car prices in the testing dataset,[,-1] for price(depedent)

Predict_1 <- predict(model_31,test[,-1])
test$predictedtest_price <- Predict_1

#calculate Error to find the randomness of prediction to evaluate any requiste variable missed or not

test$error <-  test$price - test$predictedtest_price

# Accuracy of the predictions-Calculate correlation

rsquared <- cor(test$price,test$predictedtest_price)^2

# check R-squared :0.837;84% quite accurate model











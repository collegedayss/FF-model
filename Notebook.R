library(readr)
IBM <- read_csv("IBM.csv")
str(IBM)
length(IBM$Date)
?as.Date()
dates <- c(IBM$Date)
as.Date(dates, "%m/%d/%Y")
IBMvec <- c(IBM$`Adj Close`)
Return <- c(diff(log(IBMvec), lag = 1), NA)
newIBM <- cbind.data.frame(IBM, Return)
head(newIBM)
numIBM <- newIBM[ ,sapply(newIBM,is.numeric)]
apply(numIBM, 2, quantile, probs = 0.10, na.rm = TRUE)
ffdf <- read.csv("F-F_Research_Data_Weekly.csv",header = TRUE)

#Getting Number of Rows of Dataset.
Size <- dim(ffdf)[1]
Size
# combine dataframes

allData = cbind(IBM, ffdf)


#Remove last row for Dataset.
allData <- allData[-c(523),]
#New Dimensions of Dataset
size_of_data <- dim(allData)
size_of_data
#add new row to dataset
allData$ExcessReturn <- allData[,'Volume'] - allData[,'RF']
head(allData[,'ExcessReturn'])
#create two plots on One graph
par(mfrow=c(1,2))
hist(allData[,'Mkt.RF'],main="Market Returns")
hist(allData[,'ExcessReturn'],main="Company Excess Returns")
#Create Scatter Plot
par(mfrow=c(1,1))
plot(allData$Mkt.RF, allData$ExcessReturn, ylab="Company Excess Returns", xlab="Market Returns")
#Corelation Coefficient
Correlation_coefficient <- cor(allData$ExcessReturn,allData$Mkt.RF)
Correlation_coefficient

# create a linear model
CAPM = lm(ExcessReturn ~ Mkt.RF, data = allData)
summary(CAPM)

# add a fitted line
abline(CAPM)

# diagnostic plots - outliers at 270, 426, 480
# plot(CAPM)

# remove outliers - R-squared increased, indicating improved model
allDataNoOutliers = allData[-c(270, 426, 480),]
CAPM2 = lm(ExcessReturn ~ Mkt.RF, data = allDataNoOutliers)
summary(CAPM2)

# Cooks Distance - high at 37, 39, and 102
myCDs = cooks.distance(CAPM2)
sort(round(myCDs, 5))
# plot(CAPM2, pch = 18, which=c(4))
allDataNoHighCD = allDataNoOutliers[-c(37,39,102),]
CAPM3 = lm(ExcessReturn ~ Mkt.RF, data = allDataNoHighCD)
summary(CAPM3)

# non-linear model - residuals have parabola form so poly of 2 - improved QQ plot
CAPM4 = lm(ExcessReturn ~ poly(Mkt.RF,2), data = allDataNoOutliers)
# plot(CAPM4)

# final adjusted model
allDataAdjusted = allDataNoHighCD[-c(439,103),]
FinalModel = lm(ExcessReturn ~ poly(Mkt.RF, 2), data = allDataAdjusted)
summary(FinalModel)

# hypothesis test
# H0: beta1 = 0
# H1: beta1 =/= 0
summary(FinalModel)
# p-value = 3.49e-06 so Reject H0 - there is likely a relationship between Company and Market returns


# predictions when x = -7, 0.3, 4

newdata <- data.frame(Mkt.RF = c(-7))
predict(FinalModel, newdata)

newdata <- data.frame(Mkt.RF = c(0.3))
predict(FinalModel, newdata)

newdata <- data.frame(Mkt.RF = c(4))
predict(FinalModel, newdata)



#linear model for company’s excess return on the market excess return, size
#premium (SMB) and value premium (HML), i.e., Fama-French Three-Factor Model.
mlr <- lm(ExcessReturn ~ SMB + `Mkt.RF` + HML , data = allData)

#diagnostic plots
par(mfrow = c(2, 2))  # display plots in a 2*2 layout
plot(mlr)

#in the first graph, the red line is relatively horizontal at 0, no adjustment necessarily needed
#therefore we can assume linearity

#The data points are not evenly spread in the second graph, scale-location, therefore to account for 
#homogeneity of variance, we can transform the response variable by squaring it,
#now we can assume heteroscedasticity

#The third graph shows that the point corresponds fairly closely to the diagonal
#line therefore we can assume normality

mlr <- lm(ExcessReturn^(-2) ~ SMB + Mkt.RF + HML , data = allData)

#check for influential points
influencePlot(mlr, main="Influence Plot", sub="Circle size is proportional to Cook's Distance" )
outlierTest(mlr) # Bonferroni p-value for most extreme obs

#looks like there are influential points as 270, 377, 480 and 487
#480 and 270 are outliers and 487 and 377 are high leverage points

allDataNoOutliers = allData[-c(487,377, 1, 384),]

mlr <- lm(ExcessReturn^(-2) ~ SMB + Mkt.RF + HML , data = allDataNoOutliers)

#check for collinearity
vif(mlr) # variance inflation factors
#all good

#new  plots
par(mfrow = c(2, 2))  # display plots in a 2*2 layout
plot(mlr)


# everything looks good and all of our assumptions check out

#checking to see if any predictors have an effect of IBM's excess return based on p values
summary(mlr)
#SMB has an insignificant effect on IBM's market excess return
#Mkt.RF has an insignificant effect on IBM's market excess return
#HML has an insignificant effect on IBM's market excess return

anova(mlr)

#comparing full model to restricted model
mlr_restricted <- lm(ExcessReturn^(-2) ~ Mkt.RF + HML , data = allDataNoOutliers)
anova(mlr_restricted)

#Based on the full model, obtain the prediction intervals for the company’s excess return for given values of market excess = -2, SMB = 3, HML = -0.06.
newdata = data.frame(SMB=3, Mkt.RF=-2, HML=-0.06)
predict(mlr, newdata, interval="predict") 





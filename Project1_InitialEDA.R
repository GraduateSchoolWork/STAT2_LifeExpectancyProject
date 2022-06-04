################################################################################
################################################################################
###############################      Project 1     #############################
################################################################################
################################################################################

# load libraries
library(tidyverse)
library(GGally)
library(leaps)
library(olsrr)
library(car)
library(naniar) #Added by Shikha

# load and look at the data
#lifeExpectancy <- read.csv("~/SMU/Classes/Applied Statistics II/Project/Life Expectancy Data.csv") # Commented by Shikha
lifeExpectancy <- read.csv("Life Expectancy Data.csv", header = T) #Added by Shikha
head(lifeExpectancy, n=30)
summary(lifeExpectancy)
str(lifeExpectancy)

t(aggregate(Life.expectancy~Status,data=lifeExpectancy,summary)) #Added by Shikha

# look at some of the na's
lifeExpectancy[which(is.na(lifeExpectancy$Alcohol)),]
lifeExpectancy[which(is.na(lifeExpectancy$Population)),]
lifeExpectancy[which(is.na(lifeExpectancy$GDP)),]

#Looking for missing values in dataset - Added by Shikha
gg_miss_var(lifeExpectancy) + labs(title = 'Missing Values', x = 'Dataset Columns')
sapply(lifeExpectancy, function(x) sum(is.na(x)))

# omit na's if needed ??????????
lifeExpectancy <- na.omit(lifeExpectancy)
nrow(lifeExpectancy)

# see how many contries there are
unique(lifeExpectancy$Country)

# look at outliers (India would be omitted it the highest infant deaths are omitted)
lifeExpectancy[which(lifeExpectancy$infant.deaths > 500),]

################################################################################
################################################################################
##################################      EDA     ################################
################################################################################
################################################################################

# plot many variables against each other 
lifeExpectancy %>% 
  select(Life.expectancy, Year, Adult.Mortality, BMI, Status) %>%
  ggpairs(aes(color = Status, alpha = 0.5))

lifeExpectancy %>% 
  select(Life.expectancy, Adult.Mortality, BMI, Status, infant.deaths, GDP) %>%
  ggpairs(aes(color = Status, alpha = 0.5))

lifeExpectancy %>% 
  select(Life.expectancy, Adult.Mortality, BMI, Schooling, Status, GDP, Alcohol, thinness..1.19.years, Income.composition.of.resources) %>%
  ggpairs(aes(color = Status, alpha = 0.5))

lifeExpectancy %>% 
  select(Life.expectancy,Adult.Mortality,Schooling,Year,HIV.AIDS,Measles,Diphtheria,infant.deaths,thinness..1.19.years,Status) %>%
  ggpairs(aes(color = Status, alpha = 0.5))

ggpairs(lifeExpectancy, columns = 2:6)
ggpairs(lifeExpectancy, columns = 12:16)
ggpairs(lifeExpectancy, columns = 3:10)
ggpairs(lifeExpectancy, columns = 12:21)

# look at specific variables 
ggplot(lifeExpectancy, aes(x=Life.expectancy, fill=Status)) +
  geom_density(alpha = 0.5) +
  ggtitle("Distribution of Life Expectancy for Developed and Developing Countries") +
  theme_classic()
ggplot(lifeExpectancy, aes(x=Status, y=GDP, fill = Status)) +
  geom_boxplot() +
  ggtitle("Distribution of GDP for Developed and Developing Countries") +
  theme_classic()
ggplot(lifeExpectancy, aes(x=Status, y=Life.expectancy, fill = Status)) +
  geom_boxplot() +
  ggtitle("Distribution of Life Expectancy for Developed and Developing Countries") +
  theme_classic()

ggplot() +
  geom_density(data = lifeExpectancy, aes(x=HIV.AIDS), fill = "darkorange", alpha = 0.5) +
  geom_density(data = lifeExpectancy, aes(x=Measles), fill = "darkblue", alpha = 0.5) +
  geom_density(data = lifeExpectancy, aes(x=Diphtheria), fill = "steelblue", alpha = 0.5) +
  xlim(0,10) +
  theme_classic() #Added by Shikha
  

# determine the specific mean and median GDP for developing and developed countries
developed <- lifeExpectancy %>% filter(Status == "Developed")
developing <- lifeExpectancy %>% filter(Status == "Developing")
mean(developed$GDP)
median(developed$GDP)
mean(developing$GDP)
median(developing$GDP)

summary(developed$GDP) #Added by Shikha
summary(developing$GDP) #Added by Shikha

# calculate pearson correlation between specific variables
cor.test(lifeExpectancy$Life.expectancy, lifeExpectancy$Alcohol, method = "pearson")

#######################################################
#############  Start - Added by Shikha  ###############
#######################################################

# Calculate correlation matrix
lifeExpectancy <- lifeExpectancy[,-c(1)] # Removing Country, Status
cor(lifeExpectancy)
# Only under.five.deaths and infant.deaths are highly correlated

# make linear regression model to check for VIFs
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # Really high VIF for infant.deaths (213.609554) and under.five.deaths (203.591034)

# linear regression model after removing infant.deaths to check for VIFs
lifeExpectancy <- lifeExpectancy[,-4] # Removing infant.deaths
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # GDP is still at 13.647857 > 10

lifeExpectancy <- lifeExpectancy[,-14] # Removing GDP
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # All VIFs under 10 now but lets take out thinness.1.19 as well

lifeExpectancy <- lifeExpectancy[,-15] # Removing GDP
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # All VIFs under 10 now

#######################################################
##############  End - Added by Shikha  ###############
######################################################

################################################################################
################################################################################
##############################      Modeling     ###############################
################################################################################
################################################################################

# Objective 1: Build regression models and identify key relationships and observe those relationships.
#lifeExpectancy <- read.csv("~/SMU/Classes/Applied Statistics II/Project/Life Expectancy Data.csv") # Commented by Shikha
lifeExpectancy <- read.csv("Life Expectancy Data.csv", header = T) #Added by Shikha

splitPerc = .85
trainIndices = sample(1:dim(lifeExpectancy)[1],round(splitPerc * dim(lifeExpectancy)[1]))
train = lifeExpectancy[trainIndices,]
test = lifeExpectancy[-trainIndices,]
#train<-na.omit(train)


# make linear regression model
model1 <- lm(Life.expectancy~.,data=train)
summary(model1)
plot(model1)

# run forward, backward, and step-wise model selection
m1 <- ols_step_forward_p(model1)
m1
plot(m1)
m2 <- ols_step_backward_p(model1)
m2
plot(m2)
m3 <- ols_step_both_p(model1)
m3
plot(m3)

# this model is what came out from the step-wise model
#model2 <- lm(Life.expectancy~Adult.Mortality+Schooling+Year+HIV.AIDS+Measles+Diphtheria+infant.deaths+thinness..1.19.years,data=train) # Commented by Shikha
model2 <- lm(Life.expectancy~Adult.Mortality+Income.composition.of.resources+Schooling+HIV.AIDS+Diphtheria+percentage.expenditure+BMI+Polio+under.five.deaths,data=train) # Added by Shikha
summary(model2)
plot(model2)
vif(model2)

# model 3 - Intuition
model3 <- lm(Life.expectancy~Adult.Mortality+BMI+Schooling+Status+GDP+Alcohol+thinness..1.19.years, data = train)
summary(model3)
plot(model3)

# model metrics
hist(model2$residuals, col = "darkslateblue", main = "Histogram of Residuals")
sqrt(sum((model2$residuals)^2)) # Just the comment added by Shikha - value for this is 187.0329

# predictions
preds <- predict(model2, newdata = test, interval = "prediction")
preds <- as.data.frame(preds)
test$Life.expectancy2 <- preds[,1]
#preds2 <- test[,c(2,4,23)] # Commented by Shikha
preds2 <- test[,c(2,18)] # Added by Shikha
head(preds2)

ggplot() +
  geom_line(data = preds2, aes(x=Year, y=Life.expectancy)) +
  geom_line(data = preds2, aes(x=Year, y=Life.expectancy2), color = "Red")

ggplot() +
  geom_density(data = preds2, aes(x=Life.expectancy)) +
  geom_density(data = preds2, aes(x=Life.expectancy2), color = "Red") +
  ggtitle("Predictions vs Actual Values") +
  xlab("Life Expectancy") +
  theme_classic()


################################################################################

library(leaps)
# run forward selection with different function
train <- train[,-3]
test <- test[,-3]
#reg.fwd <- regsubsets(Life.expectancy~.,data = train, method = "forward", nvmax = 20) # Commented by Shikha
reg.fwd <- regsubsets(Life.expectancy~.,data = train, method = "forward", nvmax = 18) # Added by Shikha

# summary statistics from forward model
par(mfrow=c(1,3))
bics<-summary(reg.fwd)$bic
plot(1:20,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)

adjr2<-summary(reg.fwd)$adjr2
plot(1:20,adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)

rss<-summary(reg.fwd)$rss
plot(1:20,rss,type="l",ylab="train RSS",xlab="# of predictors")
index<-which(rss==min(rss))
points(index,rss[index],col="red",pch=10)

################################################################################

# LASSO
library(glmnet)

lifeExpectancy <- read.csv("~/SMU/Classes/Applied Statistics II/Project/Life Expectancy Data.csv")
splitPerc = .85
trainIndices = sample(1:dim(lifeExpectancy)[1],round(splitPerc * dim(lifeExpectancy)[1]))
train = lifeExpectancy[trainIndices,]
test = lifeExpectancy[-trainIndices,]
train <- na.omit(train)
test <- na.omit(test)

#Formatting data for GLM net
x = model.matrix(Life.expectancy~.,train)
y = train$Life.expectancy

xtest <- model.matrix(Life.expectancy~.,test)
ytest <- test$Life.expectancy

grid = 10^seq(10,-2, length=100)
lasso.mod = glmnet(x,y,alpha=1, lambda=grid)
cv.out = cv.glmnet(x,y,alpha=1)                                                 #alpha=1 performs LASSO
plot(cv.out)
bestlambda <- cv.out$lambda.min                                                 #Optimal penalty parameter.  You can make this call visually.

lasso.pred <- predict(lasso.mod, s=bestlambda, newx=xtest)

testMSE_LASSO <- mean((ytest-lasso.pred)^2)
testMSE_LASSO

coef(lasso.mod,s=bestlambda)


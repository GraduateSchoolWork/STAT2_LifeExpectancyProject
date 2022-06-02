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

# load and look at the data
lifeExpectancy <- read.csv("~/SMU/Classes/Applied Statistics II/Project/Life Expectancy Data.csv")
head(lifeExpectancy, n=30)
summary(lifeExpectancy)
str(lifeExpectancy)

# look at some of the na's
lifeExpectancy[which(is.na(lifeExpectancy$Alcohol)),]
lifeExpectancy[which(is.na(lifeExpectancy$Population)),]
lifeExpectancy[which(is.na(lifeExpectancy$GDP)),]

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

ggpairs(lifeExpectancy, columns = 2:6)
ggpairs(lifeExpectancy, columns = 12:16)
ggpairs(lifeExpectancy, columns = 3:10)
ggpairs(lifeExpectancy, columns = 12:21)

# look at specific variables 
ggplot(lifeExpectancy, aes(x=Life.expectancy, fill=Status, alpha = 0.5)) +
  geom_density()
ggplot(lifeExpectancy, aes(x=Status, y=GDP, fill = Status)) +
  geom_boxplot()

# determine the specific mean and median GDP for developing and developed countries
developed <- lifeExpectancy %>% filter(Status == "Developed")
developing <- lifeExpectancy %>% filter(Status == "Developing")
mean(developed$GDP)
median(developed$GDP)
mean(developing$GDP)
median(developing$GDP)

# calculate pearson correlation between specific variables
cor.test(lifeExpectancy$Life.expectancy, lifeExpectancy$Alcohol, method = "pearson")


################################################################################
################################################################################
##############################      Modeling     ###############################
################################################################################
################################################################################

# Objective 1: Build regression models and identify key relationships and observe those relationships.
lifeExpectancy <- read.csv("~/SMU/Classes/Applied Statistics II/Project/Life Expectancy Data.csv")

splitPerc = .85
trainIndices = sample(1:dim(lifeExpectancy)[1],round(splitPerc * dim(lifeExpectancy)[1]))
train = lifeExpectancy[trainIndices,]
test = lifeExpectancy[-trainIndices,]
#train<-na.omit(train)


# make linear regression model
model1 <- lm(Life.expectancy~.,data=lifeExpectancy)
summary(model1)
plot(model1)

# run forward, backward, and step-wise model selection
ols_step_forward_p(model1)
plot(ols_step_forward_p(model1))
ols_step_backward_p(model1)
plot(ols_step_backward_p(model1))
ols_step_both_p(model1)
plot(ols_step_both_p(model1))

# this model is what came out from the step-wise model
model2 <- lm(Life.expectancy~Country+Year+HIV.AIDS+Schooling+thinness.5.9.years+Alcohol+Hepatitis.B,data=lifeExpectancy)
summary(model2)
plot(model2)
vif(model2)

################################################################################

# run forward selection with different function
reg.fwd <- regsubsets(Life.expectancy~.,data=train,method="forward",nvmax=20)




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
library(naniar)

# load and look at the data
lifeExpectancy <- read.csv("Life Expectancy Data.csv", header = T)
head(lifeExpectancy, n=30)
summary(lifeExpectancy)
str(lifeExpectancy)

#Looking for missing values in dataset - Added by Shikha
gg_miss_var(lifeExpectancy) + labs(title = 'Missing Values', x = 'Dataset Columns')
sapply(lifeExpectancy, function(x) sum(is.na(x)))


################################################################################
################################################################################
##################################      EDA     ################################
################################################################################
################################################################################

lifeExpectancy %>% 
  select(Life.expectancy, Adult.Mortality, BMI, Schooling, Status, GDP, Alcohol, thinness..1.19.years, Income.composition.of.resources) %>%
  ggpairs(aes(color = Status, alpha = 0.5))

lifeExpectancy %>% 
  select(Life.expectancy,Adult.Mortality,Schooling,Year,HIV.AIDS,Measles,Diphtheria,infant.deaths,thinness..1.19.years,Status) %>%
  ggpairs(aes(color = Status, alpha = 0.5))


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
  theme_classic()


################################################################################
################################################################################
##############################      Modeling     ###############################
################################################################################
################################################################################

# Check VIF for all variables excluding Country 
lifeExpectancy <- read.csv("Life Expectancy Data.csv", header = T)
lifeExpectancy <- lifeExpectancy[,-1] # Removing Country

# Make linear regression model to check for VIFs
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2 # Really high VIF for infant.deaths (213.611125) and under.five.deaths (203.591539)

# linear regression model after removing infant.deaths to check for VIFs
lifeExpectancy <- lifeExpectancy[,-5] # Removing infant.deaths
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # GDP is still at 13.709051 > 10

lifeExpectancy <- lifeExpectancy[,-15] # Removing GDP
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # All VIFs under 10 now but lets take out thinness.1.19 (7.607806) as well

lifeExpectancy <- lifeExpectancy[,-16] # Removing thinness.1.19
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel) # All VIFs way under 10 now

# Check summary statistics
summary(VIFmodel)


# Objective 1: Build regression models and identify key relationships and observe those relationships.


splitPerc = .85
trainIndices = sample(1:dim(lifeExpectancy)[1],round(splitPerc * dim(lifeExpectancy)[1]))
train = lifeExpectancy[trainIndices,]
test = lifeExpectancy[-trainIndices,]

# make linear regression model
model1 <- lm(Life.expectancy~.,data=train)
summary(model1)
plot(model1)

# run step-wise model selection
m3 <- ols_step_both_p(model1)
m3
plot(m3)

# this model is what came out from the step-wise model

model2 <- lm(Life.expectancy~Adult.Mortality+Income.composition.of.resources+Schooling+HIV.AIDS+Diphtheria+percentage.expenditure+BMI+Polio+Status+Measles,data=train) # Added by Shikha
summary(model2)
plot(model2)
vif(model2)

# model metrics
hist(model2$residuals, col = "darkslateblue", main = "Histogram of Residuals")
sqrt(sum((model2$residuals)^2)) # Just the comment added by Shikha - value for this is 189.1191

# predictions
preds <- predict(model2, newdata = test, interval = "prediction")
preds <- as.data.frame(preds)
test$Life.expectancy2 <- preds[,1]
preds2 <- test[,c(3,19)]
head(preds2)

ggplot() +
  geom_density(data = preds2, aes(x=Life.expectancy)) +
  geom_density(data = preds2, aes(x=Life.expectancy2), color = "Red") +
  ggtitle("Predictions vs Actual Values") +
  xlab("Life Expectancy") +
  theme_classic()










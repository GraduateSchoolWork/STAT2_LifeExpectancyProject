################################################################################
################################################################################
##############################      Modeling     ###############################
################################################################################
################################################################################

# Check VIF for all variables excluding Country 
lifeExpectancy <- read.csv("Life Expectancy Data.csv", header = T)
lifeExpectancy <- lifeExpectancy[,-3] # Removing Status

# Make linear regression model to check for VIFs
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2 # Really high VIF for infant.deaths (2087.193026) and under.five.deaths (1892.225888)

# linear regression model after removing infant.deaths to check for VIFs
lifeExpectancy <- lifeExpectancy[,-5] # Removing infant.deaths
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2

lifeExpectancy <- lifeExpectancy[,-10] # Removing under.five.deaths
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2 # All VIFs under 10 now but lets take out thinness.1.19 (7.607806) as well

lifeExpectancy <- lifeExpectancy[,-19] # Removing Schooling
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2 

lifeExpectancy <- lifeExpectancy[,-6] # Removing percentage.expenditure
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2

lifeExpectancy <- lifeExpectancy[,-15] # Removing thinness..1.19.years
VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
vif(VIFmodel)[,3]^2

# lifeExpectancy <- lifeExpectancy[,-5] # Removing Alcohol
# VIFmodel <- lm(Life.expectancy~.,data=lifeExpectancy)
# vif(VIFmodel)[,3]^2


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










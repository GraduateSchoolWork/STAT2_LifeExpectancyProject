# STAT2_LifeExpectancyProject
Regression Project with Kaggle Life Expectancy Dataset


## Objective 1: Display the ability to build regression models using the skills and discussions from Unit 1 and 2 with the purpose of identifying key relationships and interpreting those relationships.

•	Build a model with the main goal to identify key relationships and that is highly interpretable.  Provide detailed information on summary statistics, EDA, and your model building process.
•	Provide interpretation of the regression coefficients of your final model including hypothesis testing, interpretation of regression coefficients, and confidence intervals. It’s also good to mention the Practical vs Statistical significance of the predictors.  Answer any additional questions using your model that you deem are relevant.
•	The training data set can be used for EDA and model fitting while the test set can be used to help compare models to make a final call.


## Objective 2:  While your model from Objective 1 may be interpretable there may be some additional complexity that you could incorporate to your model so that it can predict better at the expense of interpretations.  The purpose of this objective is to go through a process to compare multiple models with the goal of developing a model that can predict the best and do well on future data.  

•	Use the training and test set to build at least one additional multiple linear regression model that attempts to find a model with additional complexity than the interpretable model of Objective 1.  The point here is to make sure we understand how to add complexity to a linear regression model.   Hint:  It’s not just including a model with predictors that you’ve eliminated from Objective 1.
•	I want you to use the ISLR text book below (and the google machine) and read up on one nonparametric technique to build a regression model.  I want you to select from k-nearest neighbors’ regression or regression trees. There is a chapter on trees in the ISLR book.  For information on knn regression, see Section 3.5.  It is important to know that knn can be applied to regression as well as classification problems.  Make sure your implementation in R is using the knn regression versions rather than the classification.  See me on this if you need help or reassurance.  You will use the training and test sets here to help determine the most appropriate tree or the most appropriate “k” in knn. 
• http://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf
•	At this point you should have at least 3 models, 2 linear regression models and 1 nonparameteric model.  For each of the three models, provide the primary measure of fit for comparisons:  test ASE or via CV.  You may also include additional metrics for completeness like R squared/Adjusted Rsquared, AIC, and BIC where applicable (remember these are only derived from the training set not the test set).  

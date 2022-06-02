# STAT2_LifeExpectancyProject
Regression Project with Kaggle Life Expectancy Dataset


## Objective 1: Display the ability to build regression models using the skills and discussions from Unit 1 and 2 with the purpose of identifying key relationships and interpreting those relationships.

•	Build a model with the main goal to identify key relationships and that is highly interpretable.  Provide detailed information on summary statistics, EDA, and your model building process.<br />
•	Provide interpretation of the regression coefficients of your final model including hypothesis testing, interpretation of regression coefficients, and confidence intervals. It’s also good to mention the Practical vs Statistical significance of the predictors.  Answer any additional questions using your model that you deem are relevant.<br />
•	The training data set can be used for EDA and model fitting while the test set can be used to help compare models to make a final call.<br />


## Objective 2:  While your model from Objective 1 may be interpretable there may be some additional complexity that you could incorporate to your model so that it can predict better at the expense of interpretations.  The purpose of this objective is to go through a process to compare multiple models with the goal of developing a model that can predict the best and do well on future data.  

•	Use the training and test set to build at least one additional multiple linear regression model that attempts to find a model with additional complexity than the interpretable model of Objective 1.  The point here is to make sure we understand how to add complexity to a linear regression model.   Hint:  It’s not just including a model with predictors that you’ve eliminated from Objective 1.<br />
•	I want you to use the ISLR text book below (and the google machine) and read up on one nonparametric technique to build a regression model.  I want you to select from k-nearest neighbors’ regression or regression trees. There is a chapter on trees in the ISLR book.  For information on knn regression, see Section 3.5.  It is important to know that knn can be applied to regression as well as classification problems.  Make sure your implementation in R is using the knn regression versions rather than the classification.  See me on this if you need help or reassurance.  You will use the training and test sets here to help determine the most appropriate tree or the most appropriate “k” in knn. <br />
•	At this point you should have at least 3 models, 2 linear regression models and 1 nonparameteric model.  For each of the three models, provide the primary measure of fit for comparisons:  test ASE or via CV.  You may also include additional metrics for completeness like R squared/Adjusted Rsquared, AIC, and BIC where applicable (remember these are only derived from the training set not the test set).  <br />


## Additional details

NOTE: ALL ANALYSIS MUST BE DONE IN SAS OR R and all code must be placed in the appendix of your report. Python is okay for quick formatting of data and data visualization, but analysis should be in R or SAS. <br />


Required Information and SAMPLE FORMAT <br />

PAGE LIMIT: I do not necessarily require a page limit, but you should definitely be shooting for no more than 8 pages written for the main report (not including graphics and codes).  It of course will blow up quite larger than that due to graphics, tables, and code but good projects are clear, concise, and to the point.  You do not need to show output for every model you considered.  (You may put supporting plots/charts/tables etc. in the appendix if you want, just make sure you label and reference them appropriately.). Effective communication is critical here.  <br />

The format of your paper (headers, sections, etc) is flexible although should contain the following information.   <br />

1.Introduction Required <br />

2.Data Description  Required <br />

3.Exploratory Data Analysis Required <br />

4.Addressing Objective 1:  Required <br />
•	Restatement of Problem and the overall approach to solve it  <br />

•	Model Selection<br />
		  	Type of Selection <br />
			Options: LASSO, RIDGE, ELASTIC NET,Stepwise, Forward, Backward,Manual / Intuition, A mix of all of the above.<br />
•	Checking Assumptions  <br />
			Residual Plots <br />
			Influential point analysis (Cook’s D and Leverage)<br />
•	Parameter Interpretation<br />
	       		Interpretation<br />           
	       		Confidence Intervals Not Required, but use if beneficial to the discussion.<br />
			
5.Addressing Objective 2: Required<br />
•	Restatement of problem and the overall objective  <br />

•	Description of the approach for building a complex regression model.  Feature selection must be used here if only manually model fitting approaches were used 		in Objective 1.<br />

•	Brief description of how the nonparametric tool works intuitively.  Can this model overfit?  How?    <br />

•	Comparison of model results  <br />
			Table of test ASE and any other relevant model fitting metrics. <br />
			Discussion and insight as to what the results suggest.  Why does one fit better than the other?  Or perhaps why does it appear that all the 			    models appear to be performing about the same? <br />
			
6.Final summary Required <br />
•	Quick recap of Objective 1 and Objective 2 findings <br />

•	Provide any additional details and comments on the implications of the models.  Scope of inference?  What other data would this model be good/poor to apply 	    to?   Problems/concerns with the data or data collection? What would you do if you have more time?  What else would you collect? etc.   <br />

7.Appendix  Required <br />

•	Well commented SAS/R Code.  You may send me a github link if you wish. <br />

•	Graphics and summary tables (Can be placed in the appendix or in the written report itself.) <br />

•	Make sure you include figure labels and reference them in the report if you are using an appendix to communicate figures. <br />

# bnpredict
## Imputation and Value Prediction

Missing data is a major problem for anyone working with large datasets. There have been many approaches to dealing with missing data, from list-wise deletion of missing data to imputation via multivariate logistic regression. This packages provides a framework to use Bayesian networks to predict, evaluate and impute missing or held-out data using network-based expectation maximization.  This method is particularly well suited for highly dimensional categorical data that doesn't behave well with traditional imputation approaches.

In the following vignette, I will walk through an example in which we:

1. Create the structure of a bayesian network 
2. Hold out 25% of the values of the `job` feature in the dataset, creating missing data to predict.
3. Train the bayesian network with the truncated dataset
4. Predict the "missing" values using Most Probable Explanation technique
5. Evaluate the model
6. Visualize the evaluation process
7. Impute the data 

See the example in the vignettes folder for further instruction and information

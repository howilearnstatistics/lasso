library('glmnet')
lasso <- function(x){
  df = x
  input = model.matrix(medv~. , df)[,-1]
  response = df$medv
  grid = 10^seq(10, -2, length = 100)
  set.seed(1)
  cv.out = cv.glmnet(input, response, alpha = 1) # Fit lasso model 
  bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
  lasso_model = glmnet(input, response, alpha = 1, lambda = grid)
  lasso_coef = predict(lasso_model, type = "coefficients", s = bestlam) #extract lasso coefficients
  parameter = data.frame(name = lasso_coef@Dimnames[[1]][lasso_coef@i + 1], coefficient = lasso_coef@x)
  features = as.character(parameter$name[-1]) #extract features from parameters
  features = gsub('[[:digit:]]+', '', features) #remove number from names of features
  return(features)
}


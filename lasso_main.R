library('glmnet')
lasso <- function(x,y){
  attach(x)
  response = y
  features = setdiff(colnames(x), response)
  formula = reformulate(termlabels = features, 
                        response = response)
  input = model.matrix(formula , x)
  grid = 10^seq(10, -2, length = 100)
  set.seed(1)
  cv.out = cv.glmnet(input, 
                     get(response), 
                     alpha = 1) # Fit lasso model 
  bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
  lasso_model = glmnet(input, 
                       get(response), 
                       alpha = 1, 
                       lambda = grid)
  lasso_coef = predict(lasso_model, 
                       type = "coefficients", 
                       s = bestlam) #extract lasso coefficients
  parameter = data.frame(name = lasso_coef@Dimnames[[1]][lasso_coef@i + 1], 
                         coefficient = lasso_coef@x)
  selected_features = as.character(parameter$name[-1]) #extract features from parameters
  selected_features = gsub('[[:digit:]]+', '', selected_features) #remove number from names of features
  lasso_formula = reformulate(termlabels = selected_features,
                              response = response)
  return(lasso_formula)
}


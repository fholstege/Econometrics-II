
# function to get a balanced df 
get_balanced_df <- function(sIndex_var, sTime_var,df){
  
  # get all time periods
  time_periods <- unique(df[, sTime_var])
  
  # function - get unique per period (for in apply)
  return_unique_id <- function(period, df){ return(unique(df[df[,sTime_var] == period, sIndex_var]))}
  
  # all unique indexes per period
  obs_periods <- lapply(time_periods, return_unique_id, df=df)
  
  # all unique indexes that appear in all period
  obs_balanced <- Reduce(intersect, obs_periods)
  
  # df with balanced panels
  dfBalanced <- df[df[,sIndex_var] %in% obs_balanced,]
  
  return(dfBalanced)
  
}

# function to implement verbeek nijman test
VerbeekNijman <- function(Unbalanced_model){
  
  # parameters for plm
  param <- Unbalanced_model$args
  
  # get 
  index_model <- colnames(index(Unbalanced_model))
  index_var <- eval(Unbalanced_model$call$index)
  time_var <- index_model[index_model!= index_var]
  
  df <- cbind(index(Unbalanced_model), Unbalanced_model$model)
  
  dfBalanced <- get_balanced_df(index_var, time_var, df)

  BalancedModel <- plm(Unbalanced_model$formula, 
                       data=dfBalanced, 
                       index = c(index_var), 
                       model = param$model, 
                       effect = param$effect,
                       random.method = param$random.method,
                       random.models = param$random.models,
                       randomdfcor = param$random.dfcor,
                       inst.method = param$inst.method)
  

  phtest(BalancedModel, Unbalanced_model)
  
}

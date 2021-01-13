
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
  print(typeof(obs_balanced))
  print(length(obs_balanced))
  print(obs_balanced)
  
  # df with balanced panels
  dfBalanced <- df[df[,sIndex_var] %in% obs_balanced,]
  
  return(dfBalanced)
  
}


ge_obs_forPanels <- function(sIndex_var, sTime_var, df, minPanel){
  
  
  
  df_tally_id <- df%>% group_by_(sIndex_var) %>% tally()
  
  
  df_obs_panel <- df_tally_id %>% filter(n>=minPanel)
  
  vIDS <- as.character(unlist(df_obs_panel[,sIndex_var]))
  print(typeof(vIDS))
  print(length(vIDS))
  print(vIDS)

  dfPanels <- df[df[,sIndex_var] %in% vIDS,]
  
  return(dfPanels)
}


# function to implement verbeek nijman test
VerbeekNijman <- function(Unbalanced_model, minObs=NA){
  
  # parameters for plm
  param <- Unbalanced_model$args
  
  # get the variables from the unbalanced model, put together in dataframe
  index_model <- colnames(index(Unbalanced_model))
  index_var <- eval(Unbalanced_model$call$index)
  time_var <- index_model[index_model!= index_var]
  df <- cbind(index(Unbalanced_model), Unbalanced_model$model)
  
  if(is.na(minObs)){
    dfBalanced <- get_balanced_df(index_var, time_var, df)
  }else if(is.numeric(minObs)){
    
    dfBalanced <- ge_obs_forPanels(index_var, time_var, df,minObs )
    
  }
  
  print(dfBalanced)

  # create balanced model with same parameters
  BalancedModel <- plm(Unbalanced_model$formula, 
                       data=dfBalanced, 
                       index = c(index_var), 
                       model = param$model, 
                       effect = param$effect,
                       random.method = param$random.method,
                       random.models = param$random.models,
                       randomdfcor = param$random.dfcor,
                       inst.method = param$inst.method)
  
  # perform hausman test on balanced and unbalanced model
  VerbeekNijmanTest <- phtest(BalancedModel, Unbalanced_model)
  
  return(VerbeekNijmanTest)
  
}

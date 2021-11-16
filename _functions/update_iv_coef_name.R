update_iv_coef_name <- function(reg){
  # IV includes (fit) in dependent variable name; remove so name is 
  # consistent with OLS results.
  
  # https://stackoverflow.com/questions/29463182/how-to-output-several-variables-in-the-same-row-using-stargazer-in-r
  coef_names <- reg$coefficients %>% row.names()
  
  for(coef_name_old_i in coef_names){
    coef_name_new_i <- coef_name_old_i %>% 
      str_replace_all("\\(fit\\)", "") %>% 
      str_replace_all("`", "") 
    
    rownames(reg$coefficients)[rownames(reg$coefficients)==coef_name_old_i]<-coef_name_new_i
    rownames(reg$beta)[rownames(reg$beta)==coef_name_old_i]<-coef_name_new_i
  }
  
  rownames(reg$coefficients)[rownames(reg$coefficients)=="MA_var_exc_1996"]<-"MA_var_1996"
  rownames(reg$beta)[rownames(reg$beta)=="MA_var_exc_1996"]<-"MA_var_1996"
  
  return(reg)
}
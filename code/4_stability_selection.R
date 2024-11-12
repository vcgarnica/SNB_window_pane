########################################
######### stability selection ########## 
########################################

####### Author: Vinicius Garnica
####### Date: Oct, 2024


### Load packages -----------------------------------------------------------------------------------------------
library(data.table) # using library() instead of packman::p_loa because of HPC
library(furrr)
library(tidyverse)
library(future)
library(sharp)

### Load data sets
rm(list = ls())
load("windows.RData")

dt=windows

### Set seed
set.seed(123)

# Set up parallel backend using furrr
plan(multisession)


### Variable selection ------------------------------------------------------------------------------------------- 

selection_lasso = function(data,response){
  y_reg = data %>% select({{response}}) 
  x_reg = data %>% select(-c(SITE, DATE, DOY, matches("fa")))
  
  stab_reg = VariableSelection(
    xdata = x_reg,
    ydata = y_reg,
    K = 1000,
    seed = 123,
    resampling = "bootstrap",
    verbose = FALSE)
  
  if(any(is.na(Stable(stab_reg)))){return(NA)}
  else {
    covars= names(Stable(stab_reg))[Stable(stab_reg) != 0]
    dataset= bind_cols(y_reg,x_reg[covars])
    return(dataset)
  }
}

### Stability selection -------------------------------------------------------------------------------------------

compute_stable = function(data) {
  data %>%
    na.omit() %>%
    nest(data = -c(VAR, LAG)) %>%
    filter(map(data, nrow) >= 10) %>%   # at least 10 environments for correlation 
    mutate(
      fa1 = map(data, ~ selection_lasso(.x, fa1)),
      fa2 = map(data, ~ selection_lasso(.x, fa2)),
      fa3 = map(data, ~ selection_lasso(.x, fa3))
    ) %>%
    select(-data)
}

stable_5 = compute_stable(dt[[1]])
stable_10 = compute_stable(dt[[2]])
stable_15 = compute_stable(dt[[3]])
stable_20 = compute_stable(dt[[4]])
stable_25 = compute_stable(dt[[5]])
stable_30 = compute_stable(dt[[6]])

plan(sequential) 

### Save -------------------------------------------------------------------------------------------
stable_results = list(stable_5, stable_10, stable_15, stable_20, stable_25, stable_30)
lags = c(5, 10, 15, 20, 25, 30)

for (i in seq_along(stable_results)) {
  result = stable_results[[i]]
  lag = lags[i]
  save(result, file = paste0("data/stable/stable_", lag, ".RData"))
}


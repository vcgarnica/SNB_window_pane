###########################################
######### Bootstrap correlations ########## 
###########################################

####### Author: Vinicius Garnica
####### Date: Oct, 2024


### Load packages
library(data.table)
library(furrr)
library(tidymodels)
library(tidyverse)
library(future)

### Set seed
set.seed(123)

# Set up parallel backend using furrr
plan(multisession)

### Load data sets
rm(list = ls())
load("data/stable/stable_15.RData")

### Correlations -------------------------------------------------------------------------------------------

cor_spearman =function(splits, var1 = "",var2= "") {
  rstatix::cor_test(!!var1,!!var2, method = "spearman", data = analysis(splits))
}

parallel_correlations = function(data,unnest_factor,var1, var2,lag) {
  res_cor = data %>%
    unnest({{unnest_factor}}) %>%
    filter(LAG == {{lag}}) %>%
    mutate(
      cor_boot = future_map(
        splits,
        ~ cor_spearman(.x,var1,var2)
      )
    ) %>%
    unnest(cor_boot)
  
  result = res_cor %>%
    group_by(LAG,VAR,var1,var2) %>%
    summarise(
      mean = mean(cor, na.rm = TRUE),
      median = median(cor, na.rm = TRUE),
      lower_95 = quantile(cor, 0.025, na.rm = TRUE),
      upper_95 = quantile(cor, 0.975, na.rm = TRUE),.groups = 'drop') %>%
    mutate(across(where(is.numeric),~round(.,2)))
  
  return(result)
}


### Bootstrap -------------------------------------------------------------------------------------------

### Wrangle data to improve code efficiency
fa_data = result %>%
  pivot_longer(cols = c(fa1,fa2,fa3), names_to = "FA", values_to = "data")

### Define number of bootstraps
num_bootstraps = 1000

### Create bootstrap samples and extract variables
boot_data = fa_data %>% 
  mutate(
    boot = map(data, ~ bootstraps(.x, times = num_bootstraps,apparent = FALSE)),
    vars= map(data, ~ colnames(.x))) %>%
  setDT()

### Initialize an empty list to store the results
combined_outputs = list()

### Loop through each LAG value
for (lag_value in unique(boot_data$LAG)) {
  for (fa_value in unique(boot_data$FA)) {
    for (var_value in unique(boot_data$VAR)) {
      subset_data = boot_data %>%
        filter(VAR == var_value & FA == fa_value & LAG == lag_value)
      
      dep_vars = subset_data %>%
        select(vars) %>%
        unnest(cols = c(vars)) %>%
        filter(grepl("fa", vars)) %>%
        pull()
      
      ind_vars = subset_data %>%
        select(vars) %>%
        unnest(cols = c(vars)) %>%
        filter(!grepl("fa", vars)) %>%
        pull()
      
      combined_output = ind_vars %>%
        future_map_dfr(~ {
          output = tibble(parallel_correlations(subset_data, boot, dep_vars, .x, lag_value))})
      
      # Store the result in the combined_outputs list
      combined_outputs[[paste0("LAG_", lag_value, "_FA_", fa_value, "_VAR_", var_value)]] = combined_output
    }
  }
}

result_15 = bind_rows(combined_outputs)


### Save -------------------------------------------------------------------------------------------
save(result_15, file = "results/result_15.RData")


plan(sequential) 
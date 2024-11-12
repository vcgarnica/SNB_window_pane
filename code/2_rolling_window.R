################################
######## Rolling Window  #######
################################

####### Authors: Vinicius Garnica
####### Date: Oct, 2024

### Load packages -----------------------------------------------------------------------------------------------
pacman::p_load(data.table,
               tidyverse,
               slider,
               doParallel,
               dplyr,
               purrr)

### Load data sets
rm(list = ls())
load("data/engineered_variables.RData")

# Register the parallel backend -----------------------------------------------------------------------------------------------
cl = makeCluster(detectCores())
registerDoParallel(cl)


### Rolling average and sum functions -----------------------------------------------------------------------------------------------
# Function to compute rolling mean for specified columns and window size
slide_mean = function(data, wind){ 
  data %>%
    mutate(across(matches("DPD|VPS|VPA|VPD|RH.A|RH.min|RH.max|T.A.|T.min|T.max"), # variables that will be used
                  ~ slide_dbl(.x, ~mean(.x),
                              .before = wind - 1,                                 # -1 will allow us to calculate moving averages across all days including the specified one
                              .complete = TRUE),
                  .names = "{.col}.mean_")) %>%                                   # renaming columns
    select(DATE, DOY,matches("mean_")) %>%                                 # keep only variables of interest
    rename_with(~ paste0(., wind, sep = ""), -c("DATE", "DOY"))
}

# Function to compute rolling sum for specified columns and window size
slide_sum = function(data, wind){
  data %>%
    mutate(across(-matches("SITE|DATE|DOY|nhours|DPD|VPS|VPA|VPD|RH.A|RH.min|RH.max|T.A.|T.min|T.max"),
                  ~ slide_dbl(.x, ~sum(.x), 
                              .before = wind - 1, 
                              .complete = TRUE),  
                  .names = "{.col}.sum_")) %>%
    select(DATE, DOY, matches("sum_")) %>%
    rename_with(~ paste0(., wind, sep = ""), -c("DATE","DOY"))
}

### Preparing data sets--------------------------------------------------------------------------------

# Function to compute rolling mean and sum for different window sizes
wind_function = function(DF, wind) {
  res_mean = DF %>%
    ungroup() %>%
    group_by(SITE) %>%
    nest() %>%
    summarise(BLOCK = map(data, ~slide_mean(.x, wind))) %>%
    unnest(data = ., cols = BLOCK)
  
  res_sum = DF %>%
    ungroup() %>%
    group_by(SITE) %>%
    nest() %>%
    summarise(BLOCK = map(data, ~slide_sum(.x, wind))) %>%
    unnest(data = ., cols = BLOCK)
  
  res = left_join(res_mean, res_sum, by = c("SITE", "DATE", "DOY"))
  return(res)
}


### Length of windows from 5 to 30. Users should define this windows based on expert knowledge about the system under investigation.
wind_size = seq(5, 30, 5)

### Loops for each variable and window size --------------------------------------------------------------------------------
output = vector('list')

for (i in names(engineered_variables)) {
  output[[i]] = map(wind_size, ~ wind_function(engineered_variables[[i]], .x))
}

### Stop the parallel backend
stopCluster(cl)

### Since the output is a list, we clean and omit NAs. Why are there NAs?
### Note that the first 4 values resulting from the computation of the rolling average with a 5 day window will be NAs. That makes sense.
### Similarly, the first 9 values of the rolling average (or sum) for a 10 day window will be NAs. 

remove_nas = function(df) {
  df[complete.cases(df), ]
}

rolling_window = map(output , ~ map(.x, remove_nas)) %>% 
  purrr::flatten() 

### Quality check --------------------------------------------------------------------------------

rolling_window[[30]] %>% # change data sets by changing the number from 1 to 30. There should be one only observation per day
  group_by(DATE, SITE) %>% 
  summarise(n=n()) %>%
  ggplot(aes(x = day(DATE), y = month(DATE), fill = n)) +
  geom_tile() +
  labs(x = NULL) +
  facet_wrap(~SITE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

### Save --------------------------------------------------------------------------------
save(rolling_window, file = "data/rolling_window.RData")

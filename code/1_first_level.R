####################################
####### First-level variable ####### 
####################################

####### Authors: Vinicius Garnica
####### This part was built with directions from Dr. Felipe Dalla Lana
####### Date: Oct, 2024

### Load packages -----------------------------------------------------------------------------------------------
pacman::p_load(data.table,
               lubridate,
               tidyverse)

### Load data sets
rm(list = ls())
load("data/weather_data.RData")

### Transform
data_weather$YEAR = as.factor(data_weather$YEAR)
data_weather$SITE = as.factor(data_weather$SITE)

### Glimpse
glimpse(data_weather)

### Indicator variables -------------------------------------------------------------------------------------------

# Robust peak detection algorithm (using z-scores) https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data
ThresholdingAlgo = function(y,lag,threshold,influence) {
  signals = rep(0,length(y))
  filteredY = y[0:lag]
  avgFilter = NULL
  stdFilter = NULL
  avgFilter[lag] = mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] = sd(y[0:lag], na.rm=TRUE)
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] = 1;
      } else {
        signals[i] = -1;
      }
      filteredY[i] = influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] = 0
      filteredY[i] = y[i]
    }
    avgFilter[i] = mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] = sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}

### Feature engineering of hourly data -------------------------------------------------------------------------------------------

data_hour = 
  data_weather %>%
  group_by(SITE) %>%
  mutate(
    DPD = TMP - ((HMD / 100)^(1/8) * (112 + (0.9 * TMP)) - 112 + (0.1 * TMP)), # Dew point depression
    VPS = 0.611 * 10^((7.5 * TMP) / (237.3 + TMP)), # Saturated vapor pressure
    VPA = VPS * (HMD / 100), # Actual vapor pressure
    VPD = VPS - VPA, # Vapor pressure deficit
    
    # Relative humidity
    RH.L35 = ifelse(HMD <= 35, 1, 0), 
    RH.G90 = ifelse(HMD >= 90, 1, 0), 
    
    # Temperature
    T.L0 = ifelse(TMP < 0, 1, 0),
    T.3T7 = ifelse(TMP >= 3 & TMP <= 7, 1, 0),
    T.7T10 = ifelse(TMP >= 7 & TMP <= 10, 1, 0),
    T.10T13 = ifelse(TMP >= 10 & TMP <= 13, 1, 0), 
    T.13T16 = ifelse(TMP >= 13 & TMP <= 16, 1, 0), 
    T.16T19 = ifelse(TMP >= 16 & TMP <= 19, 1, 0), 
    T.19T22 = ifelse(TMP >= 19 & TMP <= 22, 1, 0), 
    T.22T25 = ifelse(TMP >= 22 & TMP <= 25, 1, 0),
    T.25T28 = ifelse(TMP >= 25 & TMP <= 28, 1, 0),
    T.G28 = ifelse(TMP >= 28, 1, 0),
    
    # Rainfall
    R.EPISODE = ifelse(RNF > 0, 1, 0), 
    
    # Combination of variables
    TRH.16T19nRH.L40 = ifelse(TMP >= 16 & TMP <= 19 & HMD <= 40, 1, 0), 
    TRH.19T22nRH.L40 = ifelse(TMP >= 19 & TMP <= 22 & HMD <= 40, 1, 0), 
    TRH.22T25nRH.L40 = ifelse(TMP >= 22 & TMP <= 25 & HMD <= 40, 1, 0), 
    TRH.25T28nRH.L40 = ifelse(TMP >= 25 & TMP <= 28 & HMD <= 40, 1, 0),
    TRH.G28nRH.L40 = ifelse(TMP >= 28 & HMD <= 40, 1, 0),
    
    TRH.3T7nRH.G80 = ifelse(TMP >= 3 & TMP <= 7 & HMD >= 80, 1, 0),
    TRH.7T10nRH.G80 = ifelse(TMP >= 7 & TMP <= 10 & HMD >= 80, 1, 0),     
    TRH.10T13nRH.G80 = ifelse(TMP >= 10 & TMP <= 13 & HMD >= 80, 1, 0),   
    TRH.13T16nRH.G80 = ifelse(TMP >= 13 & TMP <= 16 & HMD >= 80, 1, 0), 
    TRH.16T19nRH.G80 = ifelse(TMP >= 16 & TMP <= 19 & HMD >= 80, 1, 0), 
    TRH.19T22nRH.G80 = ifelse(TMP >= 19 & TMP <= 22 & HMD >= 80, 1, 0), 
    
    TR.3T7nR.G0.2 = ifelse(TMP >= 3 & TMP <= 7 & RNF > 0.2, 1, 0), 
    TR.7T10nR.G0.2 = ifelse(TMP >= 7 & TMP <= 10 & RNF > 0.2, 1, 0), 
    TR.10T13nR.G0.2 = ifelse(TMP >= 10 & TMP <= 13 & RNF > 0.2, 1, 0), 
    TR.13T16nR.G0.2 = ifelse(TMP >= 13 & TMP <= 16 & RNF > 0.2, 1, 0), 
    TR.16T19nR.G0.2 = ifelse(TMP >= 16 & TMP <= 19 & RNF > 0.2, 1, 0),
    TR.19T22nR.G0.2 = ifelse(TMP >= 19 & TMP <= 22 & RNF > 0.2, 1, 0),
    
    RH6.peak4 = ThresholdingAlgo(HMD, 6, 4, 1)$signals,
    T6.peak4 = ThresholdingAlgo(TMP, 6, 4, 1)$signals
  ) %>%
  left_join(., data_weather %>% select(SITE, DATE, YEAR, TIME, PERIOD1, PERIOD2, PERIOD3, DOY))

  
### Supporting functions for variables -------------------------------------------------------------------------------------------

### Define a function to calculate the maximum consecutive hours above a certain level
MAX_rl = function(var, level) {
  x = ifelse(var >= level, 1, 0)
  z = rle(x) 
  ifelse(length(z$lengths) == 1,
         ifelse(z$values > 0, z$lengths, 0),
         max((z$lengths[z$values > 0])))
}

### Define a function to calculate the maximum consecutive hours below a certain level
MIN_rl = function(var, level) {
  x = ifelse(var <= level, 1, 0)
  z = rle(x) 
  ifelse(length(z$lengths) == 1,
         ifelse(z$values > 0, z$lengths, 0),
         max((z$lengths[z$values > 0])))
}

### Define a function to count events where a variable is above or equal to a level for n hours
COUNT_rl_above = function(var, level, n) {
  x = ifelse(var >= level, 1, 0)
  z = rle(x)
  sum(z$values[z$lengths >= n])
}

### Define a function to count events where a variable is below or equal to a level for n hours
COUNT_rl_below = function(var, level, n) {
  x = ifelse(var <= level, 1, 0)
  z = rle(x)
  sum(z$values[z$lengths >= n])
}

### Define a function to count the number of windows with more than n hours above a certain level
WIN_rl = function(var, level, n) {
  x = ifelse(var >= level, 1, 0)
  z = rle(x) 
  e = z$lengths[z$values == 1]
  sum(ifelse(e >= n, e - (n - 1), 0))
}

### Define a function to calculate hourly Growing Degree Days (Celsius)
gdd = function(Tmax, Tmin, Tb) {
  res = ((Tmax + Tmin) / 2) - Tb
  return(res)
}

### Creating a function to summarize variables in daytime and nighttime -------------------------------------------------------------------------------------
### Daytime function. Here we we don't need to use PERIOD from data_weather because both 24h, daytime, dusk and dusk are subsets of the same calendar day.
### However, nighttime period collect data from two calendar days. Think about it.

FUNCTION_day = function(dataset, period = "24h") {
  dd = case_when(period == "24h" ~ "24h",
                 period == "daytime" ~ "daytime",
                 period == "dawn" ~ "dawn",
                 period == "dusk" ~ "dusk")
  
  dataset %>% group_by(SITE, DATE) %>%
    summarise(
      nhours._   = n(),
      DPD._      = mean(DPD),
      VPD._      = mean(VPD),
      
      RH.A._     = mean(HMD),
      RH.max._   = max(HMD),
      RH.min._   = min(HMD), 
      RH.L35._   = sum(RH.L35),
      RH.G90._   = sum(RH.G90),
      RH.90.rl.count8._ = COUNT_rl_above(var = HMD, level = 90, n = 8),
      RH.40.rl.count8._ = COUNT_rl_below(var = HMD, level = 40, n = 8),
      RH.90.rl.count6._ = COUNT_rl_above(var = HMD, level = 90, n = 6),
      RH.40.rl.count6._ = COUNT_rl_below(var = HMD, level = 40, n = 6),
      
      T.A._      = mean(TMP),      
      T.max._    = max(TMP),
      T.min._    = min(TMP),
      T.L0._     = sum(T.L0), 
      T.3T7._    = sum(T.3T7),  
      T.7T10._   = sum(T.7T10),      
      T.10T13._  = sum(T.10T13),
      T.13T16._  = sum(T.13T16),  
      T.16T19._  = sum(T.16T19),
      T.19T22._  = sum(T.19T22),
      T.22T25._  = sum(T.22T25),
      T.25T28._  = sum(T.25T28),
      T.G28._    = sum(T.G28),
      
      TRH.16T19nRH.L40._  = sum(TRH.16T19nRH.L40),
      TRH.19T22nRH.L40._  = sum(TRH.19T22nRH.L40),
      TRH.22T25nRH.L40._  = sum(TRH.22T25nRH.L40),
      TRH.25T28nRH.L40._  = sum(TRH.25T28nRH.L40),
      TRH.G28nRH.L40._    = sum(TRH.G28nRH.L40),
      
      TRH.3T7nRH.G80._  = sum(TRH.3T7nRH.G80),
      TRH.7T10nRH.G80._  = sum(TRH.7T10nRH.G80),
      TRH.10T13nRH.G80._  = sum(TRH.10T13nRH.G80),
      TRH.13T16nRH.G80._  = sum(TRH.13T16nRH.G80),
      TRH.16T19nRH.G80._  = sum(TRH.16T19nRH.G80),
      TRH.19T22nRH.G80._  = sum(TRH.19T22nRH.G80),
      
      TR.3T7nR.G0.2._  = sum(TR.3T7nR.G0.2),
      TR.7T10nR.G0.2._  = sum(TR.7T10nR.G0.2),
      TR.10T13nR.G0.2._  = sum(TR.10T13nR.G0.2),
      TR.13T16nR.G0.2._  = sum(TR.13T16nR.G0.2), 
      TR.16T19nR.G0.2._  = sum(TR.16T19nR.G0.2),
      TR.19T22nR.G0.2._  = sum(TR.19T22nR.G0.2),
      
      R.S._      = sum(RNF),
      R.AH._     = sum(R.EPISODE),
      R.0.5.rl.count5._  = COUNT_rl_above(var = RNF, level = 0.5, n = 5),
      R.1.rl.max._  = MAX_rl(var = RNF, level = 1),
      R.2.rl.max._  = MAX_rl(var = RNF, level = 2),
      
      RH6.peak4._ = sum(rle(RH6.peak4)$values != 0),
      T6.peak4._  = sum(rle(T6.peak4)$values != 0)
    ) %>%
    mutate(
      GDD._  = gdd(T.max._, T.min._, 0),
      T.AMP._ = (T.max._ - T.min._),
      RH.AMP._ = (RH.max._ - RH.min._)
    ) %>%
    setNames(gsub("_", dd, names(.))) # Change variable names to reflect the time of interest
}



### Define a function to summarize and create new variables for nighttime 

FUNCTION_nighttime = function(dataset, period = "nighttime"){  # The "period" argument is use to change the variables names, so we can combine then we can combine 24h and overnighttime data with no conflict   
  dd = case_when(period == "nighttime"~ "nighttime")
  
  dataset %>% group_by(SITE, PERIOD3) %>% # group by includes period3 instead of day because we want to keep nighttime hours consecutive. If group by DATE, we end up with nonconsecutive nighttime hours.
      summarise(
        nhours._   = n(),
        DPD._      = mean(DPD),
        VPD._      = mean(VPD),
        
        RH.A._     = mean(HMD),
        RH.max._   = max(HMD),
        RH.min._   = min(HMD), 
        RH.L35._   = sum(RH.L35),
        RH.G90._   = sum(RH.G90),
        RH.90.rl.count8._ = COUNT_rl_above(var = HMD, level = 90, n = 8),
        RH.40.rl.count8._ = COUNT_rl_below(var = HMD, level = 40, n = 8),
        RH.90.rl.count6._ = COUNT_rl_above(var = HMD, level = 90, n = 6),
        RH.40.rl.count6._ = COUNT_rl_below(var = HMD, level = 40, n = 6),
        
        T.A._      = mean(TMP),      
        T.max._    = max(TMP),
        T.min._    = min(TMP),
        T.L0._     = sum(T.L0), 
        T.3T7._    = sum(T.3T7),  
        T.7T10._   = sum(T.7T10),      
        T.10T13._  = sum(T.10T13),
        T.13T16._  = sum(T.13T16),  
        T.16T19._  = sum(T.16T19),
        T.19T22._  = sum(T.19T22),
        T.22T25._  = sum(T.22T25),
        T.25T28._  = sum(T.25T28),
        T.G28._    = sum(T.G28),
        
        TRH.16T19nRH.L40._  = sum(TRH.16T19nRH.L40),
        TRH.19T22nRH.L40._  = sum(TRH.19T22nRH.L40),
        TRH.22T25nRH.L40._  = sum(TRH.22T25nRH.L40),
        TRH.25T28nRH.L40._  = sum(TRH.25T28nRH.L40),
        TRH.G28nRH.L40._    = sum(TRH.G28nRH.L40),
        
        TRH.3T7nRH.G80._  = sum(TRH.3T7nRH.G80),
        TRH.7T10nRH.G80._  = sum(TRH.7T10nRH.G80),
        TRH.10T13nRH.G80._  = sum(TRH.10T13nRH.G80),
        TRH.13T16nRH.G80._  = sum(TRH.13T16nRH.G80),
        TRH.16T19nRH.G80._  = sum(TRH.16T19nRH.G80),
        TRH.19T22nRH.G80._  = sum(TRH.19T22nRH.G80),
        
        TR.3T7nR.G0.2._  = sum(TR.3T7nR.G0.2),
        TR.7T10nR.G0.2._  = sum(TR.7T10nR.G0.2),
        TR.10T13nR.G0.2._  = sum(TR.10T13nR.G0.2),
        TR.13T16nR.G0.2._  = sum(TR.13T16nR.G0.2), 
        TR.16T19nR.G0.2._  = sum(TR.16T19nR.G0.2),
        TR.19T22nR.G0.2._  = sum(TR.19T22nR.G0.2),
        
        R.S._      = sum(RNF),
        R.AH._     = sum(R.EPISODE),
        R.0.5.rl.count5._  = COUNT_rl_above(var = RNF, level = 0.5, n = 5),
        R.1.rl.max._  = MAX_rl(var = RNF, level = 1),
        R.2.rl.max._  = MAX_rl(var = RNF, level = 2),
        
        RH6.peak4._ = sum(rle(RH6.peak4)$values != 0),
        T6.peak4._  = sum(rle(T6.peak4)$values != 0)
      ) %>%
    mutate(
      GDD._  = gdd(T.max._, T.min._, 0),
      T.AMP._ = (T.max._ - T.min._),
      RH.AMP._ = (RH.max._ - RH.min._)
    ) %>%
    setNames(gsub("_", dd, names(.))) # Change variable names to reflect the time of interest
}



### Applying formula to different intra-day periods -------------------------------------------------------------------------------------------

data_24h  = FUNCTION_day(data_hour, period = "24h") %>% mutate(DOY =yday(DATE)) %>% relocate(DOY,.after=DATE)# applying function above to entire day or 24h
data_day  = data_hour %>% filter(PERIOD1=="daytime") %>% FUNCTION_day(., period = "daytime") %>% mutate(DOY =yday(DATE)) %>% relocate(DOY,.after=DATE)# filtering and applying functions above to hours with sunlight
data_dawn  = data_hour %>% filter(PERIOD2=="dawn") %>% FUNCTION_day(., period = "dawn")%>% mutate(DOY =yday(DATE)) %>% relocate(DOY,.after=DATE) # filtering and applying functions around dawn (3 before and 3 after)
data_dusk  = data_hour %>% filter(PERIOD2=="dusk") %>% FUNCTION_day(., period = "dusk")%>% mutate(DOY =yday(DATE)) %>% relocate(DOY,.after=DATE) # filtering and applying functions around dusk (3 before and 3 after)
data_nighttime = data_hour %>% filter(PERIOD1=="nighttime") %>% FUNCTION_nighttime(., period = "nighttime") # filtering and applying functions above to hours without sunlight

### Merging date to the nighttime set. Period 3 will drop

data_nighttime= left_join(data_nighttime %>% filter(!nhours.nighttime<10),unique(data_hour %>% 
                                 filter(PERIOD1=="nighttime") %>% 
                                 select(PERIOD3,DATE,SITE)) %>%
               distinct(PERIOD3,.keep_all = TRUE), by=c("SITE","PERIOD3")) %>% # period consists of consecutive nighttime hours
  relocate(DATE,.before = PERIOD3) %>%
  select(-PERIOD3) %>%  # relocating merged column and removing period3
  distinct(SITE,DATE,.keep_all = TRUE) %>% 
  mutate(DOY =yday(DATE)) %>% 
  relocate(DOY,.after=DATE)


### Save -------------------------------------------------------------------------------------------
### create a list of data frames
engineered_variables = list("d24h"= data_24h,
                         "daytime"= data_day,
                         "nighttime" = data_nighttime,
                         "dawn" = data_dawn,
                         "dusk" = data_dusk) 


save(engineered_variables, file = "data/engineered_variables.RData")



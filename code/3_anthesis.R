#########################################################
######## Modeling wheat anthesis and setting LAG ########
#########################################################

####### Authors: Vinicius Garnica
####### Date: Oct, 2024

### Load packages -----------------------------------------------------------------------------------------------
pacman::p_load(data.table,
               tidyverse,
               lubridate,
               meteor,
               openmeteo,
               dplyr,
               purrr)


### Load data sets
rm(list = ls())
load("data/fixed_window.RData") 
load("data/loadings.RData") #the outcome file is the object fa.loadings 

### Formulas ------------------------------------------------------------------------------

veff = function(Temp) {
  result = numeric(length(Temp))
  
  condition1 = Temp < -4 | Temp > 17
  condition2 = Temp >= -4 & Temp < 3
  condition3 = Temp >= 3 & Temp < 10
  condition4 = Temp >= 10 & Temp <= 17
  
  result[condition1] = 0
  result[condition2] = (Temp[condition2] - (-4)) / (3 - (-4))
  result[condition3] = 1
  result[condition4] = (17 - Temp[condition4]) / (17 - 10)
  
  return(result)
}

fv = function(vdd, Vbase, Vsat) {
  result = numeric(length(vdd))
  
  condition1 = vdd < Vbase
  condition2 = vdd >= Vbase & vdd <= Vsat
  condition3 = vdd > Vsat
  
  result[condition1] = 0
  result[condition2] = (vdd[condition2] - Vbase) / (Vsat - Vbase)
  result[condition3] = 1
  
  return(result)
}

tt = function(Temp, Tbase, sigma) {
  result = numeric(length(Temp))
  
  condition1 = Temp <= Tbase | Temp > 37
  condition2 = Temp > Tbase & Temp <= 26
  condition3 = Temp > 26 & Temp <= 37
  
  result[condition1] = 0
  result[condition2] = 26 * (exp(-((Temp[condition2] - 26) / (2 * sigma))^2))
  result[condition3] = 26 * (1 - ((Temp[condition3] - 26) / (37 - 26))^2)
  
  return(result)
}


fp = function(ph,pbase,popt){
  result = numeric(length(ph))
  
  condition1 = ph < pbase
  condition2 = ph >= pbase & ph <= popt
  condition3 = ph > popt
  
  result[condition1] = 0
  result[condition2] = (ph[condition2] - pbase) / (popt - pbase)
  result[condition3] = 1
  
  return(result)
}

ts = function(Temp, Tbase) {
  return(sin((pi/2) * (Temp - Tbase) / (26 - Tbase)))
}


### Data wrangling ------------------------------------------------------------------------------
outcome_list = lapply(c("sev"), function(element) {
  df = as.data.frame(loadings[[element]])
  df = tibble::rownames_to_column(df, "SITE")
  df$VAR = element
  return(df)
})

print(outcome_list)

fa_loadings=do.call(rbind, outcome_list) %>% 
  mutate_at(vars(SITE,VAR),factor)

### Download data ------------------------------------------------------------------------------
dt=read.csv("data/locations.csv")
dt$start_date = as.Date(paste0(dt$year-1,"-09-15"))
dt$end_date = as.Date(paste0(dt$year,"-06-01"))


### Download function ----------------------------------------------------------------------------
download_weather_data = function(lat, lon, start_date, end_date, retries = 3) {
  for (attempt in 1:retries) {
    
    # Adjust lat/lon on the second attempt
    if (attempt == 2) {  # This line is now adjusting coordinates on the second attempt
      lat = lat + 0.001
      lon = lon + 0.001
    }
    
    result = tryCatch(
      {
        weather_history(
          c(lat, lon),
          start = start_date,
          end = end_date,
          hourly = list("temperature_2m", "relative_humidity_2m", "precipitation", "et0_fao_evapotranspiration"),
          timezone = "auto"
        )
      },
      error = function(e) {
        NULL  
      }
    )
    
    if (!is.null(result)) return(result)  # Stop retrying if successful
    
    Sys.sleep(2)  # Wait 2 seconds before retrying
  }
  return(NULL)  # Return NULL if all attempts fail
}

### Download weather dt for each unique site -----------------------------------------------------
failed_sites = c()  
weather_list = list()

for (i in seq_len(nrow(dt))) {
  site = as.character(dt$site[i])
  
  lat = dt$lat[i]
  lon = dt$lon[i]
  start_date = dt$start_date[i]
  end_date = dt$end_date[i]
  
  # Download weather dt
  weather = download_weather_data(lat, lon, start_date, end_date, retries = 3)
  
  if (!is.null(weather)) {
    weather$site = site
    weather_list[[site]] = weather
  } else {
    failed_sites[[site]] = site
  }
}

### Combine all weather dt into one dataframe -------------------------------------------------
combined_weather = bind_rows(weather_list)

length(unique(combined_weather$site))


daily_weather = combined_weather %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(site,date) %>%
  reframe(T_air=mean(hourly_temperature_2m)) %>%
  drop_na()

### Remove extra data
rm(weather_list)

### Observed anthesis date ------------------------------------------------------------------------------

anthesis_dates = data.frame(
  site = c("RW22", "CL22", "MR22", "KS22", "KS23", "KS24", "PY22", "PY23", "SB22", "SB23", "SB24", "LB23", "UN23", "OX23", "AL24", "BE24", "RO24", "LB24"),
  observed_DATE = as.Date(c("2022-04-13", "2022-04-24", "2022-04-22", "2022-04-17", "2023-04-10", "2024-04-13", "2022-04-20", "2023-04-22", "2022-04-23", "2023-04-14", "2024-04-15", "2023-04-18", "2023-04-20", "2023-04-22", "2024-04-15", "2024-04-13", "2024-04-27", "2024-04-18"))
)

### Predicted anthesis date ------------------------------------------------------------------------------

# Merge regions in the state to account for average planting date

daily_weather = daily_weather %>% 
  mutate(region= case_when(site=="AL24" ~'Piedmont',
                           str_detect(site, 'KS') ~'Southeastern Plains',
                           site=="BE24" ~'Middle Atlantic Coastal Plain',
                           site=="RO24" ~'Piedmont',
                           site=="OX23" ~'Piedmont',
                           site=="CL22" ~'Piedmont',
                           str_detect(site, 'SB') ~ 'Piedmont',
                           site=="RW22" ~'Southeastern Plains',
                           str_detect(site, 'UN') ~ 'Piedmont',
                           str_detect(site, 'MR') ~ 'Piedmont',
                           str_detect(site, 'LB') ~ 'Southeastern Plains',
                           str_detect(site, 'PY') ~ 'Middle Atlantic Coastal Plain'))


anthesis = daily_weather %>%
  left_join(.,dt %>% select(site,lat)) %>%
  mutate(
    photo = photoperiod(date,lat),
    filtered = case_when(
      region == "Piedmont" & !(month(date) %in% c(9)) & (!(month(date)==10 & day(date) < 10)) ~ TRUE,
      region == "Southeastern Plains" & !(month(date) %in% c(9)) & (!(month(date)==10 & day(date) < 20)) ~ TRUE,
      region == "Middle Atlantic Coastal Plain" & !(month(date) %in% c(9)) & (!(month(date)==10 & day(date) < 30)) ~ TRUE,
      TRUE ~ FALSE
      )) %>%
  filter(filtered == TRUE) %>%
  mutate(
    tt= tt(T_air,1.5,7.6),
    ts  = ts(T_air,1.5),
    veff = veff(T_air),
    fpa= fp(photo, 5,20)
  ) %>%
  group_by(site) %>%
  mutate(
    vdd= cumsum(veff),
    fv = fv(vdd,30,80),
    PVT = (tt*fv*fpa*ts),
    aPVT = 148+cumsum(PVT)) %>%
  filter(aPVT >= 500) %>%
  slice(1) %>%
  select(site, date) %>%
  left_join(anthesis_dates, by = "site") %>%
  rename(predicted_DATE = date) %>% 
  mutate(predicted_DOY = yday(predicted_DATE),
         observed_DOY = yday(observed_DATE),
         diff=predicted_DOY-observed_DOY) %>%
  rename(SITE = site);anthesis


fa_loadings = left_join(fa_loadings,
                        anthesis)

write.csv(fa_loadings,"results/loading.csv")

### Visualizing differences ------------------------------------------------------------------------------

fa_loadings %>%
  filter(VAR=="sev")%>%
  ggplot() +
  geom_point(aes(x = SITE, y = diff), group = 1, show.legend = TRUE) +
  labs(x = "Site", y = "Day of Year") +
  theme_minimal()


### Merging and calculating LAG ------------------------------------------------------------------------------

### Day of anthesis as anchor point
window_anthesis = 
  lapply(lapply(fixed_window, left_join, fa_loadings %>% select(SITE,fa1,fa2,fa3,VAR,predicted_DATE)), function(x) {
    x %>% 
      mutate(LAG= as.numeric(predicted_DATE-DATE)) %>% 
      select(-predicted_DATE)
  })


### Combine data sets with the same window length -------------------------------------------------------------------------------------
### This is the same as combining data sets with different intra-day periods.

pattern = c("SITE","DATE","DOY","VAR","fa1","fa2","fa3","LAG")

merge_datasets = function(data,lags,pattern) {
  result = data[[lags[1]]]
  for (i in lags[-1]) {
    result = left_join(result, data[[i]], by = pattern)
  }
  return(result)
}

window_sizes = list(c(1, 7, 13, 19, 25), c(2, 8, 14, 20, 26), c(3, 9, 15, 21, 27),
                     c(4, 10, 16, 22, 28), c(5, 11, 17, 23, 29), c(6, 12, 18, 24, 30))


# Combine window data for 'window_sev' and 'window_1000gdd'
rolling_windows =lapply(window_sizes, function(window_size) {merge_datasets(window_anthesis, window_size, pattern)})

### Save --------------------------------------------------------------------------------
save(rolling_windows,file = "data/rolling_windows.RData")

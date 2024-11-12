#############################
########## Intra-day ######## 
#############################

####### Author: Vinicius Garnica
####### Date: Oct, 2024

### Load packages -----------------------------------------------------------------------------------------------
pacman::p_load(lubridate,
               ggplot2,
               suncalc,
               data.table,
               dplyr)

### Load and wrangle data sets -----------------------------------------------------------------------------------
rm(list = ls())
load("data/weather_data_2022.RData")
load("data/weather_data_2023.RData")
load("data/weather_data_2024.RData")

### Create data set
data_weather = rbind(data_weather_22,data_weather_23,data_weather_24) %>%
  mutate(DOY = yday(DATE)) 

### Check missing values
table(day(data_weather$TIME),month(data_weather$TIME), data_weather$SITE)

### Clean data 
data_weather = data_weather %>%
  group_by(DOY,SITE) %>%
  filter(n()>= 20) %>%      #remove days with less than 20 hourly observations
  ungroup

### Creating intra-day periods -----------------------------------------------------------------------------------

### To get site-specific sunset and sunrise hours, we will be using the suncalc R package and site coordinates.

### Add sunrise and sunset hours based on coordinates based on each location
dt=data.frame(sites=c("PY22","MR22","SB22","RW22", "CL22", "KS22",
                      "PY23","UN23","SB23","LB23", "OX23", "KS23",
                      "BE24","AL24","SB24","LB24", "RO24", "KS24"),
              lat=c(35.849167,34.949337,35.696489,34.511219,35.671203,35.378138,
                    35.849213,35.089407,35.699578,34.713276,36.35593184,35.375757,
                    35.559115,34.859825,35.697913,34.754557,36.322885,35.377746),
              lon=c(-76.655278,-80.424468,-80.623680,-79.3061264,-78.511441,-77.559817,
                    -76.659167,-80.468470,-80.620807,-78.9671584,-78.54890126219952,-77.560750,
                    -76.568492,-80.512979,-80.624366,-79.0343698, -78.8776242,-77.557221))

### Obtain sunset and sunrise times during the season at each location during the time the crop is established.
###This will be merged into the dataset to calculate nighttime and daytime hours.

site_list_22 =
  expand.grid(date = seq.Date(as.Date("2022-01-01"), as.Date("2022-06-01"), by = 1), 
              sites=c("PY22","MR22","SB22","RW22", "CL22", "KS22")) %>% # create a data frame to get sunlight data from Jan to May
  mutate(month=month(date),
         doy = yday(date)) %>%
  filter(month %in% c(1,2,3,4,5,6))


site_list_23 =
  expand.grid(date = seq.Date(as.Date("2023-01-01"), as.Date("2023-06-01"), by = 1), 
              sites=c("PY23","UN23","SB23","LB23", "OX23", "KS23")) %>% # create a data frame to get sunlight data from Jan to May
  mutate(month=month(date),
         doy = yday(date)) %>%
  filter(month %in% c(1,2,3,4,5,6))

site_list_24 =
  expand.grid(date = seq.Date(as.Date("2024-01-01"), as.Date("2024-06-01"), by = 1), 
              sites=c("BE24","AL24","SB24","LB24", "RO24", "KS24")) %>% # create a data frame to get sunlight data from Jan to May
  mutate(month=month(date),
         doy = yday(date)) %>%
  filter(month %in% c(1,2,3,4,5,6))

dt_list=rbind(site_list_22,site_list_23,site_list_24) %>%
      left_join(.,dt,by="sites")

### Obtain sunrise and sunset times for each site and date ----------------------------------------------------------
sun = getSunlightTimes(data = dt_list, keep = c("sunrise", "sunset"), tz = "EST")

### Add DOY column to the sun data
sun$doy = yday(sun$date)

### Join sun data with dt_list and remove unnecessary columns
sun = left_join(sun, dt_list, by = c("date", "lat", "lon", "doy")) %>% select(-date, -lat, -lon)

### Annotate daytime and nighttime hours at each location
data_weather=left_join(data_weather,sun,by= c("DOY"="doy","SITE"="sites")) %>%
  group_by(YEAR,SITE) %>%
  mutate(PERIOD1=ifelse(TIME>=sunrise & TIME<=sunset ,"daytime","nighttime"), # period 1 will be using to get daytime and nighttime hours at each location 
         PERIOD3=rleid(PERIOD1)) %>% # we want to keep nighttime hours consecutive when grouping by. If group by DATE, we end up with nonconsecutive nighttime hours, instead of the same day.
  select(-sunrise,-sunset,-month) %>%
  group_by(YEAR,SITE) %>%
  mutate(PERIOD2 = case_when(PERIOD1=="nighttime" & lag(PERIOD1)=="daytime"~"dusk",
                             PERIOD1=="daytime" & lag(PERIOD1)=="nighttime"~"dawn")) %>%
  ungroup() 

View(data_weather) # view results

### Function to add dawn periods
add_dawn = function(df) {
  index_dusk = which(df$PERIOD2 == "dawn")
  for (index in index_dusk) {
    start_row = max(1, index - 4)
    end_row = min(nrow(df), index + 3)
    df$PERIOD2[start_row:end_row] = "dawn"
  }
  return(df)
}

### Function to add dusk periods
add_dusk = function(df) {
  index_dusk = which(df$PERIOD2 == "dusk")
  for (index in index_dusk) {
    start_row = max(1, index - 3)
    end_row = min(nrow(df), index +4)
    df$PERIOD2[start_row:end_row] = "dusk"
  }
  return(df)
}

### Group by the "SITE" column and apply the function within each group
data_weather = data_weather %>%
  group_by(SITE, YEAR) %>%
  group_modify(~ add_dusk(.x)) %>%
  group_modify(~ add_dawn(.x))

View(data_weather) # view results

### Check for missing data -----------------------------------------------------------------------------------  
anyNA(data_weather$RNF)
anyNA(data_weather$TMP)
anyNA(data_weather$HMD)
anyNA(data_weather$DEW)
anyNA(data_weather$DATE)

### Plot data ----------------------------------------------------------------------------------- 
data_weather %>%
  ggplot() + 
  geom_path(aes(x=DOY, y=TMP),colour="red",linewidth=1)+
  facet_wrap(vars(SITE))

data_weather %>%
  ggplot() + 
  geom_path(aes(x=DOY, y=DEW),colour="blue",linewidth=1)+
  facet_wrap(vars(SITE))


data_weather %>%
  ggplot() + 
  geom_path(aes(x=DOY, y=RNF),colour="black",linewidth=1)+
  facet_wrap(vars(SITE))


### Save -----------------------------------------------------------------------------------
save(data_weather, file = "data/weather_data.RData")


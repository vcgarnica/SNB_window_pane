#############################
######### Heatmaps ########## 
#############################

####### Author: Vinicius Garnica
####### Date: Mar, 2025

### Load packages ------------------------------------------------------------------------------
pacman::p_load(data.table,
               tidyverse,
               ggthemes,
               ggpmisc,
               cowplot,
               officer,
               flextable,
               gridExtra,
               patchwork)


theme_set(theme_bw())

### Load data sets ------------------------------------------------------------------------------ 
rm(list = ls())
load("results/result_5.RData")
load("results/result_10.RData")
load("results/result_15.RData")
load("results/result_20.RData")
load("results/result_25.RData")
load("results/result_30.RData")

    
### Filtering data sets by loading factor ------------------------------------------------------------------------------ 

data.fa1 = rbind(result_5,result_10,result_15,result_20,result_25,result_30) %>% ungroup() %>%
  filter(var1=="fa1" & VAR=="sev")     

data.fa2 = rbind(result_5,result_10,result_15,result_20,result_25,result_30) %>%ungroup() %>%
  filter(var1=="fa2" & VAR=="sev" )     

data.fa3 = rbind(result_5,result_10,result_15,result_20,result_25,result_30) %>%ungroup() %>%
  filter(var1=="fa3" & VAR=="sev")       

### Functions ------------------------------------------------------------------------------

process_data = function(data) {
  data = data %>%
    mutate(
      significant = case_when(
        mean <= 0 & upper_95 >= 0 ~ FALSE,
        mean < 0 & upper_95 < 0 ~ TRUE,
        mean > 0 & lower_95 > 0 ~ TRUE,
        mean >= 0 & lower_95 <= 0 ~ FALSE,
        mean == 0 | lower_95 == 0 | upper_95 == 0 ~ FALSE
      ),
      Correlation = case_when(
        mean >= 0.7 & significant == TRUE ~ "Strongly positive",
        mean <= -0.7  & significant == TRUE ~ "Strongly negative",
        mean >= 0.4 & mean < 0.7  & significant == TRUE ~ "Moderatly positive",
        mean <= -0.4 & mean > -0.7 & significant == TRUE~ "Moderatly negative",
        mean > -0.4 & mean < 0.4  & significant == TRUE~ "Non-significant or weak",
        significant == FALSE ~ "Non-significant or weak"
      ),
      Support = ifelse(
        (significant == FALSE),
        "Drop",
        "Stay"
      )
    )
  
  # Number of distinct variables in var2 (assuming var2 is a column in the data)
  n_distinct_var2 = n_distinct(na.omit(data$var2))
  
  # Max and min correlation values
  correlation_summary = data %>%
    ungroup() %>%
    summarise(
      max_mean = max(mean, na.rm = TRUE),
      min_mean = min(mean, na.rm = TRUE)
    )
  
  # Reorder the correlation levels
  data$Correlation = factor(
    data$Correlation,
    levels = rev(c("Strongly negative", "Moderatly negative", "Non-significant or weak", "Moderatly positive", "Strongly positive"))
  )
  
  # Returning the modified data and the additional information as a list
  list(
    modified_data = data,
    n_distinct_var2 = n_distinct_var2,
    correlation_summary = correlation_summary
  )
}


filter_data = function(data,var1) {
  
  results = data %>%
    complete(var2, LAG = seq(-40, 100), fill = list(var1 = {{var1}}, significant = FALSE)) %>% # fill NA for LAG observations that are non-significant
    arrange(var2,LAG) %>%
    group_by(var2) %>%
    mutate(consecutive = rleid(Support)) %>%
    group_by(var1, var2,consecutive) %>%
    arrange(var2,LAG) %>%
    mutate(c_consecutive = n()) %>%
    filter(c_consecutive>=7 & Correlation %in% c("Moderatly negative","Strongly negative", "Strongly positive","Moderatly positive")) %>%
    ungroup() %>%
    select(-consecutive) %>%
    distinct(var1, var2)
  
  ### Combine
  filtered_data = left_join(results,data)
  
  # Number of distinct variables in var2 (assuming var2 is a column in the data)
  n_distinct_var2 = n_distinct(na.omit(filtered_data$var2))
  
  # Max and min correlation values
  correlation_summary = filtered_data %>%
    ungroup() %>%
    summarise(
      max_mean = max(mean, na.rm = TRUE),
      min_mean = min(mean, na.rm = TRUE)
    )
  
  # Returning the modified data and the additional information as a list
  list(
    filtered_data = filtered_data,
    n_distinct_var2 = n_distinct_var2,
    correlation_summary = correlation_summary
  )
}



heatmap_plot = function(data, var1, palette) {
  
  # Returning the time series boundaries
  lag_max = max(data$LAG)
  lag_min = min(data$LAG)
  
  # Data wrangling
  a = data %>%
    filter(var1=={{var1}}) %>%
    ungroup() %>%
    complete(var2, LAG = seq(lag_min-4,lag_max+5, by = 1), var1, fill = list(Correlation = "Non-significant or weak")) %>%  # filling the dataset that was removed during stability selection with non-significant values
    group_by(var2) %>%
    mutate(consecutive_count = max(1, cumsum(Correlation %in% c("Strongly positive", "Moderatly positive")) + 1),
           max_consecutive = max(consecutive_count)*LAG) %>%
    ungroup() %>%
    mutate(
      var1 = case_when(var1=="fa1"~as.character("hat(λ)[1]"),
                       var1=="fa2"~as.character("hat(λ)[2]"),
                       var1=="fa3"~as.character("hat(λ)[3]"))) 
  
  p = ggplot(a, aes(x = LAG, y=reorder(var2,max_consecutive), fill = mean)) +
    geom_tile(color = "white", linewidth = 0.2) +
    labs(y = NULL,x = "Days relative to anthesis",fill = expression(italic(hat(rho)^"*")))+
    coord_equal(xlim = c(lag_max, lag_min)) +
    facet_grid(col = vars(var1), labeller = label_parsed) + 
    scale_fill_viridis_c(option = "plasma",direction = -1,na.value = palette)+
    scale_x_reverse(breaks = seq(100, -40, by = -10)) +
    theme(legend.position = "bottom", 
          axis.text.y = element_text(size=7),
          strip.text = element_blank(),
          axis.title.x = element_text(size=12,margin = margin(t = 12)),
          strip.text.x = element_blank(),
          plot.title = element_text(size=20),
          strip.background = element_blank())
  
  return(p)
}


calculate_area = function(subset_data, sev_data, fa) {
  var_sym = sym(unique(subset_data$var2))
  lag_input = subset_data$LAG
  
  column_input = sev_data %>%
    select(SITE, DATE, LAG, !!var_sym)
  
  row_input = column_input %>%
    filter(LAG %in% lag_input)
  
  row_input %>%
    group_by(SITE) %>%
    summarise(area = sum(!!var_sym)) %>%
    rename(!!paste0(fa, max(subset_data$LAG), '_', min(subset_data$LAG), '.', var_sym) := area)
}

summ_function = function(data){
  res = data %>%
    dplyr::summarise(across(where(is.numeric), list(
      na = ~sum(is.na(.)),
      min = ~round(min(., na.rm = TRUE),1),
      q1 = ~round(quantile(., 0.25, na.rm = TRUE),1),
      mean = ~round(mean(., na.rm = TRUE),1),
      q3 = ~round(quantile(., 0.75, na.rm = TRUE),1),
      max = ~round(max(., na.rm = TRUE),1)
    ),
    .names = "{.col}_{.fn}")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable_statistic",
      values_to = "value"
    ) %>%
    mutate(variable_statistic = sub("_(?!.*_)", "_DELIMITER_", variable_statistic, perl = TRUE)) %>%
    separate(variable_statistic, into = c("variable", "statistic"), sep = "_DELIMITER_") %>%
    pivot_wider(names_from = statistic, values_from = value)
  return(res)
}

export_word_table = function(data, file_path) {
  
  doc = read_docx()
  
  ft = flextable(data) %>%
    fontsize(size = 7, part = "all") %>%          
    font(fontname = "Times New Roman", part = "all")
  
  doc = doc %>%
    body_add_flextable(value = ft) %>%
    body_add_par("", style = "Normal") 
  
  print(doc, target = file_path)
}



### Descriptive analysis  ------------------------------------------------------------------------------

process_data(rbind(result_5,result_10,result_15,result_20,result_25,result_30))


### FA1  ---------------------------------------------------------------------------------------

### Data wrangling
fa1 = process_data(data.fa1);fa1
fa1.filtered = filter_data(fa1$modified_data,"fa1");fa1.filtered

### Summaries
fa1.filtered

### Plots
heatmap_plot(fa1.filtered$filtered_data, "fa1","#21918c") 
ggsave("figures/fig2.tiff", width =12,height =7,units = "in",limitsize = FALSE)


heatmap_plot(fa1$modified_data, "fa1","#21918c") 
ggsave("figures/fa1_total.png", width =25,height =90,units = "in",limitsize = FALSE)


### FA2  ---------------------------------------------------------------------------------------

### Data wrangling
fa2 = process_data(data.fa2);fa2
fa2.filtered = filter_data(fa2$modified_data,"fa2")

### Summaries
fa2.filtered

### Plots
heatmap_plot(fa2.filtered$filtered_data, "fa2","#bc3754") 
ggsave("figures/fig3.tiff", width =12,height =5,units = "in",limitsize = FALSE)

heatmap_plot(fa2$modified_data, "fa2","#bc3754") 
ggsave("figures/fa2_total.png", width =25,height =90,units = "in",limitsize = FALSE)

### FA3  ---------------------------------------------------------------------------------------

### Data wrangling
fa3 = process_data(data.fa3);fa3
fa3.filtered = filter_data(fa3$modified_data,"fa3")

### Summaries
fa3.filtered

### Plots
heatmap_plot(fa3.filtered$filtered_data, "fa3","deeppink3") 
ggsave("figures/fig4.tiff", width =12,height =7,units = "in",limitsize = FALSE)

heatmap_plot(fa3$modified_data, "fa3","deeppink3") 
ggsave("figures/fa3_total.png", width =25,height =90,units = "in",limitsize = FALSE)




#######################################
######### Creating predictors ######### 
#######################################


### Load window pane data ---------------------------------------------------------------------------------------

load('data/rolling_windows.RData')

sev_data=map(rolling_windows, ~filter(.x, VAR == 'sev'))

sev_data=sev_data %>%
  reduce(function(x, y) full_join(x, y, by = c('SITE','DATE','DOY','fa1','fa2','fa3','VAR', 'LAG')))


site_df = data.frame(SITE = factor(unique(sev_data$SITE)))

### FA1 second-level feature engineering ---------------------------------------------------------------------------------------

# Step 1: Filter and process consistent variables
consistent_fa1_vars =
  fa1.filtered$filtered_data %>%
  filter(significant == TRUE) %>%
  select(var1, var2, LAG, significant) %>%
  complete(var2, LAG = seq(-40, 80), fill = list(var1 = "fa1", significant = FALSE)) %>%
  arrange(var2,desc(LAG)) %>%
  mutate(run_id = data.table::rleid(significant)) %>%
  group_by(var2, run_id) %>%
  mutate(run_length = n()) %>%
  ungroup() %>%
  filter(significant == TRUE & run_length >= 7)%>%
  distinct(var2, LAG)



# Step 2: Split data based on LAG differences
split_data_fa1 = consistent_fa1_vars %>%
  group_by(var2) %>%
  group_split() %>%
  map(function(df) {
    if (any(abs(diff(df$LAG)) > 3)) {
      split_points = which(abs(diff(df$LAG)) >3)
      split1 = df[1:split_points[1], ]
      split2 = df[(split_points[1] + 1):nrow(df), ]
      list(split1, split2)
    } else {
      list(df)
    }
  }) %>%
  flatten() 


# Step 3: Calculate area and Reduce dataset
fa1_df = reduce(map(split_data_fa1, ~ calculate_area(.x, sev_data,"fa1.")), left_join, by = "SITE") %>%
  left_join(site_df, by = "SITE") 




### FA2 second-level feature engineering ---------------------------------------------------------------------------------------

# Step 1: Filter and process consistent variables
consistent_fa2_vars =
  fa2.filtered$filtered_data %>%
  filter(significant == TRUE) %>%
  select(var1, var2, LAG, significant) %>%
  complete(var2, LAG = seq(-40, 80), fill = list(var1 = "fa2", significant = FALSE)) %>%
  arrange(var2,desc(LAG)) %>%
  mutate(run_id = data.table::rleid(significant)) %>%
  group_by(var2, run_id) %>%
  mutate(run_length = n()) %>%
  ungroup() %>%
  filter(significant == TRUE & run_length >= 7)%>%
  distinct(var2, LAG)


# Step 2: Split data based on LAG differences
split_data_fa2 = consistent_fa2_vars %>%
  group_by(var2) %>%
  group_split() %>%
  map(function(df) {
    if (any(abs(diff(df$LAG)) > 3)) {
      split_points = which(abs(diff(df$LAG)) >3)
      split1 = df[1:split_points[1], ]
      split2 = df[(split_points[1] + 1):nrow(df), ]
      list(split1, split2)
    } else {
      list(df)
    }
  }) %>%
  flatten() 



# Step 3: Calculate area and Reduce dataset
fa2_df = reduce(map(split_data_fa2, ~ calculate_area(.x, sev_data,"fa2.")), left_join, by = "SITE") %>%
  left_join(site_df, by = "SITE")





### FA3 second-level feature engineering ---------------------------------------------------------------------------------------

# Step 1: Filter and process consistent variables
consistent_fa3_vars =
  fa3.filtered$filtered_data %>%
  filter(significant == TRUE) %>%
  select(var1, var2, LAG, significant) %>%
  complete(var2, LAG = seq(-40, 80), fill = list(var1 = "fa1", significant = FALSE)) %>%
  arrange(var2,desc(LAG)) %>%
  mutate(run_id = data.table::rleid(significant)) %>%
  group_by(var2, run_id) %>%
  mutate(run_length = n()) %>%
  ungroup() %>%
  filter(significant == TRUE & run_length >= 7)%>%
  distinct(var2, LAG)


# Step 2: Split data based on LAG differences
split_data_fa3 = consistent_fa3_vars %>%
  group_by(var2) %>%
  group_split() %>%
  map(function(df) {
    if (any(abs(diff(df$LAG)) > 3)) {
      split_points = which(abs(diff(df$LAG)) >3)
      split1 = df[1:split_points[1], ]
      split2 = df[(split_points[1] + 1):nrow(df), ]
      list(split1, split2)
    } else {
      list(df)
    }
  }) %>%
  flatten() 


# Step 3: Calculate area and Reduce dataset
fa3_df = reduce(map(split_data_fa3, ~ calculate_area(.x, sev_data,"fa3.")), left_join, by = "SITE") %>%
  left_join(site_df, by = "SITE")

## Data wrangling -----------------------------------------------------------------------

# List of data frames

# Reduce dataset
weather_library = reduce(list(fa1_df,fa2_df,fa3_df), left_join, by = "SITE")

save(weather_library, file = "data/weather_library.RData")


## Export -----------------------------------------------------------------------

fa1_result = split_data_fa1 %>%
  map(~ .x %>%
        group_by(var2) %>%
        summarise(max_LAG = max(LAG), 
                  min_LAG = min(LAG),
                  Duration =max_LAG-min_LAG +1)) %>%
  do.call(rbind,.) %>%
  mutate(second_level = paste0("fa1.", max_LAG, "_", min_LAG, ".", var2)) %>%
  left_join(summ_function(fa1_df), by = c("second_level" = "variable")) %>%
#  select(-second_level) %>%
  arrange(desc(max_LAG)) %>% 
  na.omit()

export_word_table(fa1_result, "results/fa1_variables.docx")


fa2_result = split_data_fa2 %>%
  map(~ .x %>%
        group_by(var2) %>%
        summarise(max_LAG = max(LAG), 
                  min_LAG = min(LAG),
                  Duration =max_LAG-min_LAG +1)) %>%
  do.call(rbind,.) %>%
  mutate(second_level = paste0("fa2.", max_LAG, "_", min_LAG, ".", var2)) %>%
  left_join(summ_function(fa2_df), by = c("second_level" = "variable")) %>%
#  select(-second_level) %>%
  arrange(desc(max_LAG))%>% 
  na.omit()


export_word_table(fa2_result, "results/fa2_variables.docx")

fa3_result = split_data_fa3 %>%
  map(~ .x %>%
        group_by(var2) %>%
        summarise(max_LAG = max(LAG), 
                  min_LAG = min(LAG),
                  Duration =max_LAG-min_LAG +1)) %>%
  do.call(rbind,.) %>%
  mutate(second_level = paste0("fa3.", max_LAG, "_", min_LAG, ".", var2)) %>%
  left_join(summ_function(fa3_df), by = c("second_level" = "variable")) %>%
 # select(-second_level) %>%
  arrange(desc(max_LAG))%>% 
  na.omit()



export_word_table(fa3_result, "results/fa3_variables.docx")



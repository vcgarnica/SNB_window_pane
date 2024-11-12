######################################
######### Association plots ########## 
######################################

####### Author: Vinicius Garnica
####### Date: Oct, 2024


### Load packages ------------------------------------------------------------------------------
pacman::p_load(purrr,
               patchwork,
               tidyverse,
               gridExtra)


theme_set(theme_bw())
### Load data sets ------------------------------------------------------------------------------ 
rm(list = ls())
load("data/weather_library.RData")
SNB_data = read.csv("data/sev_final.csv")

# Function to create plots ------------------------------------------------------------------------------ 
plot_predictor = function(predictor) {
  a=SNB_data %>% group_by(SITE) %>% summarise(sev=mean(sev, na.rm = TRUE)) %>%
    left_join(.,weather_library)
  
  ggplot(a ,aes_string(x = paste0("`", predictor, "`"), y = "sev")) +
    geom_point() +
    geom_smooth(method="lm")+
    #geom_smooth(method = "gam",formula =y ~ s(x, bs = "cs"))+
    labs(title = predictor,
         x = "Cumulative hours over optimal period",
         y = "SNB severity (%)")+
    theme(plot.title = element_text(face = "italic"))+
    coord_cartesian(ylim = c(0, 30))
}


# Function to save plots in batches
save_plots_in_batches = function(predictors, batch_size = 20) {
  num_batches = ceiling(length(predictors) / batch_size)
  for (i in 1:num_batches) {
    start_index = (i - 1) * batch_size + 1
    end_index = min(i * batch_size, length(predictors))
    predictors_batch = predictors[start_index:end_index]
    
    plots = map(predictors_batch, plot_predictor)
    
    filename = paste0("figures/predictors_batch_", i, ".png")
    g = arrangeGrob(grobs = plots, ncol = 5)
    ggsave(filename, g, width = 20, height = 16)
  }
}

### Filter numeric predictors ------------------------------------------------------------------------------ 
numeric_predictors = weather_library %>%
  select(where(is.numeric)) %>%
  names()

# Save the plots ------------------------------------------------------------------------------ 
save_plots_in_batches(numeric_predictors)


# Plot selected variables ------------------------------------------------------------------------------ 

a = SNB_data %>%
  group_by(SITE) %>%
  summarise(sev = mean(sev, na.rm = TRUE)) %>%
  left_join(weather_library) %>%
  select(sev, 
         "fa1.32_26.TR.19T22nR.G0.2.daytime.sum_20", 
         "fa1.-9_-18.TRH.13T16nRH.G80.dusk.sum_30", 
        "fa3.52_44.T.3T7.dusk.sum_15",
        "fa2.3_-3.T.22T25.dusk.sum_30") %>%
  pivot_longer(cols = !sev, names_to = "predictor")

# Define the order of the predictors
predictor_order <- c("fa1.32_26.TR.19T22nR.G0.2.daytime.sum_20", 
                     "fa1.-9_-18.TRH.13T16nRH.G80.dusk.sum_30",
                     "fa2.3_-3.T.22T25.dusk.sum_30",
                     "fa3.52_44.T.3T7.dusk.sum_15")

# Reorder the 'predictor' column based on the specified order
a$predictor <- factor(a$predictor, levels = predictor_order)

# Create individual plots for each predictor
plot_list = a %>%
  split(.$predictor) %>%
  map(~ ggplot(.x, aes(x = value, y = sev)) +
        geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, linewidth = 1.7, colour = "gray50", fill = "gray80") +
        geom_point(size = 2, alpha = 0.5) +
        labs(x = "Cumulative hours or events over the optimal epidemiological period",
             y = "Average SNB severity (%)") +
        theme(panel.spacing = unit(1, "lines"),
              text = element_text(size = 10),
              strip.text = element_text(face = "italic", size = 9)) +
        coord_cartesian(ylim = c(0, 30)))

# Combine the plots using patchwork
wrap_plots(plot_list, ncol = 2) + plot_annotation(tag_levels = 'A') + plot_layout(axis_titles =  "collect") & theme(text = element_text(size = 12),
                                                                                                                    plot.tag.position = c(-0.05, 1),   # Moves the tag further from the plots
                                                                                                                    plot.margin = margin(15, 15, 15, 15),  # Increases margin around the plot
                                                                                                                    axis.title.x = element_text(margin = margin(t = 10)),  # Adds space between x-axis title and the plot
                                                                                                                    axis.title.y = element_text(margin = margin(r = 10)))

ggsave("figures/fig5.tiff",width =6.5,height = 6.2,units = "in",limitsize = FALSE)

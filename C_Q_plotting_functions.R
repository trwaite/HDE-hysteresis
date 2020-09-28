# plots C-Q relationships for up to 4 variables
# specify start date and end date
# optionally specify which site to plot (otherwise, both will be included in each plot)
multi_var_CQ <- function (var1, var2 = NA, var3 = NA, var4 = NA, startDate, endDate, site1 = NA){
  # subset the data between the given start and end dates
  subset <- WQ_hourly_discharge %>% 
    filter(date >= ymd(startDate) & date <= ymd(endDate)) %>% 
    arrange(dateTime)
  # subset to the given site if it's provided
  if(!is.na(site1)){
    subset <- subset %>% 
      filter(site == site1)
  }
  
  # find the peak discharge for the storm
  peak <- subset %>% filter(hourlyDischarge == max(hourlyDischarge)) 
  peakDateTime <- peak[["dateTime"]]
  
  # classify each observation as rising limb or falling limb
  subset <- subset %>% 
    mutate(limb = case_when(dateTime <= peakDateTime ~ "RL", T ~ "FL"))
  
  
  # plot the first variable
  a <-  subset %>%  
    ggplot(aes_string(x = "hourlyDischarge", y = var1, 
                      col = case_when(!is.na(site1) ~ "limb", T ~ "site"))) +
    geom_path() +
    # start points
    geom_point(data = subset[subset$site == "MC",][1,], 
               aes_string("hourlyDischarge", y = var1), col = "black") +
    geom_point(data = subset[subset$site == "SI",][1,], 
               aes_string("hourlyDischarge", y = var1), col = "black") +
    xlab ("Discharge (ft^3/s)") + ylab(y_axis_label(var1)) +
    theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA),
                       legend.position = "none")
  
  
  figure <- a
  
  # plot the second variable if it's given
  if(!is.na(var2)){
    b <-  subset %>%  
      ggplot(aes_string(x = "hourlyDischarge", y = var2, 
                        col = case_when(!is.na(site1) ~ "limb", T ~ "site"))) +
      geom_path() +
      # start points
      geom_point(data = subset[subset$site == "MC",][1,], 
                 aes_string("hourlyDischarge", y = var2), col = "black") +
      geom_point(data = subset[subset$site == "SI",][1,], 
                 aes_string("hourlyDischarge", y = var2), col = "black") +
      theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA),
                         legend.position = "none") +
      xlab ("Discharge (ft^3/s)") + ylab(y_axis_label(var2)) 
    
    figure <- ggarrange(a, b)
  }
  
  # plot the third variable if it's given
  if(!is.na(var3)){
    c <-  subset %>%  
      ggplot(aes_string(x = "hourlyDischarge", y = var3, 
                        col = case_when(!is.na(site1) ~ "limb", T ~ "site"))) +
      geom_path() +
      # start points
      geom_point(data = subset[subset$site == "MC",][1,], 
                 aes_string("hourlyDischarge", y = var3), col = "black") +
      geom_point(data = subset[subset$site == "SI",][1,], 
                 aes_string("hourlyDischarge", y = var3), col = "black") +
      theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA),
                         legend.position = "none") +
      xlab ("Discharge (ft^3/s)") + ylab(y_axis_label(var3)) 
    
    figure <- ggarrange(a, b, c)
  }
  
  # plot the fourth variable if it's given
  if(!is.na(var4)){
    d <-  subset %>%  
      ggplot(aes_string(x = "hourlyDischarge", y = var4, 
                        col = case_when(!is.na(site1) ~ "limb", T ~ "site"))) +
      geom_path() +
      # start points
      geom_point(data = subset[subset$site == "MC",][1,], 
                 aes_string("hourlyDischarge", y = var4), col = "black") +
      geom_point(data = subset[subset$site == "SI",][1,], 
                 aes_string("hourlyDischarge", y = var4), col = "black") +
      theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA),
                         legend.position = "none") +
      xlab ("Discharge (ft^3/s)") + ylab(y_axis_label(var4)) 
    
    figure <- ggarrange(a, b, c, d)
  }
  # show the figure
  figure
}


## generate y-axis labels for C-Q plots
y_axis_label <- function(var){
  if(var == "Turb") return ("Turbidity (NTU)")
  if(var == "NO3_mgL") return ("[Nitrate] (mg/L)")
  if(var == "CHLugL") return ("[Chlorophyll] (ug/L)")
  if(var == "FDOMqsu") return("fDOM (QSU)")
  if(var == "BGAugL") return("Cyanobacteria")
}

## generate y-axis labels for normalized C_Q plots
y_axis_label_n <- function(var){
  if(var == "Turb") return ("Normalized turbidity (NTU)")
  if(var == "NO3_mgL") return ("Normalized [nitrate] (mg/L)")
  if(var == "CHLugL") return ("Normalized [chlorophyll] (ug/L)")
  if(var == "FDOMqsu") return("Normalized fDOM (QSU)")
}











# given a df (data) of discharge and concentration data,
# returns a df with date-time, discharge, original concentration, 
# and smoothed concentration
# uses stl() to smooth the concentrations
# note: data must have "hourlyDischarge" and "dateTime" columns
# must also specify the frequency (# observations per day in data)
trend.cq.df <- function(data, Site, var, startDate, endDate, freq) {
  # subset the data using the start and end dates
  subset <- data %>% 
    filter(site == Site) %>% 
    filter((dateTime) >= ymd_hm(startDate) & (dateTime) <= ymd_hm(endDate)) %>% 
    arrange(dateTime)
  # make a time series of the given variable
  tsVar <- ts(subset[[var]], start = 0, frequency = freq)
  # make a time series of the discharge
  tsDis <- ts(subset[["hourlyDischarge"]], start = 0, frequency = freq)
  
  # decompose the time series
  decompVar <- stl(tsVar, s.window = "periodic")
  decompDis <- stl(tsDis, s.window = "periodic")
  
  
  # create and return a df with dateTime, discharge, 
  #original variable, and smoothed variable
  dfOut <- cbind.data.frame(subset$dateTime, subset$hourlyDischarge,subset[,var],
                            decompVar$time.series[,2],decompDis$time.series[,2] )
  names(dfOut) <- c("dateTime", "discharge", "var", "smoothVar", "smoothDis")
  return(dfOut)
}

# given a dataframe returned by trend.cq.df,
# makes a cq plot with the original concentration data and the smoothed data
plot.smooth.cq <- function(data, var) {
  data %>% 
    ggplot(aes(x = discharge, y = var)) +
    geom_path(col = "grey") +
    #geom_point(col = "grey") +
    geom_path(aes(x = smoothDis, y = smoothVar), col = "red") +
    xlab("Discharge (m^3/s)") +
    ylab(y_axis_label(var)) +
    theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA),
                       legend.position = "none")
}

# normalize utility function
normalize <- function(x, max, min){
  return((x-min)/(max-min))
}

# given a dataframe returned by trend.cq.df or ma.cq.df, 
# returns a dataframe with columns added for normalized versions of
# discharge, concentration, smoothed discharge, and smoothed concentration
normalize.cq <- function(data){
  # identify max and min concentration and discharge values
  maxC <- max(data$var)
  minC <- min(data$var)
  maxQ <- max(data$discharge)
  minQ <- min(data$discharge)
  
  # same for the smoothed data
  maxCs <- max(data$smoothVar)
  minCs <- min(data$smoothVar)
  maxQs <- max(data$smoothDis)
  minQs <- min(data$smoothDis)
  
  # add in normalized columns 
  norm.df <- data %>% 
    mutate(var_n = sapply(var, normalize, max = maxC, min = minC),
           dis_n = sapply(data$discharge, normalize, max = maxQ, min = minQ),
           smoothVar_n = sapply(smoothVar, normalize, max = maxCs, min = minCs),
           smoothDis_n = sapply(smoothDis, normalize, max = maxQs, min = minQs))
  
  return(norm.df)
}

# given a dataframe returned by normalize.cq,
# makes a cq plot of the normalized smoothed data
plot.smooth.n.cq <- function(data, var) {
  data %>% 
    ggplot(aes(x = smoothDis_n, y = smoothVar_n)) +
    geom_path() +
    xlab("Normalized discharge (m^3/s)") +
    ylab(y_axis_label_n(var)) +
    theme_bw() + theme(plot.background = element_rect(fill = "transparent",colour = NA),
                       legend.position = "none")
}

addLimbs <- function(data){
  # max discharge for normalized data has value 1
  peakDate <- (data %>% filter(smoothDis_n == 1))[[1]]
  # add column for rising or falling limb
  # RL if before peakDate, FL if after, peak if on peakDate
  dfOut <- data %>% 
    mutate(limb = case_when(dateTime < peakDate ~ "RL",
                            dateTime > peakDate ~ "FL",
                            T ~ "peak"))
  return(dfOut)
}

# utility function to find the discharge from the next data point, 
# given a dateTime and a dataframe
nextQ <- function(data, date){
  if(date == data[[nrow(data), 1]]){
    return(NA)
  }
  # get the next data point
  futureDates <- data %>% filter(dateTime > date )
  nextDate <- min(futureDates$dateTime)
  return((data %>% filter(dateTime == nextDate))[["smoothDis_n"]])
}

# utility function to find the concentration from the next data point, 
# given a dateTime and a dataframe
nextC <- function(data, date){
  if(date == data[[nrow(data), 1]]){
    return(NA)
  }
  # get the next data point
  futureDates <- data %>% filter(dateTime > date )
  nextDate <- min(futureDates$dateTime)
  return((data %>% filter(dateTime == nextDate))[["smoothVar_n"]])
}

single.hi.avg <- function(data,d){
  # add a column with the next day's smoothDis_n
  temp <- data %>% mutate(smoothDis_n_next = sapply(dateTime, nextQ, data = data))
  # get the times when d is between the discharges for this time and the next time
  segments <- temp %>% filter(d > smoothDis_n & d < smoothDis_n_next |
                                d < smoothDis_n & d > smoothDis_n_next)
  # separate the limbs
  segsRL <- segments %>% filter(limb == "RL")
  segsFL <- segments %>% filter(limb == "FL")
  
  # if there's no rising limb segment, check to see if the exact 
  # value of d is present in the rising limb
  # otherwise, throw an error
  exactRL <- F
  if(nrow(segsRL) == 0){
    exactRL <- data %>% filter((limb == "RL") & (smoothDis_n == d))
    if(length(exactRL) > 0){
      avgRL <- exactRL[["smoothVar_n"]]
      exactRL <- T
    } else{
      stop (paste0("Error: discharge value not intersected by both limbs : ",
                   as.character(d)))
    }
  }
  # do the same for the falling limb
  exactFL <- F
  if(nrow(segsFL) == 0){
    exactFL <- data %>% filter((limb == "FL") & (smoothDis_n == d))
    if(length(exactFL) > 0){
      avgFL <- exactFL[["smoothVar_n"]]
      exactFL <- T
    } else{
      stop (paste0("Error: discharge value not intersected by both limbs : ",
                   as.character(d)))
    }
  }
  
  # if the exact value wasn't in the rising limb, 
  # calculate the concentration at d using the segment
  if(!exactRL){
    sumC_RL <- 0
    # loop through each RL segment and find the concentration value
    for(i in 1:nrow(segsRL)){
      # store the concentrations and discharges of the segment ends
      RL_c1 <- segsRL$smoothVar_n[i]
      RL_c2 <- nextC(data, segsRL$dateTime[i])
      RL_q1 <- segsRL$smoothDis_n[i]
      RL_q2 <- segsRL$smoothDis_n_next[i]
      # calculate concentration at d from the segment
      RL_val <- ((RL_c2 - RL_c1)/(RL_q2 - RL_q1))*(d - RL_q1)+RL_c1
      sumC_RL <- sumC_RL + RL_val
    }
    # calculate the average concentration value
    avgRL <- sumC_RL/nrow(segsRL)
  }
  
  # do the same for the falling limb
  if(!exactFL){
    sumC_FL <- 0
    # loop through each FL segment and find the concentration value
    for(i in 1:nrow(segsFL)){
      # store the concentrations and discharges of the segment ends
      FL_c1 <- segsFL$smoothVar_n[i]
      FL_c2 <- nextC(data, segsFL$dateTime[i])
      FL_q1 <- segsFL$smoothDis_n[i]
      FL_q2 <- segsFL$smoothDis_n_next[i]
      # calculate concentration at d from the segment
      FL_val <- ((FL_c2 - FL_c1)/(FL_q2 - FL_q1))*(d - FL_q1)+FL_c1
      sumC_FL <- sumC_FL + FL_val
    }
    # calculate the average concentration value
    avgFL <- sumC_FL/nrow(segsFL)
  }
  
  # calculate the hysteresis index
  HI <- avgRL - avgFL
  
  return(HI)
}

# given normalized data and the number of intervals desired,
# returns a vector of the discharge values at which HI will be calculated
get.dis.intervals <- function(data, n) {
  # find minimum discharge values for rising and falling limbs
  mins <- data %>% filter(limb != "peak") %>% group_by(limb) %>% 
    summarise(min = min(smoothDis_n))
  # the larger of the two will be where the intervals start
  startDis <- max(mins$min)
  # return a sequence from the starting discharge to 1 (max discharge),
  # with the length specified by the number of intervals n
  return(seq(from = startDis, to = 1-((1-startDis)/n), length.out = n))
}

# for a given storm, calculates HI for a given number of intervals
# and returns a list with the average HI and the loop area
avg.hi <- function(data, n){
  sumHI <- 0
  area <- 0
  ints <- get.dis.intervals(data, n)
  intLen <- ints[n] - ints[1]
  for(i in ints){
    height <- single.hi.avg(data, i)
    sumHI <- sumHI + height
    area <- area + abs(height)
  }
  return(list(sumHI/n, (area/n)*intLen))
}

# given c and q data from a storm, calculates the slope
slope.cq <- function(data){
  # get the concentration at the storm peak
  peak <- data %>% filter(limb == "peak")
  c.peak <- as.numeric(peak[["smoothVar"]])
  # get the concentration at the start of the storm
  start <- data %>% arrange(dateTime) %>% slice_head()
  c.start <- as.numeric(start[["smoothVar"]])
  # get the maximum concentration
  c.max <- max(data$smoothVar)
  # calculate delta C
  return((c.peak-c.start)/c.max)
}

# given a dataframe with WQ and discharge, a start and end date,
# and a variable name, returns the hysteresis index using n intervals,
# as well as a plot of the smoothed, normalized c-q relationship
# and the slope of the smoothed c-q relationship
# must specify freq, the number of observations per day in the dataframe
storm_cq <- function(data, Site, startDate, endDate, var, n, freq){
  # smooth, normalize, and add limbs to the data
  data2 <- addLimbs(normalize.cq(trend.cq.df(data, Site, var, startDate, endDate, freq)))
  # plot the smoothed and normalized c-q data
  plot <- plot.smooth.n.cq(data2, var)
  # calculate the hysteresis index and loop area
  loopData <- avg.hi(data2, n)
  hi <- as.numeric(loopData[[1]])
  names(hi) <- "HI"
  area <- as.numeric(loopData[[2]])
  names(area) <- "area"
  # calculate the slope
  slope <- slope.cq(data2)
  names(slope) <- "slope"
  # return a list of the plot, hi, and slope
  return(list(plot, hi, area, slope))
}
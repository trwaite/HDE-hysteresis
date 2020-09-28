# given data from the duration of the storm,
# returns the length of the storm in days
storm.length <- function(data, startDate, endDate){
  return(as.numeric(ymd(endDate)-ymd(startDate)))
}

# returns the length of the portion of the storm
# for which HI could be calculated (overlapping limbs)
storm.length.loop <- function(data){
  # find minimum discharge rows for rising and falling limbs
  mins <- data %>% filter(limb != "peak") %>% group_by(limb) %>% 
    summarise(min = min(hourlyDischarge))
  # the larger of the two is where the intervals start
  minDisLoop <- max(mins$min)
  # subset data to just the loop
  loop <- data %>% filter(hourlyDischarge >= minDisLoop)
  # get start and end dates for the loop
  start <- loop %>% summarize(start = min(dateTime))
  end <- loop %>% summarize(end = max(dateTime))
  return(as.numeric(end-start))
}

# returns the change in discharge (raw data)
# from the start of the event to the peak
storm.dis.change <- function(data){
  start <- (data %>% slice_min(dateTime))[["hourlyDischarge"]]
  peak <- (data %>% slice_max(hourlyDischarge))[["hourlyDischarge"]]
  return(as.numeric(peak)-as.numeric(start))
}

# returns the change in discharge (raw data)
# from the start of the overlapping rise/fall discharge
# (where the HI begins to be calculated) to the peak
storm.dis.change.loop <- function(data){
  # find minimum discharge values for rising and falling limbs
  mins <- data %>% filter(limb != "peak") %>% group_by(limb) %>% 
    summarise(min = min(hourlyDischarge))
  # the larger of the two is where the intervals start
  start <- max(mins$min)
  peak <- (data %>% slice_max(hourlyDischarge))[["hourlyDischarge"]]
  return(as.numeric(peak) - as.numeric(start))
}

# returns the maximum discharge value from the event
storm.max.dis <- function(data){
  maxDis <- data %>% summarise(maxDis = max(hourlyDischarge))
  return(as.numeric(maxDis))
}

# returns the baseline discharge (discharge at beginning of event)
base.dis <- function(startDate, data){
  dis <- data %>% filter(date == ymd(startDate)) %>% 
    summarise(mean = mean(hourlyDischarge))
  return(as.numeric(dis))
}

# returns the average water temperature during the event
storm.avg.temp <- function(data){
  return(as.numeric(data %>% summarise(mean = mean(Temp))))
}
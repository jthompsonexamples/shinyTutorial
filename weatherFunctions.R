getRecentObservations <- function(lat, lon, startDate=0, endDate=0){
  #debug values
  # lat <- 38.8894
  # lon <- -77.0352
  
  getAPI <- GET(paste('https://api.weather.gov/points',paste(lat,lon, sep=','),sep='/')) # go to the API
  
  dataAPI <- content(getAPI, 'text') # extract the text info into json format
  dataAPI <- fromJSON(dataAPI) # parse the json
  localTZ <- dataAPI$properties$timeZone
  ### Whats in the JSON
  stationsURL <- dataAPI$properties$observationStations
  
  stationsAPI <- GET(stationsURL)
  ### Get the nearby stations 
  
  #### Error checking
  if (stationsAPI$status_code!=200) return(errorCheck(stationsAPI$status_code))
  ####
  stationsData <- content(stationsAPI, 'text')
  stationsData <- fromJSON(stationsData)
  # get the geoData for the stations and find the closest (probably the first listed)
  
  
  coordList <- stationsData$features$geometry$coordinates
  coordTib <- tibble(lon=sapply(coordList, function(l) l[1]),
                     lat=sapply(coordList, function(l) l[2]))
  # stationTib <- bind_cols(stationsGeo, # merge with the geo data
  #                         coordTib)
  minDist <- which.min(distGeo(c(lon, lat), coordTib[,c('lon','lat')])) # get the closest
  station <- stationsData$features$properties$stationIdentifier[minDist] # do a call to that station
  obsURL <- paste0(stationsData$observationStations[minDist],'/observations')
  #obsURL <- stationsData$observationStations[minDist]
  ## Theres a few calls that we can use, look at your API documentations
  # get the observations
  
  obsAPI <- GET(obsURL)
  obsData <- content(obsAPI, 'text') # extract the text info into json format
  obsData <- fromJSON(obsData) # parse the json
  ### error checking
  if (obsAPI$status_code!=200){
    df <- data.frame(x=0, y=0, label=paste('ERROR', hourlyAPI$status_code))
    p <- ggplot(df, aes(x=x, y=y, label=label))+
      geom_label(size=16,
                 fontface='bold')+
      theme_bw()+
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
      )
    return(list(plot=p, obs=data.frame()))
  }
  ###
  # the observation call gets a lot of information, we'll just do temp
  
  obsTib <- tibble(time=obsData$features$properties$timestamp)
  obsTib <- bind_cols(obsTib, obsData$features$properties$temperature)
  obsTib <- obsTib %>%
    mutate(time=ymd_hms(time), tempF=(value*(9/5))+32) %>% # convert the temp to celcius
    mutate(time=force_tzs(time, tzone='UTC',tzone_out=localTZ)) %>% # convert the time to local
    rename(tempC=value)
  
  ### Plot it
  m <- plotRecentObservations(obsTib, station, startDate, endDate)
  # m <- ggplot(obsTib)+
  #   geom_line(aes(x=time, y=tempF, group=1))+
  #   xlab('Date')+
  #   ylab('Temperature\nF')+
  #   ggtitle(paste('Recent Observations:', station))
  obsTib$time <- as.character(obsTib$time)
  return(list(obs=obsTib, plot=m, station=station))
}

plotRecentObservations <- function(obsTib, station, startDate, endDate){
  if (startDate!=0 & endDate!=0){
    obsTib <- obsTib %>% filter(time>=startDate & time<=endDate)
  }
  obsTib$time <- ymd_hms(obsTib$time)
  m <- ggplot(obsTib)+
    geom_line(aes(x=time, y=tempF, group=1))+
    xlab('Date')+
    ylab('Temperature\nF')+
    ggtitle(paste('Recent Observations:', station))
  return(m)
}

getWeatherForcast <- function(lat, lon){
  # got into the NOAA API and get the forcast for the given coords
  path <- paste('https://api.weather.gov/points',paste(lat,lon, sep=','),sep='/')
  getAPI <- GET(path) # request from API
  dataAPI <- content(getAPI, 'text') # extract the text info into json format
  dataAPI <- fromJSON(dataAPI) # parse the json
  
  ### Whats in the JSON
  hourlyURL <- dataAPI$properties$forecastHourly
  stationTZ <- dataAPI$properties$timeZone
  hourlyAPI <- GET(hourlyURL)
  ### Get the hourly forcast
  
  #### Error checking
  
  if (hourlyAPI$status_code!=200){
    df <- data.frame(x=0, y=0, label=paste('ERROR', hourlyAPI$status_code))
    p <- ggplot(df, aes(x=x, y=y, label=label))+
      geom_label(size=16,
                 fontface='bold')+
      theme_bw()+
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
      )
    return(p)
  }
  
  hourlyData <- content(hourlyAPI, 'text')
  hourlyData <- fromJSON(hourlyData)
  forcastTib <- as_tibble(hourlyData$properties$periods)
  forcastTib <- forcastTib %>% mutate(startTime=ymd_hms(startTime, tz=stationTZ))
  
  relLocation <- paste0(dataAPI$properties$relativeLocation$properties$city,
                        ', ',
                        dataAPI$properties$relativeLocation$properties$state)
  ### Plot it
  
  p <- ggplot(forcastTib)+
    geom_line(aes(x=startTime, y=temperature, group=1))+
    xlab('Date')+
    ylab('Temperature\nF')+
    ggtitle(paste(relLocation, 'Forecast'))
  return(p)
}

getWeatherGeo <- function(){
  # go into the NOAA API and get the geo info for weather stations
  
  # check if the packages are installed, if not do it.
  
  getAPI <- GET('https://api.weather.gov/stations') # request from API
  dataAPI <- content(getAPI, 'text') # extract the text info into json format
  dataAPI <- fromJSON(dataAPI) # parse the json
  stationTib <- as_tibble(dataAPI$features$properties) # get the station data
  stationTib <-  # the elevation info is a list with the units and value
    stationTib %>%  # use mutate to pull it into two columns
    mutate(elevationUnits=elevation$unitCode,
           elevation=elevation$value)
  # station geo is a list with each coordinate a vector inside a list, extract
  coordList <- dataAPI$features$geometry$coordinates
  coordTib <- tibble(lon=sapply(coordList, function(l) l[1]),
                     lat=sapply(coordList, function(l) l[2]))
  stationTib <- bind_cols(stationTib, # merge with the geo data
                          coordTib)
  stationTib <-
    stationTib %>%
    mutate(hoverText=mapply(labelMaker, stationIdentifier, name))
  return(stationTib)
  # turn it all into a spatialPointsDataframe maybe
}

labelMaker <- function(sta, n){
  temp <- HTML(sprintf('StationID:%s <br> Name:%s',sta,n))
  #temp <- paste('StationID:',sta,' <br> Name:',n)
  return(temp)
}

errorCheck <- function(status){
  if (status!=200){
    df <- data.frame(x=0, y=0, label=paste('ERROR', status))
    p <- ggplot(df, aes(x=x, y=y, label=label))+
      geom_label(size=16,
                 fontface='bold')+
      theme_bw()+
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
      )
    return(list(plot=p, obs=data.frame()))
  }else return(NULL)
}
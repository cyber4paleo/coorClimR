library(jsonlite)
convertNeotomaPollenToDF <- function(taxonname){
  ## uses the pollen endpoint with relative abundances
  root = "http://apidev.neotomadb.org/v1/data/pollen?"
  url = paste(root, "taxonname=", taxonname, sep="")
  print(url)
  response = fromJSON(url)
  data <- response$data
  data['Latitude'] <- (data['LatitudeNorth'] + data['LatitudeSouth'])/2
  data['Longitude'] <- (data['LongitudeEast'] + data['LongitudeWest']) / 2
  fields <- c("SiteID", "SiteName", "Age", "Latitude", "Longitude")
  df <- data[fields]
  names(df) <- c("ID", "SiteName", "Age", "Latitude", "Longitude")
  return(df)
}

convertNeotomaSDToDF <- function(taxonname){
  ## uses the sample data endpoint
  root = "http://api.neotomadb.org/v1/data/sampledata?"
  url = paste(root, "taxonname=", taxonname, sep="")
  url = URLencode(url)
  print(url)
  response = fromJSON(url)
  data <- response$data ## this is the array
  data['Latitude'] <- (data['SiteLatitudeNorth'] + data['SiteLatitudeSouth'])/2
  data['Longitude'] <- (data['SiteLongitudeEast'] + data['SiteLongitudeWest']) / 2
  interpAge <- (data['SampleAgeYounger'] + data['SampleAgeOlder']) / 2
  data[which(is.na(data['SampleAge'])), ]['SampleAge'] <- interpAge
  fields <- c("SampleAge", "Latitude", "Longitude")
  df <- data[fields]
  names(df) <- c("Age", "Latitude", "Longitude")
  df <- na.omit(df)
  df <- unique(df)
  return(df)
}




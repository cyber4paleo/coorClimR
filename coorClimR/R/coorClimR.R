library(jsonlite)

getData <- function(x, y ="", t="", producer="", model="", modelVersion="", variableType="", variableUnits="", variablePeriod="", variablePeriodType="",
                    averagingPeriod="", averagingPeriodType="", resolution="", sampleID="", siteName="", siteID="", verbose=T){
  if (class(x) == "data.frame"){
    ## this is for multi-row dataframes
    df <- apply(x, 1, function(d){
      q <- getDataRow(d['Longitude'], d['Latitude'], d['Age'], producer=producer, model=model, modelVersion=modelVersion,
                      variableType=variableType, variableUnits=variableUnits, averagingPeriod=averagingPeriod, averagingPeriodType=averagingPeriodType,
                      resolution=resolution, siteID=d['siteName'], siteName=d['siteID'], sampleID=d['sampleID'], verbose=verbose)
      return(q)
    })
    df <- do.call("rbind", df)
    print(df)
    return(df)
  }else if (class(x) == "numeric"){
    ##this is for an x,y,t combination as arguments
    print("Executing for single row")
    return (getDataRow(x, y, t, producer=producer, model=model, modelVersion=modelVersion,
                       variableType=variableType, variableUnits=variableUnits, averagingPeriod=averagingPeriod, averagingPeriodType=averagingPeriodType,
                       resolution=resolution, siteID=siteID, siteName=siteName, sampleID=sampleID, verbose=verbose))
  }
}


getDataRow <- function(x, y, t, producer="", model="", modelVersion="", variableType="", variableUnits="", variablePeriod="", variablePeriodType="",
                       averagingPeriod="", averagingPeriodType="", resolution="", siteID="", siteName="", sampleID="", verbose=F){
  ##check variable types
  if (!checkNumeric(x)){
    return ("Request failed: x must be numeric.")
  }
  if (!checkNumeric(y)){
    return("Request failed: y must be numeric")
  }
  if (!checkNumeric(t)){
    return("Request failed: t must be numeric")
  }
  t <- round(t)
  ## build the query string
  root <- "http://130.211.157.239:8080/data?"
  url = root
  url = paste(url, "latitude=", y, sep="")
  url = paste(url, "&longitude=", x, sep="")
  url = paste(url, "&yearsBP=", t, sep="")
  if (producer != ""){
    url = paste(url, "&sourceProducer=", producer, sep="")
  }
  if (model != ""){
    url = paste(url, "&modelName=", model, sep="")
  }
  if (modelVersion!= ""){
    url = paste(url, "&modelVersion=", modelVersion, sep="")
  }
  if (variableType != ""){
    url = paste(url, "&variableType=", variableType, sep="")
  }
  if (variableUnits != ""){
    url = paste(url, "&variableUnits=", variableUnits, sep="")
  }
  if (variablePeriod != ""){
    url = paste(url, "&variablePeriod=", variablePeriod, sep="")
  }
  if (variablePeriodType != ""){
    url = paste(url, "&variablePeriodType=", variablePeriodType, sep="")
  }
  if (averagingPeriod != ""){
    url = paste(url, "&averagingPeriod=", averagingPeriod, sep="")
  }
  if (averagingPeriodType != ""){
    url = paste(url, "&averagingPeriodType=", averagingPeriodType, sep="")
  }
  if (resolution != ""){
    url = paste(url, "&resolution=", resolution, sep="")
  }
  if (siteName != "" && !is.na(siteName)){
    url = paste(url, "&siteName=", siteName, sep="")
  }
  if(siteID != "" && !is.na(siteID)){
    url = paste(url, "&siteID=", siteID, sep="")
  }
  if(sampleID != "" && !is.na(sampleID)){
    url = paste(url, "&sampleID=", sampleID, sep="")
  }
  url = URLencode(url)
  if (verbose){
    print(url)
  }
  ## execute the request to the API data service
  response <- fromJSON(url)
  if (response$success){
    return(response$data)
  }else{
    return(paste("Request failed.  Server says", response['message']))
  }
}

checkNumeric <- function(x) is.numeric(x) & !is.na(x)


# convertNeotomaPollenToDF <- function(taxonname, ageold="", ageyoung="", loc="", gpid="", altmin="", altmax=""){
#   ## uses the pollen endpoint with relative abundances
#   root = "http://apidev.neotomadb.org/v1/data/pollen?"
#   url = paste(root, "taxonname=", taxonname, sep="")
#   print(url)
#   response = fromJSON(url)
#   data <- response$data
#   data['Latitude'] <- (data['LatitudeNorth'] + data['LatitudeSouth'])/2
#   data['Longitude'] <- (data['LongitudeEast'] + data['LongitudeWest']) / 2
#   fields <- c("SiteID", "SiteName", "Age", "Latitude", "Longitude")
#   df <- data[fields]
#   names(df) <- c("ID", "SiteName", "Age", "Latitude", "Longitude")
#   return(df)
# }

convertNeotomaSDToDF <- function(taxonname, ageold="", ageyoung="", loc="", gpid="", altmin="", altmax=""){
  ## uses the sample data endpoint
  ## this is better than the apidev pollen endpoint, because it includes taxa other than pollen samples, like mammals 
  ## loc is longitudeWest, latitudeSouth, longitudeEast, latitudeNorth.
  root = "http://api.neotomadb.org/v1/data/sampledata?"
  url = paste(root, "taxonname=", taxonname, sep="")
  if(ageold != ""){
    url = paste(url, "&ageold=", ageold, sep="")
  }
  if(ageyoung != ""){
    url = paste(url, "&ageyoung=", ageyoung, sep="")
  }
  if (gpid != ""){
    url = paste(url, "&gpid=", gpid, sep="")
  }
  if (loc != ""){
    if (class(loc)== "numeric" && length(loc) == 4){
      url = paste(url, "&loc=", paste(loc[1], loc[2], loc[3], loc[4], sep=","), "=", sep="")
    }else{
      return("Invalid query string: Loc parameter not correct.")
    }
  }
  if (altmin != ""){
    url = paste(url, "&altmin=", altmin, sep="")
  }
  if (altmax != ""){
    url = paste(url, "&altmax=", altmax, sep="")
  }
  url = URLencode(url)
  print(url)
  response = fromJSON(url)
  data <- response$data ## this is the array
  data['Latitude'] <- (data['SiteLatitudeNorth'] + data['SiteLatitudeSouth'])/2
  data['Longitude'] <- (data['SiteLongitudeEast'] + data['SiteLongitudeWest']) / 2
  calcAge <- (data['SampleAgeYounger'] + data['SampleAgeOlder']) / 2
  data[which(is.na(data['SampleAge'])), ]['SampleAge'] <- calcAge
  fields <- c("DatasetID", "SampleAge", "Latitude", "Longitude")
  df <- data[fields]
  names(df) <- c("SampleID", "Age", "Latitude", "Longitude")
  df <- na.omit(df)
  df <- unique(df)
  return(df)
}



queryNeotoma <- function(taxonname, ageold="", ageyoung="", loc="", gpid="", altmin="", altmax=""){
  inputDF <- convertNeotomaSDToDF(taxonname, ageold=ageold, ageyoung=ageyoung, loc=loc, gpid=gpid, altmin=altmin, altmax=altmax)
  output <- getData(inputDF)
  return (output)
}

queryVertnet <- function(args){
  return()
}

queryAll <- function(args){
  return()
}
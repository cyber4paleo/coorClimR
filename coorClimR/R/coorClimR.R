library(jsonlite)

#' Generic function to dispatch calls to the climate data api service
#' @param x A dataframe with minimum columns names: Latitude, Longitude, Age OR a double precision longitude value.  If x is a dataframe, it can also accept columns siteName, sampleID, siteID to preserve object identification through the api call process
#' @param y A double precision latitude coordinate
#' @param t A double precision or integer number representing years before 1950, can be negative to represent time since 1950
#' API filter parameters
#' @param model A string specifying the model from which the climate data was created.  Default = "" (all)
#' @param modelVersion A string representing the model version that produced the output. Default = "" (all)
#' @param variableType A string representing the type of variable to return. Default = "" (all)
#' @param variableUnits A string representing the units in which the variables are measures. Default = "" (all)
#' @param variablePeriod An integer representing the measuring period for the variable. Default = "" (all)
#' @param variablePeriodType A string representing the type of measuring period for the variable. Default = "" (all)
#' @param averaingPeriod An integer representing the period of which the data has been averaged. Default= "" (all)
#' @param averagingPeriodType A string representing the type of period over which the data has been averaged.  Default = "" (all)
#' @param resolution A double precision value representing the native resolution at which the climate data was produced and stored.  Default = "" (all)
#' @param sampleID A string or integer identifier for the sample at the space-time location.  Default = "" (none)
#' @param siteID A string or integer identifier for the site at the x-y location of the sample.  Default = "" (none)
#' @param siteName A string or integer identifier for the site at the x-y location of the sample. Default = "" (none)
#' @return Outputs a data.frame representation of the api response.  Columns:
#'  From Database: variableunits, variablePeriodType, VariableType, variableID, Producer, sourceID, ModelVersion, tableName, VariableDescription, averagingPeriod, averagingPeriodType, Model, tableName
#'  Optional Identifiers:  siteName, sampleID, siteID
#'  Response Values: value, latitude, longitude, yearsBP
#' @examples
#' ## Create some data
#' t <- rbind(c(1, -122, 37, 1000), c(2, -100, 38, 1000))
#' t <- data.frame(t)
#' names(t) <- c("sampleID", "Longitude", "Latitude", "Age")
#' getData(t)
#'
getData <- function(x, y ="", t="", producer="", model="", modelVersion="", variableType="", variableUnits="", variablePeriod="", variablePeriodType="",
                    averagingPeriod="", averagingPeriodType="", resolution="", sampleID="", siteName="", siteID="", verbose=T){
  if (class(x) == "data.frame"){
    ## this is for multi-row dataframes
    print(x)
    df <- apply(x, 1, function(d){
      q <- getDataRow(d['Longitude'], d['Latitude'], d['Age'], producer=producer, model=model, modelVersion=modelVersion,
                      variableType=variableType, variableUnits=variableUnits, averagingPeriod=averagingPeriod, averagingPeriodType=averagingPeriodType,
                      resolution=resolution, siteID=d['siteName'], siteName=d['siteID'], sampleID=d['sampleID'], verbose=TRUE)
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


#' Get climate data for a single space-time coordnate
#' @param x A double precision longitude value.
#' @param t A double precision or integer number representing years before 1950, can be negative to represent time since 1950
#' API filter parameters
#' @param model A string specifying the model from which the climate data was created.  Default = "" (all)
#' @param modelVersion A string representing the model version that produced the output. Default = "" (all)
#' @param variableType A string representing the type of variable to return. Default = "" (all)
#' @param variableUnits A string representing the units in which the variables are measures. Default = "" (all)
#' @param variablePeriod An integer representing the measuring period for the variable. Default = "" (all)
#' @param variablePeriodType A string representing the type of measuring period for the variable. Default = "" (all)
#' @param averaingPeriod An integer representing the period of which the data has been averaged. Default= "" (all)
#' @param averagingPeriodType A string representing the type of period over which the data has been averaged.  Default = "" (all)
#' @param resolution A double precision value representing the native resolution at which the climate data was produced and stored.  Default = "" (all)
#' @param sampleID A string or integer identifier for the sample at the space-time location.  Default = "" (none)
#' @param siteID A string or integer identifier for the site at the x-y location of the sample.  Default = "" (none)
#' @param siteName A string or integer identifier for the site at the x-y location of the sample. Default = "" (none)
#' @return Outputs a data.frame representation of the api response.  Columns:
#'  From Database: variableunits, variablePeriodType, VariableType, variableID, Producer, sourceID, ModelVersion, tableName, VariableDescription, averagingPeriod, averagingPeriodType, Model, tableName
#'  Optional Identifiers:  siteName, sampleID, siteID
#'  Response Values: value, latitude, longitude, yearsBP
#' @examples
#' getData(-122, 37, 16000, siteName="mySite", variableType="Maximum Temperature")
#' getData(-90, 40, 900, siteName="myOtherSite", sampleID=1291, variableType="Precipitation")

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

#' Check if a number is numeric
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

#' Call the Neotoma web service, get the response, and format into a dataframe that can be ingested by the getData function
#' Function uses the Neotoma API SampleData endpoint
#' @param taxonname String The name of the taxonomic grouping that you wish tok query neotoma for.  Matches a taxon in the neotoma database
#' @param ageold Integer Oldest age, as calendar years before present, to include in results from neotoma.
#' @param ageyoung Integer Youngest age, as calendar years before present, to include in results.
#' @param loc A list of the form longitudeWest, latitudeSouth, longitudeEast, latitudeNorth that represents a bounding box in which to search for occurrences in Neotoma
#' @param gpid Integer Limit occurrences to a geopolitical entity id. Valid values provided by GeoPoliticalUnits database table in Neotoma.
#' @param altmin Integer Minimum site altitude in meters.
#' @param altmax Integer Maximum site altitude in meters.
#' @return data.frame.  Columns: sampleID (actually a Neotoma dataset ID), Latitude, Longitude, Age
#' @examples
#' convertNeotomaSDToDF("bison bison")
#' convertNeotomaSDToDF("picea")
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
  ## format the response so it can be correctly ingested by getData
  data['Latitude'] <- (data['SiteLatitudeNorth'] + data['SiteLatitudeSouth'])/2
  data['Longitude'] <- (data['SiteLongitudeEast'] + data['SiteLongitudeWest']) / 2
  calcAge <- (data['SampleAgeYounger'] + data['SampleAgeOlder']) / 2
  data[which(is.na(data['SampleAge'])), ]['SampleAge'] <- calcAge
  fields <- c("DatasetID", "SampleAge", "Latitude", "Longitude")
  df <- data[fields]
  names(df) <- c("sampleID", "Age", "Latitude", "Longitude")
  df <- na.omit(df)
  df <- unique(df)
  return(df)
}

#' Query the Neotoma API for taxon occurrences and then get the climate for each space-time occurrence held in Neotoma
#' @param taxonname String The name of the taxonomic grouping that you wish tok query neotoma for.  Matches a taxon in the neotoma database
#' @param ageold Integer Oldest age, as calendar years before present, to include in results from neotoma.
#' @param ageyoung Integer Youngest age, as calendar years before present, to include in results.
#' @param loc A list of the form longitudeWest, latitudeSouth, longitudeEast, latitudeNorth that represents a bounding box in which to search for occurrences in Neotoma
#' @param gpid Integer Limit occurrences to a geopolitical entity id. Valid values provided by GeoPoliticalUnits database table in Neotoma.
#' @param altmin Integer Minimum site altitude in meters.
#' @param altmax Integer Maximum site altitude in meters.
#' @param model A string specifying the model from which the climate data was created.  Default = "" (all)
#' @param modelVersion A string representing the model version that produced the output. Default = "" (all)
#' @param variableType A string representing the type of variable to return. Default = "" (all)
#' @param variableUnits A string representing the units in which the variables are measures. Default = "" (all)
#' @param variablePeriod An integer representing the measuring period for the variable. Default = "" (all)
#' @param variablePeriodType A string representing the type of measuring period for the variable. Default = "" (all)
#' @param averaingPeriod An integer representing the period of which the data has been averaged. Default= "" (all)
#' @param averagingPeriodType A string representing the type of period over which the data has been averaged.  Default = "" (all)
#' @param resolution A double precision value representing the native resolution at which the climate data was produced and stored.  Default = "" (all)
#' @return Outputs a data.frame representation of the api response.  Columns:
#'  From Database: variableunits, variablePeriodType, VariableType, variableID, Producer, sourceID, ModelVersion, tableName, VariableDescription, averagingPeriod, averagingPeriodType, Model, tableName
#'  Optional Identifiers:  siteName, sampleID, siteID
#'  Response Values: value, latitude, longitude, yearsBP
#' @examples
#' queryNeotoma("bison bison")
#' queryNeotoma("sedum", altmax=1500, variablePeriod=1, resolution=0.5)

queryNeotoma <- function(taxonname, ageold="", ageyoung="", loc="", gpid="", altmin="", altmax="", producer="", model="", modelVersion="",
                         variableType="", variableUnits="", variablePeriod="", variablePeriodType="",
                         averagingPeriod="", averagingPeriodType="", resolution=""){
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

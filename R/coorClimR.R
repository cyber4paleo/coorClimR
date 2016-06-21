library(jsonlite)
library(rvertnet)

#' Get Climate Data
#' @export
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
      if (d['siteName'] != "" && !is.null(d['siteName']) && !is.na(d['siteName'])){
        siteName = d['siteName']
      }else{
        siteName = ""
      }
      if (d['siteID'] != "" && !is.null(d['siteID']) && !is.na(d['siteID'])){
        siteID = d['siteID']
      }else{
        siteID = ""
      }
      if(d['sampleID'] != "" && !is.null(d['sampleID']) && !is.na(d['sampleID'])){
        sampleID = d['sampleID']
      }else{
        sampleID = ""
      }
      q <- getDataRow(d['Longitude'], d['Latitude'], d['Age'], producer=producer, model=model, modelVersion=modelVersion,
                      variableType=variableType, variableUnits=variableUnits, averagingPeriod=averagingPeriod, averagingPeriodType=averagingPeriodType,
                      resolution=resolution, siteID=siteID, siteName=siteName, sampleID=sampleID, verbose=TRUE)
    return(q)
    })
    df <- na.omit(df)
    df <-do.call("rbind", df)
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
#' @export
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
    warning ("Request failed: x must be numeric.")
    return(data.frame())
  }
  if (!checkNumeric(y)){
    warning("Request failed: y must be numeric")
    return(data.frame())
  }
  if (!checkNumeric(t)){
    warning("Request failed: t must be numeric")
    return(data.frame())
  }
  t <- round(t)
  if (t > 50000){
    warning("T is too large, development requires that t must be less than 25000")
    return(data.frame())
  }
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
    ## make sure we actually have data returned, not just an empty list
    if (length(response$data) > 0){
      return(response$data)
    }else{
      print("No results returned from niche database.")
      return(NA)
    }

  }else{
    return(paste("Request failed.  Server says", response['message']))
  }
}

#' Check if a number is numeric
#' @export
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

#' Get Neotoma Occurrence Data
#' Function uses the Neotoma API SampleData endpoint
#' @export
#' @param taxonname String The name of the taxonomic grouping that you wish to query Neotoma for.  Matches a taxon in the neotoma database
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
  if (nrow(data) == 0){
    print("Failed to get data: No data returned from Neotoma")
    return(FALSE)
  }
  ## format the response so it can be correctly ingested by getData
  data['Latitude'] <- (data['SiteLatitudeNorth'] + data['SiteLatitudeSouth'])/2
  data['Longitude'] <- (data['SiteLongitudeEast'] + data['SiteLongitudeWest']) / 2
  calcAge <- (data['SampleAgeYounger'] + data['SampleAgeOlder']) / 2
  data[which(is.na(data['SampleAge'])), ]['SampleAge'] <- calcAge
  fields <- c("DatasetID", "SampleAge", "Latitude", "Longitude") ## select only these columns
  df <- data[fields]
  names(df) <- c("sampleID", "Age", "Latitude", "Longitude") ## rename the columns so they will fit better into getData function
  df <- na.omit(df) ## get only unique columns that don't have NAs in them
  df <- unique(df)
  return(df)
}

#' Get Climate Data for Neotoma Occurrences
#' @export
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

#' Plot a climate variable through time
#' @export
#' @param climateDF A data.frame object produced by getData that has the climate data you wish to plot
#' @param responseVariable The name of the variable for the Y axis of the plot
#' @param responsePeriod The measurement period of the variable for the Y axis of the plot
#' @param title The title you want to put on the plot
#' @param plotAVG Boolean flag for plotting the mean of each yearsBP bin (binned average)
#' @param plotSD Boolean flag for plotting the standard deviation of year yearsBP bin(binned SD)
#' @param pointColor Color string representing the color in which to plot the individual points
#' @param lineColor If plotAVG and/or plotSD are true, then those lines will be plotted in this color
#' @return data.frame with the binned averages, std, and median for the dataset
#' @examples
#'  sequoia <- queryNeotoma("sequoia")
#'  makeTSPlot(sequoia, lineColor="green")
makeTSPlot <- function(climateDF, responseVariable="Precipitation", responsePeriod=1, title=paste(responseVariable, responsePeriod, "vs. Time"),
                       plotAVG=T, plotSD=TRUE, pointColor='gray', lineColor='red'){
  #subset the data
  df <- climateDF[which(climateDF$VariableType == responseVariable), ]
  df <- df[which(df$variablePeriod == responsePeriod), ]
  xVals <- df$yearsBP
  yVals <- df$value
  plot(xVals, yVals, ylab=paste(responseVariable, responsePeriod), xlab="Years Before Present", pch=20, col=pointColor)
  title(title)
  x <- na.omit(df)
  s <- ddply(t1,~yearsBP,summarise,mean=mean(value),sd=sd(value), median=median(value))
  lines(s$yearsBP, s$mean, lwd=3, col=lineColor)
  lines(s$yearsBP, s$mean + 2*s$sd, col=lineColor)
  lines(s$yearsBP, s$mean - 2*s$sd, col=lineColor)
  return(s)
}
#' Scatter two environmental variables against each other
#' @export
#' @param climateDF A data.frame object produced by getData that has the climate data you wish to plot
#' @param xVariable Name of variable type to put on the x axis
#' @param yVariable Name of variable type to put on the y axis
#' @param xPeriod The measurement period of the variable for the X axis of the plot
#' @param yPeriod The measurement period of the variable for the T axis of the plot
#' @param pointColor Color string in which to plot the xy points
#' @param plotModern boolean flag indicating whether to plot the modern points in a different color than the background points
#' @param modernColor if plotModern is TRUE, then the modern points will be plotted in this color
#' @param title The title to give the plots
#' @return
#' VOID
#' @examples
#' d <- queryNeotoma("ilex")
#' makeScatterPlot(d, xVariable='Maximum Temperature', yVariable='Minimum Temperature', xPeriod=7, yPeriod=1, modernColor='blue')

makeScatterPlot <- function(climateDF, xVariable="Precipitation", yVariable="Maximum Temperature", xPeriod=1, yPeriod=1,
                            pointColor='gray', plotModern=TRUE, modernColor='black', title=paste(xVariable, xPeriod, "vs.\n", yVariable, yPeriod)){
  xdf <- climateDF[which(climateDF$VariableType == xVariable), ]
  xdf <- xdf[which(xdf$variablePeriod == xPeriod), ]
  ydf <- climateDF[which(climateDF$VariableType == yVariable), ]
  ydf <- ydf[which(ydf$variablePeriod == yPeriod), ]
  if (nrow(ydf) != nrow(xdf)){
    warning("Length of x and y differ.  Did you enter the variables correctly?")
    return(F)
  }
  xValues = xdf$value
  yValues = ydf$value
  plot(xValues, yValues, col=pointColor, xlab=paste(xVariable, xPeriod), ylab=paste(yVariable, yPeriod))
  if (plotModern){
    yModern <- ydf[which(ydf$yearsBP == 0), ]
    xModern <- xdf[which(xdf$yearsBP == 0), ]
    points(xModern$value, yModern$value, col=modernColor)
  }
  m <- lm(xValues~yValues)
  modsum <- summary(m)
  r2 <- modsum$adj.r.squared
  print(paste("R2 Value: ", r2))
  title(title)
}


#' Use Vertnet API to get data, and return only specific columns needed.
#' @export
#' @param taxonname string: Name(s) of the taxonomic grouping that you wish to query Vertnet for.
#' @param genus string: Target genus name(s).
#' @param species string: Target species name(s).
#' @param state string: Target state name(s).
#' @param limit numeric: Number of results that you would like to accept. Defaults to 10000 if left empty.
#' @return output data.frame: Lat, Lon, and Age data.
#' @examples 
#' convertVertnettoDF("bison")
#' convertVertnettoDF("(kansas state OR KSU)", limit = 200)
#' convertVertnettoDF(genus = "mustela", species = "(nivalis OR erminea)")
#'
convertVertnettoDF <- function(taxonname, genus = "", species = "", state = "", limit = ""){

  # The default search has 1000 limit. However, if no limit is given, set to 100,000
  if (limit == ""){
    limit = 100
  }
  # API data request
  response <- vertsearch(taxonname, genus=genus, species=species, state=state, limit=limit, compact = TRUE, verbose = TRUE)

  # Get the specific column data that we need from the response.
  # lat, lon, age
  df <- response$data[c("decimallongitude", "decimallatitude", "year")]
  names(df) <- c("Longitude", "Latitude", "Age")
  df <- data.matrix(df)
  df <- data.frame(df)
  df <- na.omit(df)
  df <- unique(df)
  return(df)
}

#' Get climate data for Vertnet occurrences.
#' @export
#' @param taxonname string: Name(s) of the taxonomic grouping that you wish to query Vertnet for.
#' @param genus string: Target genus name(s).
#' @param species string: Target species name(s).
#' @param state string: Target state name(s).
#' @param limit numeric: Number of results that you would like to accept. Defaults to 10000 if left empty.
#' @return output data.frame: The data which you seek.
#' @examples 
#' queryVertnet("bison")
#' queryVertnet("(kansas state OR KSU)", limit = 200)
#' queryVertnet(genus = "mustela", species = "(nivalis OR erminea)")
#'
queryVertnet <- function(taxonname, genus = "", species = "", state = "", limit = ""){
  inputDF <- convertVertnettoDF(taxonname=taxonname, genus=genus, species=species, state=state, limit=limit)
  output <- getData(inputDF)
  return(output)
}

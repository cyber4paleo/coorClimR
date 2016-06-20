library(jsonlite)

getData <- function(x, y, t, producer="", model="", modelVersion="", variableType="", variableUnits="", variablePeriod="", variablePeriodType="",
                    averagingPeriod="", averagingPeriodType="", resolution=""){
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
  url = URLencode(url)
  ## execute the request to the API data service
  response <- fromJSON(url)
  if (response$success){
    return(response$data)
  }else{
    return(paste("Request failed.  Server says", response['message']))
  }
}

checkNumeric <- function(x) is.numeric(x) & !is.na(x)
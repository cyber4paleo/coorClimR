convertVertnettoDF <- function(taxonname, genus = "", species = "", state = "", limit = ""){

  # The default search has 1000 limit. However, if no limit is given, set to 100,000
  if (limit == ""){
    limit = 10
  }
  # API data request
  df <- vertsearch(taxonname, genus=genus, species=species, state=state, limit=limit, compact = TRUE, verbose = TRUE)

  # start picking apart the response and keeping the relevant data
  # lat, lon, siteID or some ID, year
  df <- apply(df$data, 1, function(x){
    l <- c(x["decimallongitude"], x["decimallatitude"], x["year"], x["occurrenceid"])
    return(l)
  })
  # rownames(df) <- c("Longitude", "Latitude", "Age", "ID")
  df <- data.frame(df)
  df <- na.omit(df)
  df <- unique(df)
  return(df)
}

queryVertnet <- function(taxonname, genus = "", species = "", state = "", limit = ""){
  inputDF <- convertVertnettoDF(taxonname=taxonname, genus=genus, species=species, state=state, limit=limit)
  # output <- getData(inputDF)
  print(class(inputDF))
  return(inputDF)


# super function of getData that will compile a data frame of multiple calls
v1 <- c(37,-122,1000)
v2 <- c(36,-121,1000)
# rn <- c("Longitude", "Latitude", "Year BP")
df <- rbind(v1, v2)
total.df <- data.frame()

get.all.data <- function(df_in){

  df_out <- ddply(df_in, 1, function(x){
    lat <- x[1]
    lon <- x[2]
    yearbp <- x[3]
    fetch.data <- getData(lon, lat, yearbp)
    df_out <- data.frame(lat,lon,yearbp, fetch.data)
    return(df_out)
  })
  return(df_out)
}

total.df <- get.all.data(df)
unlist(total.df)

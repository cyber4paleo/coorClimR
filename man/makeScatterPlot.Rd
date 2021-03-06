% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coorClimR.R
\name{makeScatterPlot}
\alias{makeScatterPlot}
\title{Scatter two environmental variables against each other}
\usage{
makeScatterPlot(climateDF, xVariable = "Precipitation",
  yVariable = "Maximum Temperature", xPeriod = 1, yPeriod = 1,
  pointColor = "gray", plotModern = TRUE, modernColor = "black",
  title = paste(xVariable, xPeriod, "vs.\\n", yVariable, yPeriod))
}
\arguments{
\item{climateDF}{A data.frame object produced by getData that has the climate data you wish to plot}

\item{xVariable}{Name of variable type to put on the x axis}

\item{yVariable}{Name of variable type to put on the y axis}

\item{xPeriod}{The measurement period of the variable for the X axis of the plot}

\item{yPeriod}{The measurement period of the variable for the T axis of the plot}

\item{pointColor}{Color string in which to plot the xy points}

\item{plotModern}{boolean flag indicating whether to plot the modern points in a different color than the background points}

\item{modernColor}{if plotModern is TRUE, then the modern points will be plotted in this color}

\item{title}{The title to give the plots}
}
\value{
VOID
}
\description{
Scatter two environmental variables against each other
}
\examples{
d <- queryNeotoma("ilex")
makeScatterPlot(d, xVariable='Maximum Temperature', yVariable='Minimum Temperature', xPeriod=7, yPeriod=1, modernColor='blue')
}


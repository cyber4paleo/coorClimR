# coorClimR
Geocoordinates-to-Climate in R

#### Authors
[Scott Farley](mailto:sfarley2@wisc.edu), University of Wisconsin, Madison

Chris Heiser

### Problem
Managing large climate datasets on personal computers is difficult, and is overwhelming for projects that require only small, point based analyses.  This type of analysis does not require the spatial nature of the grid-based GCM output.  However, to date, there are no tools that allow a user to directly query for values at a single point within a GCM output grid.  

### Solution
This project is an R-wrapper around an [API](http://github.com/scottsfarley93/niche-api) that stores gridded climate data and its metadata in a postgres database.  This r-package allows users to get climate data for a single point and return the results as a simple data frame, making visualization and analysis easy.  This package integrates with the [Neotoma R Package](https://github.com/ropensci/neotoma), the [PaleoBiology Database R Package](https://github.com/ropensci/neotoma), and the [VertNet R Package](https://github.com/ropensci/rvertnet) to support existing workflows.

### Example Use Case
A paleoecologist wants to plot *Quercus* pollen abundance against January minimum temperature to visually test hypotheses about oak niche stability since the last deglaciation.  Using the Neotoma R Wrapper, she can get the spatiotemporal coordinates (latitude, longitude, time) and relative pollen abundance for every occurrence in the Neotoma Paleoecological Database.  Using the coorClimR R package, she can query the niche database for the climate variables of her choice, in this case January minimum temperature.  The use of this package allows her to sidestep the downloading, management, and interpolation associated with manually doing this analysis.  

```
library(coorClimR)
quercus.climate <- queryNeotoma("quercus")
makeScatterPlot(quercus.climate, xVariable="Maximum Temperature", yVariable='Precipitation")
```

### API:
1.  getData: Get climate data for a point or set of points
2.  getDataRow: Search the database for climate at a space-time location
3.  queryNeotoma: Query the Neotoma Paleoecological Database and get climate data formfossil occurrences
4.  queryVertnet: Query the Vertnet database and get climate data at vertebrate localities
5.  queryAll: Query all supported databases for a taxon and get climate data at the x-y-t coordinates
6.  makeTSPlot: Plot a climate variable vs. time
7.  makeScatterPlot: Plot two climate variables against each other


Full documentation is is the coorClimR.pdf reference manual.



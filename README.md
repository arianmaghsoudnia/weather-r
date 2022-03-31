A wrapper around RNCEP package to retreive data for a specific location.
Inputs for retreiving data are the coordinates and the date range of the query.
The queried parameters are the temperature, relative humidity, and solar radiation.
Please note that the RNCEP package uses a spatial and temporal griding system for the datapoint. This script, if necessary does the interpolations and reports and saves all retreived measurements in a joined cleaned dataframe.
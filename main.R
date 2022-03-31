#####BE VERY CAREFUL WITH THE SIZE OF THE DATAFRAMES EACH OF THE FUNCTIONS
##### OF RNCEP RETURN. FOR EXAMPLE FOR TEMPERATURE, THE LATITUDES 
##### LONGTITUDES IN WHICH THE VARIABLE IS REPORTED IS DIFFERENT 
##### FROM THE RADIATION, SO IT IS ADVISABLE IF THE USER FIRST TEST
##### IT MANUALLY BECAUSE MERGING THE DATAFRAMES OF DIFFERENT NUMBER
##### OF ROWS, AND THEREFORE ERROR MIGHT OCCUER. THE INTERPOLATION
##### FUNCTION TAKES FOUR POINTS- TWO FOR LATITUDES AND TWO FOR
##### LONGTITUDES- FOR EACH TIMESTEP. SO MAKE SURE EACH ONE OF
##### THE CALLED RNCEP DATAFRAMES HAVE 4 VALUES OF LATITUDE AND
##### LONGTITUDE FOR EACH TIMESTEP. IN THE NEXT VERSIONS THIS
##### WILL BE AUTOMATICALLY CHECKED. 

### clear workspace
#rm(list = ls(all.names = TRUE))

#Necessary package installation
if (!require("rio")){ 
  install.packages("rio")
  install_formats()}
if (!require("RNCEP")) {install.packages("RNCEP")}





### Setting inputs
months <- c(1,2)
years <- c(2022)

exact_lat <- <latitude>
exact_lon <- <longitude>

latitude <- c(exact_lat-0.1,exact_lat+0.1)
longitude <- c(exact_lon-0.1,exact_lon+0.1)

# The function retrieves data from the RNCEP datasets and returns a
retrieve_data <- function(latitude,longitude,months,years)
{
  ## Retrieve the temperature from a particular pressure level for
  ## a specified spatial and temporal extent
  t <- NCEP.gather(variable='air.sig995', level='surface',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longitude,
                            reanalysis2 = FALSE, return.units = TRUE)

  hum <- NCEP.gather(variable='rhum.sig995', level='surface',
                              months.minmax=months, years.minmax=years,
                              lat.southnorth=latitude, lon.westeast=longitude,
                              reanalysis2 = FALSE, return.units = TRUE)

  #‘dswrf.sfc’ Downward solar radiation flux (At Surface) W/m^2
  rad <- NCEP.gather(variable='dswrf.sfc', level='gaussian',
                              months.minmax=months, years.minmax=years,
                              lat.southnorth=latitude, lon.westeast=longitude,
                              reanalysis2 = FALSE, return.units = TRUE)
  #lhtfl.sfc’ Latent heat net flux (At Surface) W/m^2
  latent <- NCEP.gather(variable='lhtfl.sfc', level='gaussian',
                                 months.minmax=months, years.minmax=years,
                                 lat.southnorth=latitude, lon.westeast=longitude,
                                 reanalysis2 = FALSE, return.units = TRUE)
  #‘nswrs.sfc’ net shortwave radiation (At Surface) W/m^2
  # nswrs <- NCEP.gather(variable='nswrs.sfc', level='gaussian',
  #                               months.minmax=months, years.minmax=years,
  #                               lat.southnorth=latitude, lon.westeast=longitude,
  #                               reanalysis2 = FALSE, return.units = TRUE)
  # #‘uswrf.sfc’ Upward solar radiation flux (At Surface) W/m^2
  # usrf <- NCEP.gather(variable='uswrf.sfc', level='gaussian',
  #                              months.minmax=months, years.minmax=years,
  #                              lat.southnorth=latitude, lon.westeast=longitude,
  #                              reanalysis2 = FALSE, return.units = TRUE)
  df_t <- NCEP.array2df(t[,,])
  df_hum <- NCEP.array2df(hum[,,])
  df_rad <- NCEP.array2df(rad[,,])
  df_lat <- NCEP.array2df(latent[,,])
  #df_nswrs <- NCEP.array2df(nswrs[,,])
  #df_usrf <- NCEP.array2df(usrf[,,])

  names(df_t)[names(df_t) == "variable1"] <- "temperature"
  names(df_t)[names(df_t) == "latitude"] <- "lat_temp_hum"   #=latitude_temperature_humidity
  names(df_t)[names(df_t) == "longitude"] <- "lon_temp_hum"
  names(df_hum)[names(df_hum) == "variable1"] <- "RH"
  #set2 of data; different latitude and longtitude set but same datetime
  names(df_rad)[names(df_rad) == "latitude"] <- "lat_set_2"
  names(df_rad)[names(df_rad) == "longitude"] <- "lon_set_2"
  names(df_rad)[names(df_rad) == "variable1"] <- "rad"
  names(df_lat)[names(df_lat) == "variable1"] <- "latent"
  #names(df_nswrs)[names(df_nswrs) == "variable1"] <- "short rad"
  #names(df_usrf)[names(df_usrf) == "variable1"] <- "upward rad"
  df_merge <- df_t
  df_merge$RH <- df_hum[,"RH"]
  df_merge$lat_set_2 <- df_rad[,"lat_set_2"]
  df_merge$lon_set_2 <- df_rad[,"lon_set_2"]
  df_merge$rad <- df_rad[,"rad"]
  df_merge$latent <- df_lat[,"latent"]
  #df_merge$short_rad <- df_nswrs[,"short rad"]
  #df_merge$upward_rad <- df_usrf[,"upward rad"]

  ##If you need to visualize the dataframes on a weather map
  ### NCEP.vis.area(t, layer=1, show.pts=TRUE, draw.contours=TRUE,
  ###     cols=heat.colors(64), transparency=.5, axis.args=NULL, map.args=NULL,
  ###               grid.args=NULL, title.args=NULL, interp.loess.args=NULL,
  ###               image.plot.args=NULL, contour.args=NULL, points.args=NULL)

  return(df_merge)
}


##Function to drop unwanted columns
df_drop_col <- function(df,col2drp)
{
  df <- df[, !(names(df) %in% col2drp)]
  return(df)
}

#Function to drop unwanted row
df_drop_row <- function(df,row2drp)
{
  df <- df[-row2drp,]
  return(df)
}

##Function to replace "_" with "-" in the datetime column retrieved from
##RNCEP package to make it more like the standrad R date format

rename_std <- function(chr)
{
  chr <- gsub("_","-",chr)
  return(chr)
}

##As we are dealing with large dataframes, these simple functions helps us to
##print faster a useful brief first and last portion of any large dataframe
hd <- function(df)
{
  return(head(df,10))
}

tl <- function(df)
{
  return(tail(df,10))
}

##Function to change the datetime format of the df and separate it
##to a standard date and a time row
std_date_time <- function(df)
{
  df[ ,"datetime"] <- apply(as.array(df[ ,"datetime"]),1,rename_std)
  df$date<- substr(df$datetime,1,10)
  df$date<- as.Date(df$date)
  df$time<- substr(df$datetime,nchar(df$datetime)-1,nchar(df$datetime))
  df$time<- as.integer(df$time)
  return(df)
}

## The function gets a dataframe and shifts the colummn "col2shift"
## by "shiftval" rows
shift_col <- function(df,col2shift,shift_val)
{
  df_shifted<-df
  seq_df=as.integer(seq(1,nrow(df),1))
  counter<-0
  for(i in seq_df)
  {
    df_shifted[[col2shift]][i] <- df[[col2shift]][i+shift_val]
    counter<-counter+1
  }
  return(df_shifted)
}

interpolate <- function(df,feature)
{
  if(feature=="temperature"| feature=="RH")
  {
    std_lat_name <- "lat_temp_hum"
    std_lon_name <- "lon_temp_hum"
  }
  else
  {
    std_lat_name <- "lat_set_2"
    std_lon_name <- "lon_set_2"
  }
  ###Shift the longitude by one to compare and see if we are in same set 
  df_shifted_lon <- shift_col(df,std_lon_name,1)
  df$shifted_lon <- df_shifted_lon[[std_lon_name]]
  ###Checks if two executive rows belong to the same longitude
  df$same_lon <- df[[std_lon_name]] == df$shifted_lon
  ###Dividing the df which is labeled as True or False; true if the longitude 
  ###is the same as the next row; eventually diving the df to two dataframes each
  ### one for one set of latitude numbers. True/False sequence does not matter.
  df_false <- df[df$same_lon==TRUE, ]
  df_true <- df[df$same_lon==FALSE, ]
  ### To substract two executive temperatures for interpolation function 
  df_false$shifted_feature <-  (shift_col(df_false,feature,1))[[feature]]
  df_true$shifted_feature <-  (shift_col(df_true,feature,1))[[feature]]
  
  df_false$shifted_time <-  (shift_col(df_false,"time",1))$time
  df_true$shifted_time <-  (shift_col(df_true,"time",1))$time
  
  df_false$time_change_coeff <- ((df_false$time-df_false$shifted_time))
  df_true$time_change_coeff <- ((df_true$time-df_true$shifted_time))
  
  lon_low <- df_false[1,3]
  lon_high <- df_false[2,3]
  cte_long <- (exact_lon-lon_low)/(lon_high-lon_low)
  df_false$feature_interp_lon <- (cte_long* df_false$shifted_feature + (1-cte_long)*df_false[[feature]])
  df_true$feature_interp_lon <- (cte_long*df_true$shifted_feature + (1-cte_long)*df_true[[feature]])
  
  df_false_red <- df_false[df_false$time_change_coeff==0, ]
  df_true_red <- df_true[df_true$time_change_coeff==0, ]
  
  #todrop <- c("shifted_lat","shifted_lon","same_lat","shifted_t","shifted_time","time_change_coeff")
  todrop <- NULL
  df_false_red <- df_drop_col(df_false_red,todrop)
  df_true_red <- df_drop_col(df_true_red,todrop)
  
  df_joined <- df_false_red
  ###levelize df_true and df_false to combine
  df_true_red[nrow(df_false_red),] <- NA
  ### Now we can interpolate btw the latitudes
  lat_false <- df_false[1,2]
  lat_true <- df_true[1,2]
  lat_step <- lat_false-lat_true
  cte_lat <- (lat_false-exact_lat)/lat_step
  df_joined$feature_interp <- cte_lat*df_true_red$feature_interp_lon+(1-cte_lat)*df_false_red$feature_interp_lon
  
  df_clean <- df_joined
  clean_drop <- c("datetime","latitude","temperature","t_interp_lon","exact_lon","same_lon","shifted_feature","feature_interp_lon")
  df_clean <- df_drop_col(df_clean,clean_drop)
  return(df_clean)
}


df_retrieved_raw <- retrieve_data(latitude,longitude,months,years)

mk_final_clean <- function(df)
{
  df_final_temp <- std_date_time(df_retrieved_raw)
  df_final <- interpolate(df_final_temp,"temperature")
  names(df_final)[names(df_final) == "feature_interp"] <- "temperature_int"
  df_final$RH_int <- interpolate(df_final_temp,"RH")$feature_interp
  #df_final$rad_int <- interpolate(df_final_temp,"rad")$feature_interp
  #df_final$latent_int <- interpolate(df_final_temp,"latent")$feature_interp
  #df_final$short_rad_int <- interpolate(df_final_temp,"short_rad")$feature_interp
  #df_final$upward_rad_int <- interpolate(df_final_temp,"upward_rad")$feature_interp
  clean_drop <- c("lat_temp_hum","lon_temp_hum","RH","lat_set_2","lon_set_2","rad","latent","short_rad","upward_rad")
  df_final <- df_drop_col(df_final,clean_drop)
  return(df_final)
}
df_final = mk_final_clean(df_retrieved_raw)
df_temperature =  interpolate(df_final_temp,"temperature")
export(df_final,'data_export_2022.csv')
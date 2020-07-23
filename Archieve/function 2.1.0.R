rm(list = ls(all.names = TRUE))

if (!require("rio")) install.packages("rio")
install_formats()
if (!require("RNCEP")) install.packages("RNCEP") 

library("RNCEP")
library("rio")

base_dir <- getwd()
directory<- "/media/arian/Linux Files/Aries/Weather"

mk_path <- function(dir,file_name,file_type)
{
  full_name = paste0(file_name,".",file_type)
  path <- file.path(dir,full_name,fsep="/") 
  return(path)
}
fileName='helloworld'
fileType='csv'
mk_path(directory,fileName,fileType)

months <- c(1,2)
years <- c(2018,2018)
lat_0 <- 45
lat_1 <- 47.5
lon_0 <- 7.5
lon_1 <- 10

exact_lat <- 45.46
exact_lon <- 9.19
latitude <- c(exact_lat-0.1,exact_lat+0.1)
longitude <- c(exact_lon-0.1,exact_lon+0.1)
time_step <-6


## Retrieve the temperature from a particular pressure level for
## a specified spatial and temporal extent
north_it_t <- NCEP.gather(variable='air.sig995', level='surface',
                         months.minmax=months, years.minmax=years,
                         lat.southnorth=latitude, lon.westeast=longitude,
                         reanalysis2 = FALSE, return.units = TRUE)

north_it_hum <- NCEP.gather(variable='rhum.sig995', level='surface',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longitude,
                            reanalysis2 = FALSE, return.units = TRUE)

#‘dswrf.sfc’ Downward solar radiation flux (At Surface) W/m^2
north_it_rad <- NCEP.gather(variable='dswrf.sfc', level='gaussian',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longitude,
                            reanalysis2 = FALSE, return.units = TRUE)
#lhtfl.sfc’ Latent heat net flux (At Surface) W/m^2
north_it_latent <- NCEP.gather(variable='lhtfl.sfc', level='gaussian',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longitude,
                            reanalysis2 = FALSE, return.units = TRUE)
#‘nswrs.sfc’ net shortwave radiation (At Surface) W/m^2
north_it_nswrs <- NCEP.gather(variable='nswrs.sfc', level='gaussian',
                               months.minmax=months, years.minmax=years,
                               lat.southnorth=latitude, lon.westeast=longitude,
                               reanalysis2 = FALSE, return.units = TRUE)
#‘uswrf.sfc’ Upward solar radiation flux (At Surface) W/m^2
north_it_usrf <- NCEP.gather(variable='uswrf.sfc', level='gaussian',
                               months.minmax=months, years.minmax=years,
                               lat.southnorth=latitude, lon.westeast=longitude,
                               reanalysis2 = FALSE, return.units = TRUE)
north_it_df_t <- NCEP.array2df(north_it_t[,,])
north_italy_df_hum <- NCEP.array2df(north_it_hum[,,])
north_italy_df_rad <- NCEP.array2df(north_it_rad[,,])
north_italy_df_lat <- NCEP.array2df(north_it_latent[,,])
north_italy_df_nswrs <- NCEP.array2df(north_it_nswrs[,,])
north_italy_df_usrf <- NCEP.array2df(north_it_usrf[,,])

names(north_it_df_t)[names(north_it_df_t) == "variable1"] <- "temperature"
names(north_italy_df_hum)[names(north_italy_df_hum) == "variable1"] <- "RH"
names(north_italy_df_rad)[names(north_italy_df_rad) == "variable1"] <- "rad"
names(north_italy_df_lat)[names(north_italy_df_lat) == "variable1"] <- "latent"
names(north_italy_df_nswrs)[names(north_italy_df_nswrs) == "variable1"] <- "short rad"
names(north_italy_df_usrf)[names(north_italy_df_usrf) == "variable1"] <- "upward rad"

#df_t_hum <- cbind(north_it_df_t,north_italy_df_hum,deparse.level = 0)
df_t_hum <- north_it_df_t
df_t_hum$hum <- north_italy_df_hum[,"RH"]
df<-df_t_hum
df_2<- north_italy_df_rad
df_2$latent <- north_italy_df_lat[,"latent"]
df_2$short_rad <- north_italy_df_nswrs[,"short rad"]
df_2$upward_rad <- north_italy_df_usrf[,"upward rad"]
head(df_2,10)
#df_koskalak <- as.data.frame(north_it_t[,"latitude"],drop=FALSE)
# NCEP.vis.area(north_it_t, layer=1, show.pts=TRUE, draw.contours=TRUE,
#               cols=heat.colors(64), transparency=.5, axis.args=NULL, map.args=NULL,
#               grid.args=NULL, title.args=NULL, interp.loess.args=NULL,
#               image.plot.args=NULL, contour.args=NULL, points.args=NULL)

##Function to drop unwanted columns
df_drop_col <- function(df,col2drp)
{
  df <- df[, !(names(df) %in% col2drp)]
  return(df)
}

##Function to drop unwanted row
# df_drop_col <- function(df,row2drp)
# {
#   df <- df[-row2drp,]
#   return(df)
# }

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
  ###Shift the longitude by one two compare and see if we are 
  df_shifted_lon <- shift_col(df,"longitude",1)
  df$shifted_lon <- df_shifted_lon$longitude
  ###Checks if two executive rows belong to the same longitude
  df$same_lon <- df$longitude == df$shifted_lon
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
  
  
  cte_long <- (exact_lon-lon_0)/(lon_1-lon_0)
  df_false$feature_interp_lon <- (cte_long* df_false$shifted_feature + (1-cte_long)*df_false[[feature]])
  df_true$feature_interp_lon <- (cte_long*df_true$shifted_feature + (1-cte_long)*df_true[[feature]])
  
  df_false$exact_lon <- exact_lon
  df_true$exact_lon <- exact_lon
  
  df_false_red <- df_false[df_false$time_change_coeff==0, ]
  df_true_red <- df_true[df_true$time_change_coeff==0, ]
  
  todrop <- c("shifted_lat","shifted_lon","same_lat","shifted_t","shifted_time","time_change_coeff","longitude")
  
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
  clean_drop <- c("datetime","latitude","temperature","t_interp_lon","exact_lon")
  df_clean <- df_drop_col(df_clean,clean_drop)
  return(df_true)
}

df_temp <- std_date_time(north_it_df_t)
df_clean<-df_temp
df_clean <- interpolate(df_temp,"temperature")

df_temp <- std_date_time(north_italy_df_rad)
df_clean<-df_temp
df_clean <- interpolate(df_temp,"rad")
hd(df_clean)
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
latitude <- c(45,46)
longtitude <- c(9,10)
## Retrieve the temperature from a particular pressure level for
## a specified spatial and temporal extent
north_it_t <- NCEP.gather(variable='air.sig995', level='surface',
                         months.minmax=months, years.minmax=years,
                         lat.southnorth=latitude, lon.westeast=longtitude,
                         reanalysis2 = FALSE, return.units = TRUE)

north_it_hum <- NCEP.gather(variable='rhum.sig995', level='surface',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longtitude,
                            reanalysis2 = FALSE, return.units = TRUE)

#‘dswrf.sfc’ Downward solar radiation flux (At Surface) W/m^2
north_it_rad <- NCEP.gather(variable='dswrf.sfc', level='gaussian',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longtitude,
                            reanalysis2 = FALSE, return.units = TRUE)
#lhtfl.sfc’ Latent heat net flux (At Surface) W/m^2
north_it_latent <- NCEP.gather(variable='lhtfl.sfc', level='gaussian',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longtitude,
                            reanalysis2 = FALSE, return.units = TRUE)
#‘nswrs.sfc’ net shortwave radiation (At Surface) W/m^2
north_it_nswrs <- NCEP.gather(variable='nswrs.sfc', level='gaussian',
                               months.minmax=months, years.minmax=years,
                               lat.southnorth=latitude, lon.westeast=longtitude,
                               reanalysis2 = FALSE, return.units = TRUE)
#‘uswrf.sfc’ Upward solar radiation flux (At Surface) W/m^2
north_it_usrf <- NCEP.gather(variable='uswrf.sfc', level='gaussian',
                               months.minmax=months, years.minmax=years,
                               lat.southnorth=latitude, lon.westeast=longtitude,
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
df_drop_col <- function(df,col2drp)
{
  df <- df[, !(names(df) %in% col2drp)]
  return(df)
}
std_date_time <- function(chr)
{
  chr <- gsub("_","-",chr)
  return(chr)
}

hd <- function(df)
{
  return(head(df,10))
}

apply(north_it_df_t[],1,std_date_time)
north_it_df_t[ ,"datetime"] <- apply(as.array(north_it_df_t[ ,"datetime"]),1,std_date_time)

std_date_time(north_it_df_t)
df_temp <- north_it_df_t
df_temp$date<- substr(df_temp$datetime,1,10)
df_temp$date<- as.Date(df_temp$date)
df_temp$time<- substr(df_temp$datetime,nchar(df_temp$datetime)-1,nchar(df_temp$datetime))
df_temp$date<- as.Date(df_temp$date)
typeof(df_temp[1,5])
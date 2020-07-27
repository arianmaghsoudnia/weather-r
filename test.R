### clear workspace
rm(list = ls(all.names = TRUE))

months <- c(1,2)
years <- c(2018,2018)

exact_lat <- 34.78
exact_lon <- 254

latitude <- c(exact_lat-0.1,exact_lat+0.1)
longitude <- c(exact_lon-0.1,exact_lon+0.1)


north_it_t <- NCEP.gather(variable='air.sig995', level='surface',
                          months.minmax=months, years.minmax=years,
                          lat.southnorth=latitude, lon.westeast=longitude,
                          reanalysis2 = FALSE, return.units = TRUE)

north_it_hum <- NCEP.gather(variable='rhum.sig995', level='surface',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longitude,
                            reanalysis2 = FALSE, return.units = TRUE)


north_it_rad <- NCEP.gather(variable='dswrf.sfc', level='gaussian',
                            months.minmax=months, years.minmax=years,
                            lat.southnorth=latitude, lon.westeast=longitude,
                            reanalysis2 = FALSE, return.units = TRUE)

north_it_latent <- NCEP.gather(variable='lhtfl.sfc', level='gaussian',
                               months.minmax=months, years.minmax=years,
                               lat.southnorth=latitude, lon.westeast=longitude,
                               reanalysis2 = FALSE, return.units = TRUE)

north_it_nswrs <- NCEP.gather(variable='nswrs.sfc', level='gaussian',
                              months.minmax=months, years.minmax=years,
                              lat.southnorth=latitude, lon.westeast=longitude,
                              reanalysis2 = FALSE, return.units = TRUE)

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
names(north_it_df_t)[names(north_it_df_t) == "latitude"] <- "lat_temp_hum"   #=latitude_temperature_humidity
names(north_it_df_t)[names(north_it_df_t) == "longitude"] <- "lon_temp_hum"
names(north_italy_df_hum)[names(north_italy_df_hum) == "variable1"] <- "RH"

df_merge <- north_it_df_t
df_merge$RH <- north_italy_df_hum[,"RH"]

#set2 of data; different latitude and longtitude set but same datetime
names(north_italy_df_rad)[names(north_italy_df_rad) == "latitude"] <- "lat_set_2"
names(north_italy_df_rad)[names(north_italy_df_rad) == "longitude"] <- "lon_set_2"
names(north_italy_df_rad)[names(north_italy_df_rad) == "variable1"] <- "rad"
names(north_italy_df_lat)[names(north_italy_df_lat) == "variable1"] <- "latent"
names(north_italy_df_nswrs)[names(north_italy_df_nswrs) == "variable1"] <- "short rad"
names(north_italy_df_usrf)[names(north_italy_df_usrf) == "variable1"] <- "upward rad"

df_merge$lat_set_2 <- north_italy_df_rad[,"lat_set_2"]
df_merge$lon_set_2 <- north_italy_df_rad[,"lon_set_2"]
df_merge$rad <- north_italy_df_rad[,"rad"]
df_merge$latent <- north_italy_df_lat[,"latent"]
df_merge$short_rad <- north_italy_df_nswrs[,"short rad"]
df_merge$upward_rad <- north_italy_df_usrf[,"upward rad"]

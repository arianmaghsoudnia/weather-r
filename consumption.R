
library(rio)
#source('weather_function.R')
source('tools.R')

df<- import("6.csv")
names(df)[names(df) == "value"] <- "consumption"
names(df)[names(df) == "datatime"] <- "datetime"

df$hour<- substr(df$datetime,nchar(df$datetime)-4,nchar(df$datetime)-3)

detect_time_

avg1h <- function (df,range,cons)
{
  seq_df=as.integer(seq(1,nrow(df),1))
  counter<-0
  for(i in seq_df)
  {
   if(i %% 12 != 0)
   {
     cons <- 
   }
  }
}


df$datetime <- as.Date(df$datatime)
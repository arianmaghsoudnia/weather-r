#Necessary package installation
pkg_install <- function()
{
  if (!require("rio")) install.packages("rio")
  install_formats()
  if (!require("RNCEP")) install.packages("RNCEP")
  
  library("RNCEP")
  library("rio")
  
}
pkg_install()

### setting directory
base_dir <- getwd()
directory<- "/media/arian/Linux Files/Aries/Weather"

mk_path <- function(dir,file_name,file_type)
{
  full_name = paste0(file_name,".",file_type)
  path <- file.path(dir,full_name,fsep="/")
  return(path)
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
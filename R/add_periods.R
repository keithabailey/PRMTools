#' Add new fiscal information fields to a dataframe
#'
#' This function will return a dataframe with fiscal period information added. It will only accept valid date fields (date, POSIXct, POSIXlt)
#' @param df dataframe you want add information to
#' @param date_col field on which all calculations will be made
#' @export
#' @examples
#' add_periods(myydataframe, "createddate_col")
#' will return mydataframe with the following new columns:
#' createddate_col_period
#' createddate_col_qtr
#' createddate_col_year
#' createddate_col_year_qtr
#' createddate_col_month_end


#add various periods we are interestd.
add_periods<-function(df, date_col, include_rolling = TRUE) {

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }
  library(lubridate)

  date_col_name<-date_col
  date_col<-paste("^",date_col,"$",sep="")
  
  df$temp_fixed_date<-as.Date(df[,grep(date_col, colnames(df))])
  df$temp_fixed_date[is.na(df$temp_fixed_date)]<-as.Date("1900-01-01")
  
  #normalise date to fiscal date
  df$temp_month_end<-as.Date(ceiling_date(as.Date(df[,grep(date_col, colnames(df))]), "month") - days(1))
  df$temp_month_end[is.na(df$temp_month_end)]<-as.Date("1900-01-01")

  df$temp_period<-as.numeric(year(df$temp_month_end)*100 + month((df$temp_month_end) %m+% months(5)))
  df$temp_qtr<-quarter(df$temp_month_end %m+% months(5))
  df$temp_year<-year(df$temp_month_end %m+% months(5))
  df$temp_year_qtr<-paste(df$temp_year," Q",df$temp_qtr,sep="")
  

  names(df)[names(df) == "temp_period"]<-paste(date_col_name,"_period",sep="")
  names(df)[names(df) == "temp_qtr"]<-paste(date_col_name,"_qtr",sep="")
  names(df)[names(df) == "temp_year"]<-paste(date_col_name,"_year",sep="")
  names(df)[names(df) == "temp_year_qtr"]<-paste(date_col_name,"_year_qtr",sep="")
  names(df)[names(df) == "temp_month_end"]<-paste(date_col_name,"_month_end",sep="")
  
  #rolling dates
  if (include_rolling) {
    df$temp_30_rolling<-as.numeric((difftime(Sys.Date(),
                                             df$temp_fixed_date,units=c("days"))))%/%30*-30
    df$temp_30_rolling<-Sys.Date() + df$temp_30_rolling
    
    df$temp_90_rolling<-as.numeric((difftime(Sys.Date(),
                                             df$temp_fixed_date,units=c("days"))))%/%90*-90
    df$temp_90_rolling<-Sys.Date() + df$temp_90_rolling
    
    df$temp_9m_rolling<-as.numeric((difftime(Sys.Date(),
                                             df$temp_fixed_date,units=c("days"))))%/%270*-270
    df$temp_9m_rolling<-Sys.Date() + df$temp_9m_rolling
    
    names(df)[names(df) == "temp_30_rolling"]<-paste(date_col_name,"_30_rolling",sep="")
    names(df)[names(df) == "temp_90_rolling"]<-paste(date_col_name,"_90_rolling",sep="")
    names(df)[names(df) == "temp_9m_rolling"]<-paste(date_col_name,"_9m_rolling",sep="")
  }
  
  #drop temp_fixed_date
  df <- subset( df, select = - temp_fixed_date)

  return(df)

}

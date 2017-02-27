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
add_periods<-function(df, date_col) {

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }
  library(lubridate)

  date_col_name<-date_col
  date_col<-paste("^",date_col,"$",sep="")

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

  return(df)

}

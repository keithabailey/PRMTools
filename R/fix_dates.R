#' Fix a character field to date
#'
#' This function will return a dataframe with all
#' @param df dataframe you want add information to
#' @param format formt that date is currently in so we can parse it
#' @param date_search character string to search for within the names of each column; Any that match will be changed to a date format
#' @export
#' @examples
#' fix_dates(myydataframe)

fix_dates<-function(df, format="%Y-%m-%d %H:%M:%S", date_search="date"){
  df[,grep(date_search,
           colnames(df))] <- lapply(df[,grep("date",
                                             colnames(df))],
                                    fast_strptime,
                                    format = format,
                                    lt=F)
  return(df)
}

#' @title Depure extreme values using 3 Sd +- Mean
#' @description This function delete values located in the extreme of normal distribution using two thresholds
#' @param dataframe dataframe with all data
#' @param variable name of variable of interest, e.g: yield
#' @return dataframe with variable selected depured
#' @export dataframe_cleaned
#'
clean_outliers <-  function(dataframe, variable) {
  dataframe[,variable] <- na.omit(as.numeric(dataframe[,variable]))

  Media <- mean(na.omit(dataframe[,variable]))
  DE <- sd(na.omit(dataframe[,variable]))
  LI <- as.double(Media-(2*DE))

  LI <- ifelse(LI<0,0,LI)
  LS <- as.double(Media+(2*DE))
  dataframe_cleaned <- na.exclude(subset(dataframe,
                                         dataframe[,variable] > LI & dataframe[,variable] < LS))
  return(dataframe_cleaned)
}

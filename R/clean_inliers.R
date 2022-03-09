#' @title Depure values without spatial autocorrelation
#' @description This function delete values with lack of spatial autocorrelation
#'     measured through local moran index
#' @param dataframe dataframe with all data
#' @param variable name of variable of interest, e.g: yield
#' @return dataframe with variable selected depured
#' @export dataframe_cleaned
#'
clean_inliers <- function(dataframe, coords, variable){
  coords <- coordinates(coords)
  gri <- dnearneigh(coords,0,30)
  lw <- nb2listw(gri, style = "W",zero.policy=TRUE)
  ML <- spdep::localmoran(x = dataframe[,variable],
                          listw = lw,  alternative ="less")
  dataframe <- data.frame(dataframe, ML)
  dataframe <- subset(dataframe, dataframe$Ii > 0 | dataframe$Pr.z...E.Ii.. >0.05)
  return(dataframe)
}

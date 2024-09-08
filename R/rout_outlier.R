#' Does the ROUT outlier test defined in PRISM.
#'
#' Source: Motulsky HM and Brown RE, Detecting outliers when fitting data with nonlinear regression – a new method based on robust nonlinear regression and the false discovery rate, BMC Bioinformatics 2006, 7:123.
#'
#' @param df The data frame where outliers need to be removed
#' @param x A string. The column which contains the groups you want to compare; the independent variable.
#' @param y A string. The column that contains the measured value; the dependent variable.
#' @param Q Controls the false discovery rate \(FDR\). Ex. if Q=0.01, then at least 99% of the identified outliers would be actual outliers. Higher Q means that the threshold for defining outliers is less strict.
#' @param outliers Returns the whole dataframe with outliers defined if TRUE. Removes the outliers from the dataframe if FALSE.



rout_outlier<-function(df, x, y, Q=0.01, outliers=FALSE){
  #calculates absolute value of the residuals (distances from each point from the curve)
  #lm is the linear fitting of the data
  df$res <- abs(lm(formula=df[[y]]~df[[x]], data=df)$residuals)
  df <- df[order(df$res),]
  #n is the number of data points
  n <- length(df$res)
  #k is the number of parameters fit by nonlinear regression
  k<- 1
  #Calculates the robust standard deviation of the residuals
  #0.6827 (data that lies 1 SD from the mean for Gaussian Distribution)
  # We find the 68.27 percentile
  rsdr <- quantile(df$res,0.6827)[[1]] * (n/(n-k))
  df$outlier <- FALSE
  #
  for (i in round(0.7*n):n){
    a_i <- (Q * (n-(i-1)))/n
    t <- df$res[i]/rsdr
    #two-tailed p-value from the t distribution
    p <- 2 * pt(t, df=n-k, lower.tail = F)
    #test whether this p-value is less than a_i
    df$outlier[i]<-p<a_i
  }
  df$res <- NULL
  #If true, returns the data, or if false, returns the data that removes outliers
  if(outliers==TRUE){
    return(df)
  }
  else{
    return(subset(df,outlier==FALSE))
  }
}

#' Multiple linear regression value calculator
#'
#' Generates a multiple linear model and outputs associated values and statistics.
#' @param formula String in the format of: "y~x"
#' @param df, Dataframe
#' @keywords mlr, multiple linear regression, sum of squares, ssreg, sse, rss, sxx
#' @export
#' @examples
#' mlrValues()

mlrValues = function(formula,df){
  mod = lm(formula,data=df)
  mod_coefficients = summary(mod)$coefficients
  n = nrow(df)
  p = length(mod$coefficients)-1
  y = gsub("(.+)~(.+)","\\1",formula)

  ## associated
  ssreg = sum((mod$fitted.values - mean(df[[y]]))**2)
  msreg = ssreg/p
  rss = sum((df[[y]] - mod$fitted.values)**2)
  mse = rss/(n-p-1)
  sst = rss + ssreg

  associated_values = matrix(c(ssreg,rss,sst,p,n-p-1,n-1,ssreg/p,rss/(n-p-1),sst/(n-1)),nrow=3,ncol=3)
  rownames(associated_values) = c("Regression","Error","Total")
  colnames(associated_values) = c("sum of squares","degrees of freedom","mean squares")

  ## global F statistics
  fobs = msreg/mse
  fexp = qf(.95,p,n-p-1)
  globalP = 1-pf(fobs,p,n-p-1)
  b1 = as.numeric(formatC(mod_coefficients[2,1],4,format="f"))
  df1 = p
  df2 = n-p-1
  global_test_stats = c(fobs,fexp,df1,df2)

  names(global_test_stats) = c("Fobs","Fexp (0.95)", "First degrees of freedom", "Second degrees of freedom")

  ## R^2 and R^2 adj
  r2 = (ssreg/(n-2))/(sst/(n-1))
  r2adj = 1-((mse/(sst/n-1)))
  r2 = c(r2,r2adj)
  names(r2) = c("R^2","R^2adj")

  ## p_values
  global_p_values = 1-pf(fobs,p,n-p-1)
  names(global_p_values) = "pval"

  ## combined
  combined = list(associated_values,global_test_stats,r2,global_p_values)
  names(combined) = c("associated_values","global_F_statistics","Rsquared","global_p_values")
  return(combined)
}


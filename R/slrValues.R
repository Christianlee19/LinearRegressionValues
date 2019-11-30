#' Simple linear regression value calculator
#'
#' Generates a  simple linear model and outputs associated values and statistics.
#' @param y String response variable
#' @param x String explanatory variable
#' @param df Dataframe
#' @keywords slr, simple linear regression, sum of squares, ssreg, sse, rss, sxx
#' @export
#' @examples
#' slrValues()

slrValues = function(y,x,df){
  mod = lm(df[[y]]~df[[x]])
  mod_coefficients = summary(mod)$coefficients
  n = nrow(df)

  ## parameters
  b0 = as.numeric(formatC(mod_coefficients[1,1],4,format="f"))
  b1 = as.numeric(formatC(mod_coefficients[2,1],4,format="f"))

  parameters = c(b0,b1)
  names(parameters) = c("b0","b1")

  ## associated
  sxx = sum((df[[x]] - mean(df[[x]]))**2)
  sxy = sum((df[[x]] - mean(df[[x]]))*(df[[y]] - mean(df[[y]])))
  rss = sum((df[[y]] - mod$fitted.values)**2)
  mse = rss/(n-2)
  ssreg = sum((mod$fitted.values - mean(df[[y]]))**2)
  msreg = ssreg/1
  sst = rss + ssreg

  associated_values = c(sxx,sxy,ssreg,msreg,rss,mse,sst)
  names(associated_values) = c("sxx","sxy","ssreg","msreg","rss","mse","sst")

  ## variance
  var_b0 = (rss/(n-2))*((1/n)+(mean(df[[x]])**2/sxx))
  se_b0 = sqrt(var_b0)
  var_b1 = (rss/(n-2))/(sxx)
  se_b1 = sqrt(var_b1)

  variance = c(var_b0,se_b0,var_b1,se_b1)
  names(variance) = c("var_b0","se_b0","var_b1","se_b1")

  ## test stats
  t_b0 = b0/(se_b0)
  t_b1 = b1/(se_b1)
  f = msreg/mse

  test_stats = c(t_b0,t_b1,f)
  names(test_stats) = c("t_b0","t_b1","F")

  ## confidence intervals
  ci_b0 = paste0(formatC(b0 - qt(.975,n-2)*se_b0,4,format="f"),"-",formatC(b0 + qt(.975,n-2)*se_b0,4,format="f"))
  ci_b1 = paste0(formatC(b1 - qt(.975,n-2)*se_b1,4,format="f"),"-",formatC(b1 + qt(.975,n-2)*se_b1,4,format="f"))
  ci = c(ci_b0,ci_b1)
  names(ci) = c("ci_b0","ci_b1")

  ## p_values
  pval_b0 = 2*pt(t_b0,n-2,lower=F)
  pval_b1 = 2*pt(t_b1,n-2,lower=F)
  p_values = c(pval_b0,pval_b1)
  names(p_values) = c("pval_b0","pval_b1")

  ## combined
  combined = list(parameters,associated_values,variance,test_stats,ci,p_values)
  names(combined) = c("parameters","associated_values","variance","test_stats","confidence_intervals_95","p_values")
  return(combined)
}



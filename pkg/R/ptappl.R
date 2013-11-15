
dtappl = function(x, xmin, alpha, theta, log=FALSE) {
  if(!log) { 
    pdf = ((alpha-1)/x + 1/theta)*(x/xmin)^(alpha-1) *exp((xmin-x)/theta)
  } else {
    pdf  = log((alpha-1)/x + 1/theta) + 
      (alpha-1)*log(xmin) - 
      (alpha-1) *log(x) + 
      xmin/theta -
      x/theta
  }
  pdf
}


ptappl = function(q, xmin, alpha, theta, lower.tail=TRUE) {
  ##Page 904 of Kagan and Schoenberg, 2001
  cdf = 1 - (q/xmin)^(-alpha + 1)*exp((xmin-q)/theta)
  if(lower.tail)
    cdf
  else
    1 - cdf#(cdf - dplcon(q, xmin, alpha)) 
}

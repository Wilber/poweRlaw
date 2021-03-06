#' @rdname estimate_xmin
#' @export
get_KS_statistic = function(m, xmax=1e5) {
  if(is(m, "discrete_distribution")) {
    data_cdf = dist_data_all_cdf(m, xmax=xmax)
    fit_cdf = dist_all_cdf(m, xmax=xmax)
  } else {
    #data_cdf = dist_data_cdf(m, xmax=xmax)
    q = m$dat
    n = m$internal[["n"]]; N = length(q)
    q = q[(N-n+1):N]
    q = q[q <= xmax]
    fit_cdf = dist_cdf(m, q)
    
    data_cdf = ((0:(n-1))/n)[1:length(fit_cdf)]
    #cdf = cdf[1:min(n, xmax)]
  }
  gof = max(abs(data_cdf - fit_cdf))
  return(gof)
}


#' Estimating the lower bound (xmin)
#' 
#' When fitting heavy tailed distributions, sometimes it 
#' is necessary to estimate the lower threshold, xmin. The
#' lower bound is estimated by calculating the minimising the
#' Kolmogorov-Smirnoff statistic 
#' (as described in Clauset, Shalizi, Newman (2009)).
#' \describe{
#' \item{\code{get_KS_statistic}}{Calculates the KS statistic for a particular value of xmin.}
#' \item{\code{estimate_xmin}}{Estimates the optimal lower cutoff using a 
#' goodness-of-fit based approach. This function may issue \code{warnings} 
#' when fitting lognormal, Poisson or Exponential distributions. The 
#' warnings occur for large values of \code{xmin}. Essentially, we are discarding 
#' the bulk of the distribution and cannot calculate the tails to enough
#' accuracy.}
#' \item{\code{bootstrap}}{Estimates the unncertainity in the xmin and parameter values via bootstraping.}
#' \item{\code{bootstrap_p}}{Performs a bootstrapping hypothesis test to determine whether a power law
#' distribution is plausible. This function only available for power law distribution objects.}}
#' @param m A reference class object that contains the data.
#' @param pars default \code{NULL}. A vector of parameters used to 
#' optimise over. 
#' Otherwise, for each value of \code{xmin}, the mle will be used, i.e. \code{estimate_pars(m)}.
#' For small samples, the mle may be biased. 
#' @param xmins default \code{1e5}. A vector of possible values 
#' of xmin to explore. When a single value is passed, this represents
#' the maximum value to search, i.e. by default we search from
#' (1, 1e5). See details for further information.
#' @param xmax default \code{1e5}. The maximum x value calculated when working out the CDF. See details for further 
#' information.
#' @param threads number of concurrent threads used during the bootstrap.
#' @param no_of_sims number of bootstrap simulations. When \code{no_of_sims} is large, this can 
#' take a while to run.
#' @details When estimating \code{xmin} for discrete distributions, the search space when 
#' comparing the data-cdf (empirical cdf)
#' and the distribution_cdf runs from xmin to \code{max(x)}
#' where \code{x} is the data set. This \strong{can} often be 
#' computationally brutal. In particular, when bootstrapping
#' we generate random numbers from the power law distribution, 
#' which has a long tail. 
#' 
#' To speed up computations for discrete distributions, it is sensible to put an 
#' upper bound, i.e. \code{xmax} or explicitly give values of where to search, i.e. \code{xmin}.
#' When calculating the kolmogorov-smirnov, the CDF is caculate up to the
#' maximum value of the data. However, for some data sets and during bootstrapping, very large 
#' values can be generated. This may cause both speed and memory problems. 
#' 
#' Occassionally, bootstrapping can generate strange situations. For example, 
#' all values in the simulated data set less then the \code{xmin} value. In this case, 
#' the estimated KS statistic will be \code{Inf} and the parameter values, \code{NA}.
#' @importFrom parallel makeCluster parSapply 
#' @importFrom parallel clusterExport stopCluster
#' @note Adapted from Laurent Dubroca's code found at
#' \url{http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r}
#' @export
#' @examples
#' ###################################################
#' # Load the data set and create distribution object#
#' ###################################################
#' x = 1:10
#' m = displ$new(x)
#' 
#' ###################################################
#' # Estimate xmin and pars                          #
#' ###################################################
#' est = estimate_xmin(m)
#' m$setXmin(est)
#' 
#' ###################################################
#' # Bootstrap examples                              #    
#' ###################################################
#' \dontrun{
#' bootstrap(m, no_of_sims=1, threads=1)
#' bootstrap_p(m, no_of_sims=1, threads=1)
#' }
estimate_xmin = function (m, xmins = NULL, pars=NULL, xmax=1e5) {
  ## Flag. Go through a bunch of checks to test whether we 
  ## can estimate xmin 
  estimate = length(unique(m$dat)) > 1
  
  ## Make thread safe
  if(estimate) {
    m_cpy = m$getRefClass()$new(m$dat)
    m_cpy$pars = pars
    if(is.null(xmins)) {
      xmins = unique(m$dat)
#      space = space[space <= xmins]
#      xmins = space
    }
    xmins = xmins[xmins <= xmax]
  }
 
  ## Need to have at least no_pars + 1 data points
  ## to estimate parameters. 
  ## Find (largest - no_pars) data point and subset xmins
  if(estimate) {
    unique_dat = unique(m_cpy$dat)
    q_len = length(unique_dat)
    max_data_pt_needed = sort(unique_dat, 
                              partial=q_len-m_cpy$no_pars-1)[q_len-m_cpy$no_pars-1]
    xmins = xmins[xmins <= max_data_pt_needed]
    
    ## Initialise xmin scan
    nr = length(xmins)
  }
  
  ## Bootstrapping may generate strange data
  if(!estimate || nr < 1) {
    ## Insufficient data to estimate parameters
    dat = matrix(0, nrow=1, ncol=(1 + m$no_pars))
    dat[1, ] = c(Inf, rep(NA, m$no_pars))
    estimate = FALSE
  } else {
    dat = matrix(0, nrow=nr, ncol=(1 + m_cpy$no_pars))   
    est = estimate_pars(m_cpy)$pars
  }
  
  xm = 0L
  while(estimate && xm < nr)   {
    m_cpy$xmin = xmins[xm + 1L]
    if(is.null(pars)) m_cpy$mle(initialise=est)
    else m_cpy$pars = pars
    
    ## Doesn't work for lognormal - need par matrix    
    if(!is.null(pars)) {
      L = dist_ll(m_cpy)
      I = which.max(L)
      m_cpy$pars = m_cpy$pars[I]
    }
    gof = get_KS_statistic(m_cpy, xmax)
    dat[xm <- xm + 1L,] = c(gof, m_cpy$pars)
  }
  
  row = which.min(dat[,1L])
  ## Check for numerical instabilities
  ## Can happen in the tails of the LN
  if(!length(row)) {
    row = 1L
    dat[row,] = NA_real_
    xmins[row] = NA_real_
    warning("Unable to estimate xmin. This may be due to numerical instabilities. 
            For example, the parameter estimates are in the distribution tails.")
  }
  
  xmin = xmins[row]
  pars = dat[row, 2:ncol(dat)]
  
  l = list(KS=dat[row, 1L], xmin=xmin, pars=pars)
  class(l) = "estimate_xmin"
  return(l)
}


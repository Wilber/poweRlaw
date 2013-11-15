#' @rdname dist_pdf-methods
#' @aliases dist_pdf,displ-method
setMethod("dist_pdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            pdf = dpldis(q[q >= m$xmin], m$xmin, m$pars, TRUE)
            if(!log) pdf = exp(pdf)
            pdf
          }
)

#' @rdname dist_pdf-methods
#' @aliases dist_pdf,dislnorm-method
setMethod("dist_pdf",
          signature = signature(m="dislnorm"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
<<<<<<< HEAD
            
            l1 = plnorm(q-0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            l2 = plnorm(q+0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            
            pdf = l1 + log(1-exp(l2-l1)) - 
=======
            pdf = log(plnorm(q-0.5, pars[1], pars[2], lower.tail=FALSE) -
                        plnorm(q+0.5, pars[1], pars[2], lower.tail=FALSE)) - 
>>>>>>> Adding tapered power-law distribution
              plnorm(xmin-0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            
            if(!log) pdf = exp(pdf)
            pdf
            
          }
)


#' @rdname dist_pdf-methods
#' @aliases dist_pdf,dispois-method
setMethod("dist_pdf",
          signature = signature(m="dispois"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            pdf = dpois(q, pars, log=TRUE) - ppois(xmin, pars, lower.tail=FALSE, log.p=TRUE)
            if(!log) pdf = exp(pdf)
            pdf
          }
)

#' @rdname dist_pdf-methods
#' @aliases dist_pdf,disexp-method
setMethod("dist_pdf",
          signature = signature(m="disexp"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            
            l1 = pexp(q-0.5, pars, lower.tail=FALSE, log.p=TRUE)
            l2 = pexp(q+0.5, pars, lower.tail=FALSE, log.p=TRUE)
            
            pdf = l1 + log(1-exp(l2-l1)) - pexp(xmin-0.5, pars, lower.tail=FALSE, log.p=TRUE)
            if(!log) pdf = exp(pdf)
            pdf
            
          }
)





##################################################################
##CTN distributions
##################################################################
#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conpl-method
setMethod("dist_pdf",
          signature = signature(m="conpl"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= xmin]
            }
            pdf = dplcon(q, xmin, pars, TRUE)
            if(!log) pdf = exp(pdf)
            pdf
          }
)

#' @rdname dist_pdf-methods
#' @aliases dist_pdf,contappl-method
setMethod("dist_pdf",
          signature = signature(m="contappl"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= m$xmin]
            }
            
            dtappl(q, xmin, pars[2], pars[2], log=log)
            
          }
)



#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conlnorm-method
setMethod("dist_pdf",
          signature = signature(m="conlnorm"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= m$xmin]
            }
            pdf = dlnorm(q, pars[1], pars[2], log=TRUE) - 
              plnorm(xmin, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            if(!log) pdf = exp(pdf)
            pdf
            
          }
)



#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conexp-method
setMethod("dist_pdf",
          signature = signature(m="conexp"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= m$xmin]
            }
            pdf = dexp(q, pars, log=TRUE) - pexp(xmin, pars, lower.tail=FALSE, log.p=TRUE)
            if(!log) pdf = exp(pdf)
            pdf
          }
)

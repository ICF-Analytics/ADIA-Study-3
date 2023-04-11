library(ebal)
#Entropy Balancing
#Reference:
#https://web.stanford.edu/~jhain/Paper/eb.pdf

# this is a minor modification to be aible to enter targets

ebw <- function(id, covariates,target.margins,base.weight,
                print.level=0,max.iterations=200,  tol =1/1000) {
  tgt = c(1,target.margins)
  co.x=  as.matrix(cbind(1,covariates))
  eb.out  <- eb(tr.total = tgt,
                co.x = co.x,
                coefs =   rep(0, length(target.margins)+1) ,
                base.weight = base.weight,
                max.iterations = max.iterations,
                constraint.tolerance =  tol,
                print.level = print.level)
  
  z <- list(target.margins =  tgt, co.xdata = co.x, w = eb.out$Weights.ebal,
            coefs = eb.out$coefs, maxdiff = eb.out$maxdiff, norm.constant = 1,
            constraint.tolerance =  tol, max.iterations = max.iterations,
            base.weight = base.weight, print.level = print.level,
            converged = eb.out$converged)
  class(z) <- "ebalance"
  
  ebal <- tryCatch(ebalance.trim(z),
                   error = function(e) {
                     data.frame(id=id,w=eb.out$Weights.ebal)
                   })
  
  return(data.frame(id=id,wb=ebal$w))
}

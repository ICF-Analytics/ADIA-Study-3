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

library(CVXR)

my_scale <- function(covariates, target.margins) {
  sigma <- sqrt(colMeans(covariates * covariates) - colMeans(covariates)^2)
  j <- apply(covariates, 2, \(z) length(unique(z)) == 2) &
    (colMeans(covariates) < .05 | colMeans(covariates) > .95)
  sigma[j] <- sqrt(.05 * .95)
  scale(covariates, center = target.margins, scale = sigma)
}

my_scale_rank <- function(covariates, target.margins) {
  W   <- rbind(target.margins, covariates)
  R   <- apply(W, 2, rank)
  tgt <- R[1, ]
  R   <- R[-1, ]
  cv  <- cov(R)
  vuntied <- var(seq_len(nrow(R)))
  diag(cv)[diag(cv) == 0] <- 0.01
  rat  <- sqrt(vuntied / diag(cv))
  cv   <- diag(rat) %*% cv %*% diag(rat)
  scale(R, center = tgt, scale = diag(cv))
}

find_weights <- function(id, covariates
                         , lambda = .1
                         #' initial set of weights, sampling weights
                         , base.weights = NULL
                         , target.margins = colMeans(X)
) {

  #https://web.stanford.edu/~boyd/papers/pdf/cvxr_paper.pdf
  #  see page 18  Kelly gambling
  # covariates   <- covariates[base.weights > 0, ]
  # id           <- id[base.weights > 0]
  # base.weights <- base.weights[base.weights > 0]

  n     <-  nrow(covariates)
  w     <-  Variable(n)
  x     <-  my_scale(covariates, target.margins)
  if (is.null(base.weights))
    base.weights <- rep(1 / n, n)
  base.weights <- base.weights / sum(base.weights) #so weights add up to 1
  objtive       <- Minimize(
    sum((t(x) %*% w)^2) +
      lambda * n * sum((w - base.weights)^2)
  )

  constraints   <- list(w >= 0, sum(w) == 1)

  problem       <- Problem(objtive, constraints = constraints)

  result        <- solve(problem)

  print(result$status)
  slw <- round(result$getValue(w), 4)
  return(data.frame(id = id, wb = slw))
}

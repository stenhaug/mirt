ExtractLambdas <- function(x){
    x@par[seq_len(x@nfact)]
}

#change long pars for groups into mean in sigma
ExtractGroupPars <- function(x){
    if(x@itemclass < 0L) return(list(gmeans=0, gcov=matrix(1)))
    nfact <- x@nfact
    gmeans <- x@par[seq_len(nfact)]
    phi_matches <- grepl("PHI", x@parnames)
    if (x@dentype == "Davidian") {
        phi <- x@par[phi_matches]
        tmp <- x@par[-c(seq_len(nfact), which(phi_matches))]
        gcov <- matrix(0, nfact, nfact)
        gcov[lower.tri(gcov, diag=TRUE)] <- tmp
        if(nfact != 1L)
            gcov <- gcov + t(gcov) - diag(diag(gcov))
        return(list(gmeans=gmeans, gcov=gcov, phi=phi))
    } else {
        par <- x@par
        if(x@dentype == "mixture") par <- par[-length(par)] # drop pi
        tmp <- par[-seq_len(nfact)]
        gcov <- matrix(0, nfact, nfact)
        gcov[lower.tri(gcov, diag=TRUE)] <- tmp
        if(nfact != 1L)
            gcov <- gcov + t(gcov) - diag(diag(gcov))
        return(list(gmeans=gmeans, gcov=gcov))
    }
}

myApply <- function(X, MARGIN, FUN, ...){
    t(apply(X=X, MARGIN=MARGIN, FUN=FUN, ...)) # this was fancier based on clusters
}

mirt_rmvnorm <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
                         check = FALSE, pre.ev=list())
{
    if(!length(pre.ev)){
        # Version modified from mvtnorm::rmvnorm, version 0.9-9996, 19-April, 2014.
        if(check){
            if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps), check.attributes = FALSE))
                stop("sigma must be a symmetric matrix", call.=FALSE)
            if (length(mean) != nrow(sigma))
                stop("mean and sigma have non-conforming size", call.=FALSE)
        }
        ev <- eigen(sigma, symmetric = TRUE)
        NCOL <- ncol(sigma)
        if(check)
            if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1])))
                warning("sigma is numerically non-positive definite", call.=FALSE)
    } else {
        ev <- pre.ev
        NCOL <- length(ev$values)
    }
    retval <- ev$vectors %*% diag(sqrt(ev$values), NCOL) %*% t(ev$vectors)
    retval <- matrix(rnorm(n * NCOL), nrow = n) %*%  retval
    retval <- sweep(retval, 2L, mean, "+")
    colnames(retval) <- names(mean)
    retval
}

computeItemtrace <- function(pars, Theta, itemloc, offterm = matrix(0L, 1L, length(itemloc)-1L),
                             CUSTOM.IND, pis = NULL){
    itemtrace <- .Call('computeItemTrace', pars, Theta, itemloc, offterm)
    return(itemtrace)
}

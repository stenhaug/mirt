LLdraws <- function(LLDUMMY=NULL, nfact, N, grp, prodlist, fulldata, object, J, random, ot,
                    CUSTOM.IND, lrPars, pars, itemloc, lr.random){
    theta <- mirt_rmvnorm(N,grp$gmeans, grp$gcov)
    itemtrace <- computeItemtrace(pars=pars, Theta=theta, itemloc=itemloc, offterm=ot, CUSTOM.IND=CUSTOM.IND)
    rowSums(log(itemtrace) * fulldata)
}

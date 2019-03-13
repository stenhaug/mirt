calcLogLik <- function(object, draws = 5000, G2 = TRUE, lrPars = NULL){
    # this is called monte carlo log likelihood

    # parameters i understand
    N <- nrow(fulldata)
    J <- length(pars)-1L
    nfact <- length(ExtractLambdas(pars[[1L]])) - length(object@Model$prodlist) - pars[[1L]]@nfixedeffects
    grp <- ExtractGroupPars(pars[[length(pars)]])

    # random parameters
    pars <- object@ParObjects$pars
    fulldata <- object@Data$fulldata[[1]] # odd I had to add in the [[1]]
    prodlist <- object@Model$prodlist
    itemloc <- object@Model$itemloc
    ot <- matrix(0, 1L, J)

    # start with students by draws
    # fill in each column by randomly drawing theta and calculuating probabilty of response string
    # average across all of the draws for each student
    # sum up probability
    LL <- matrix(0, N, draws)
    LL <- apply(LL, 2, FUN=LLdraws, nfact=nfact, lrPars=lrPars, pars=pars, itemloc=itemloc, N=N, grp=grp, prodlist=prodlist, fulldata=fulldata, object=object, J=J, random=object@ParObjects$random, ot=ot, CUSTOM.IND=object@Internals$CUSTOM.IND, lr.random=object@ParObjects$lr.random)
    LL <- exp(LL)
    LL[is.nan(LL)] <- 0

    mean_prob_of_student_responses <- rowMeans(LL)
    logLik <- sum(log(mean_prob_of_student_responses))
    logLik
}

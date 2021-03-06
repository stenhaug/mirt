estimation_simple <- function(data, model, group, itemtype = NULL, guess = 0, upper = 1,
                       invariance = '', pars = NULL, constrain = NULL, key = NULL,
                       parprior = NULL, mixed.design = NULL, customItems = NULL,
                       customItemsData = NULL, customGroup = NULL,
                       GenRandomPars = FALSE, large = FALSE,
                       survey.weights = NULL, latent.regression = NULL,
                       gpcm_mats=list(), spline_args=list(), monopoly.k=1,
                       control = list(), ...)
{

    # A) Beginning stuff ---------------------------------------------------------
    start.time <- proc.time()[3L]
    dots <- list(...)

    opts <- makeopts(GenRandomPars=GenRandomPars, hasCustomGroup=!is.null(customGroup), ...) # this was start of that big if
    opts$times <- list(start.time=start.time)

    if(opts$method == 'MHRM') {on.exit(set.seed((as.numeric(Sys.time()) - floor(as.numeric(Sys.time()))) * 1e8))} # on exit, reset the seed to override internal

    opts$times$start.time.Data <- proc.time()[3L]
    Data <- list()
    data <- as.matrix(data)
    data <- matrix(as.integer(data), nrow(data), dimnames=list(rownames(data), colnames(data)))
    rownames(data) <- 1L:nrow(data)
    Data$data <- data

    Data$grsm.block <- rep(1L, ncol(data))
    Data$rsm.block <- rep(1L, ncol(data))

    Data$group <- factor(group)
    Data$groupNames <- levels(Data$group)

    Data$ngroups <- if(!is.null(opts$ngroups)) opts$ngroups else length(Data$groupNames)
    Data$nitems <- ncol(Data$data)
    Data$N <- nrow(Data$data)
    Data$mins <- apply(data, 2L, min, na.rm=TRUE)

    oldmodel <- model
    PrepList <- vector('list', Data$ngroups)
    tmp <- 1L:Data$ngroups

    names(PrepList) <- Data$groupNames
    model <- buildModelSyntax(model, J=Data$nitems, groupNames=Data$groupNames,
                              itemtype=itemtype)
    Data$model <- model
    PrepListFull <- PrepList[[1L]] <- PrepData(data=Data$data, model=Data$model, itemtype=itemtype, guess=guess, upper=upper, parprior=parprior, verbose=opts$verbose, technical=opts$technical, parnumber=1L, BFACTOR=opts$dentype == 'bfactor', grsm.block=Data$grsm.block, rsm.block=Data$rsm.block, mixed.design=mixed.design, customItems=customItems, customItemsData=customItemsData, customGroup=customGroup, spline_args=spline_args, monopoly.k=monopoly.k, fulldata=opts$PrepList[[1L]]$fulldata, key=key, opts=opts, gpcm_mats=gpcm_mats, internal_constraints=opts$internal_constraints, dcIRT_nphi=opts$dcIRT_nphi, dentype=opts$dentype, item.Q=opts$item.Q)

    parnumber <- max(PrepList[[1L]]$pars[[Data$nitems+1L]]@parnum) + 1L
    attr(PrepListFull$pars, 'nclasspars') <- attr(PrepList[[1L]]$pars, 'nclasspars') <- sapply(PrepListFull$pars, function(y) length(y@parnum))

    lrPars <- list()

    tmpdata <- Data$data

    tmpdata[is.na(tmpdata)] <- 99999L
    stringfulldata <- apply(tmpdata, 1L, paste, sep='', collapse = '/')
    stringtabdata <- unique(stringfulldata)
    tmptabdata <- maketabData(stringfulldata=stringfulldata, stringtabdata=stringtabdata,
                              group=Data$group,
                              groupNames=if(opts$dentype != 'mixture') Data$groupNames else 'full',
                              nitem=Data$nitems,
                              K=PrepListFull$K, itemloc=PrepListFull$itemloc,
                              Names=PrepListFull$Names, itemnames=PrepListFull$itemnames,
                              survey.weights=survey.weights)

    large <- tmptabdata
    Data$tabdatalong <- large$tabdata
    Data$tabdata <- large$tabdata2

    for(g in seq_len(Data$ngroups)){
        select <- Data$group == Data$groupNames[g]
        Data$fulldata[[g]] <- PrepListFull$fulldata[select, , drop=FALSE]
        Data$Freq[[g]] <- large$Freq[[g]]
    }

    PrepList <- UpdateParameters(PrepList, model, groupNames=Data$groupNames)

    RETURNVALUES <- FALSE

    pars <- vector('list', Data$ngroups)
    for(g in seq_len(Data$ngroups)) pars[[g]] <- PrepList[[g]]$pars
    nitems <- Data$nitems
    Data$K <- PrepList[[1L]]$K
    nfact <- PrepList[[1L]]$pars[[nitems+1L]]@nfact

    nLambdas <- PrepList[[1L]]$pars[[1L]]@nfact
    constrain <- list()


    nspec <- ifelse(!is.null(attr(model, 'nspec')), attr(model, 'nspec'), 1L)

    dummymat <- matrix(FALSE, pars[[1L]][[nitems + 1L]]@nfact, pars[[1L]][[nitems + 1L]]@nfact)

    constrain <- UpdateConstrain(pars=pars, constrain=constrain, invariance=invariance, nfact=Data$nfact,
                                 nLambdas=nLambdas, J=nitems, ngroups=Data$ngroups, PrepList=PrepList,
                                 method=opts$method, itemnames=PrepList[[1L]]$itemnames, model=model,
                                 groupNames=Data$groupNames)
    startlongpars <- c()
    rr <- 0L
    for(g in seq_len(Data$ngroups)){
        r <- Data$Freq[[g]]
        rr <- rr + r
    }
    df <- prod(PrepList[[1L]]$K) - 1
    if(df > 1e10) {
        message("a lot of response combinations")
        df <- 1e10
    }
    nestpars <- nconstr <- 0L
    for(g in seq_len(Data$ngroups))
        for(i in seq_len(nitems+1L))
            nestpars <- nestpars + sum(pars[[g]][[i]]@est) # whats going on here?

    nmissingtabdata <- sum(is.na(rowSums(Data$tabdata)))
    dfsubtr <- nestpars - nconstr

    if(df <= dfsubtr)
        stop('Too few degrees of freedom. There are only ', df, ' degrees of freedom but ',
             dfsubtr, ' parameters were freely estimated.', call.=FALSE)
    df <- df - dfsubtr

    G2group <- numeric(Data$ngroups)
    DERIV <- vector('list', Data$ngroups) # whats going on here?
    for(g in seq_len(Data$ngroups)){
        DERIV[[g]] <- vector('list', Data$nitems)
        for(i in seq_len(Data$nitems))
            DERIV[[g]][[i]] <- selectMethod(Deriv, c(class(pars[[g]][[i]]), 'matrix'))
    }
    Ls <- makeLmats(pars, constrain, random = mixed.design$random, lr.random=latent.regression$lr.random, lrPars=lrPars)
    CUSTOM.IND <- which(sapply(pars[[1L]], class) %in% Use_R_ProbTrace())
    SLOW.IND <- which(sapply(pars[[1L]], class) %in% Use_R_Deriv())
    SEMconv <- NA
    opts$times$end.time.Data <- proc.time()[3L]

    # B) Estimation --------------------------------------------------------------
    opts$times$start.time.Estimate <- proc.time()[3L]

    # B1) EM -------------------------------------------------------------------
    if(opts$method %in% c('EM', 'QMCEM', 'MCEM', 'BL')){
        # prepare
        opts$full <- FALSE
        temp <- matrix(0L,nrow=nitems,ncol=nspec)
        sitems <- matrix(0L, nrow=sum(PrepList[[1L]]$K), ncol=nspec)
        specific <- NULL
        if(is.null(opts$quadpts)){tmp <- nfact; opts$quadpts <- select_quadpts(tmp)}

        # theta: seems to define theta (which is a grid by 0.2 from -6 to 6)
        theta <- 1
        if(!(opts$method %in% c('QMCEM', 'MCEM'))) theta <- as.matrix(seq(opts$theta_lim[1L], opts$theta_lim[2L], length.out = opts$quadpts))

        if(opts$method %in% c('QMCEM', 'MCEM')){
            Theta <- NULL
        } else {
            if(opts$quadpts^nfact <= opts$MAXQUAD){
                if(is.null(opts$technical$customTheta))
                    Theta <- thetaComb(theta, nfact)
            } else stop('Greater than ', opts$MAXQUAD, ' quadrature points.', call.=FALSE)
            if(opts$message && nfact > 3L && !(opts$odentype %in% c('custom', 'discrete')))
                message('EM quadrature for high dimensional models are better handled
                             \twith the \"QMCEM\" or \"MCEM\" method')
        }
        pars <- loadSplinePars(pars, Theta)

        # where the actual magic is happening
        ESTIMATE <- EM_group_simple(pars=pars, constrain=constrain, Ls=Ls, PrepList=PrepList, Data=Data,
                             list = list(NCYCLES=opts$NCYCLES, TOL=opts$TOL, MSTEPTOL=opts$MSTEPTOL,
                                         nfactNames=PrepList[[1L]]$nfactNames, theta=theta,
                                         itemloc=PrepList[[1L]]$itemloc, dentype=opts$dentype,
                                         sitems=sitems, specific=specific, NULL.MODEL=opts$NULL.MODEL,
                                         nfact=nfact, constrain=constrain, verbose=opts$verbose,
                                         SE = opts$SE, SE.type=opts$SE.type, delta=opts$delta, quadpts=opts$quadpts,
                                         accelerate=opts$accelerate, CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND,
                                         customPriorFun=opts$customPriorFun, Moptim=opts$Moptim, warn=opts$warn,
                                         message=opts$message, method=opts$method, full=opts$full,
                                         lrPars=lrPars, SE=opts$SE && opts$SE.type == 'numerical', Etable=opts$Etable,
                                         NULL.MODEL=opts$NULL.MODEL, PLCI=opts$PLCI, Norder=opts$Norder,
                                         keep_vcov_PD=opts$keep_vcov_PD, symmetric=opts$technical$symmetric,
                                         MCEM_draws=opts$MCEM_draws),
                             Theta=Theta, DERIV=DERIV, solnp_args=opts$solnp_args, control=control)














        if(opts$method == 'MCEM')opts$quadpts <- opts$MCEM_draws(ESTIMATE$cycles)
        opts$Moptim <- ESTIMATE$Moptim
        lrPars <- ESTIMATE$lrPars
        startlongpars <- ESTIMATE$longpars
        rlist <- ESTIMATE$rlist
        logLik <- G2 <- SElogLik <- 0
        logPrior <- ESTIMATE$logPrior
        Pl <- vector('list', length(rlist))
        for(g in seq_len(Data$ngroups)){
            if(g > 1L && opts$dentype == 'mixture') break
            Pl[[g]] <- rlist[[g]]$expected
            Pltmp <- Pl[[g]]
            if(opts$full){
                rg <- 1
                G2group[g] <- NaN
            } else {
                rg <- Data$Freq[[g]]
                Pltmp <- Pltmp[rg != 0]
                rg <- rg[rg != 0]
                Ng <- sum(rg)
                G2group[g] <- 2 * sum(rg * log(rg/(Ng*Pltmp)))
            }
            G2 <- G2 + G2group[g]
            logLik <- logLik + sum(rg*log(Pltmp))
        }
    }

    # B2 MHRM ------------------------------------------------------------------
    else if(opts$method %in% c('MHRM', 'SEM')){ #MHRM estimation
        Theta <- matrix(0, Data$N, nitems)
        if(opts$method == 'SEM') opts$NCYCLES <- NA
        ESTIMATE <- MHRM.group(pars=pars, constrain=constrain, Ls=Ls, PrepList=PrepList, Data=Data,
                               list = list(NCYCLES=opts$NCYCLES, BURNIN=opts$BURNIN,
                                           SEMCYCLES=opts$SEMCYCLES, gain=opts$gain,
                                           KDRAWS=opts$KDRAWS, MHDRAWS=opts$MHDRAWS,
                                           TOL=opts$TOL, SE=FALSE, SE.type = 'none',
                                           nfactNames=PrepList[[1L]]$nfactNames,
                                           itemloc=PrepList[[1L]]$itemloc,
                                           nfact=nfact, constrain=constrain, verbose=opts$verbose,
                                           CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND,
                                           startlongpars=startlongpars,
                                           cand.t.var=opts$technical$MHcand, warn=opts$warn,
                                           message=opts$message, expl=PrepList[[1L]]$exploratory,
                                           plausible.draws=opts$plausible.draws,
                                           MSTEPTOL=opts$MSTEPTOL, Moptim=opts$Moptim,
                                           keep_vcov_PD=opts$keep_vcov_PD),
                               DERIV=DERIV, solnp_args=opts$solnp_args, control=control)
        if(opts$plausible.draws != 0) return(ESTIMATE)
        if(opts$SE && (ESTIMATE$converge || !opts$info_if_converged)){
            if(opts$verbose)
                cat('\nCalculating information matrix...\n')
            tmp <- MHRM.group(pars=ESTIMATE$pars, constrain=constrain, Ls=Ls, PrepList=PrepList, Data=Data,
                              list = list(NCYCLES=opts$MHRM_SE_draws, BURNIN=1L,
                                          SEMCYCLES=opts$SEMCYCLES, gain=opts$gain,
                                          KDRAWS=opts$KDRAWS, MHDRAWS=opts$MHDRAWS,
                                          TOL=opts$SEtol, SE=TRUE, SE.type=opts$SE.type,
                                          nfactNames=PrepList[[1L]]$nfactNames,
                                          itemloc=PrepList[[1L]]$itemloc,
                                          nfact=nfact, constrain=constrain, verbose=FALSE,
                                          CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND,
                                          startlongpars=ESTIMATE$longpars, plausible.draws=0L,
                                          cand.t.var=opts$technical$MHcand, warn=opts$warn,
                                          message=opts$message, expl=PrepList[[1L]]$exploratory,
                                          MSTEPTOL=opts$MSTEPTOL, Moptim='NR1',
                                          keep_vcov_PD=opts$keep_vcov_PD),
                              DERIV=DERIV, solnp_args=opts$solnp_args, control=control)
            ESTIMATE$pars <- tmp$pars
            ESTIMATE$info <- tmp$info
            ESTIMATE$fail_invert_info <- tmp$fail_invert_info
            ESTIMATE$time <- c(ESTIMATE$time, SE=sum(tmp$time))
        }
        rlist <- vector('list', Data$ngroups)
        for(g in seq_len(Data$ngroups))
            rlist[[g]]$expected = numeric(1L)
    }

    # C) Model checks ------------------------------------------------------------
    for(g in seq_len(length(pars))){
        for(i in seq_len(length(pars[[1L]]))){
            if(class(pars[[g]][[i]]) == 'dich'){
                tmp <- ESTIMATE$pars[[g]][[i]]@par
                nms <- ESTIMATE$pars[[g]][[i]]@parnames
                if(tmp[nms == 'g'] > tmp[nms == 'u']){
                    if(opts$warn)
                        warning('g parameter greater than u detected. Model did not converge', call.=FALSE)
                    ESTIMATE$converge <- FALSE
                }
            }
        }
    }
    opts$times$end.time.Estimate <- proc.time()[3L]
    if(opts$logLik_if_converged && !ESTIMATE$converge) opts$draws <- 0
    opts$times$start.time.SE <- proc.time()[3L]
    if(!opts$NULL.MODEL && opts$SE){
        tmp <- ESTIMATE
        if(opts$verbose && !(opts$method %in% c('MHRM', 'MIXED', 'SEM')))
            cat('\n\nCalculating information matrix...\n')
        if(opts$SE.type %in% c('complete', 'Oakes') && opts$method %in% c('EM', 'QMCEM')){
            opts$times$start.time.SE <- ESTIMATE$start.time.SE
            ESTIMATE <- loadESTIMATEinfo(info=-ESTIMATE$hess, ESTIMATE=ESTIMATE, constrain=constrain,
                                         warn=opts$warn)
        } else if(opts$SE.type == 'SEM' && opts$method == 'EM'){
            collectLL <- as.numeric(ESTIMATE$collectLL)
            collectLL <- exp(c(NA, collectLL) - c(collectLL, NA))
            from <- suppressWarnings(max(which(collectLL <= opts$SEM_from)))
            if(from < 1L) from <- 1L
            to <- min(which(collectLL >= opts$SEM_to))
            dontrun <- FALSE
            if(from == to){
                if(opts$warn)
                    warning('SEM window is too small to compute information matrix.
                            Consider changing the starting values', call.=FALSE)
                dontrun <- TRUE
            }
            lengthsplit <- do.call(c, lapply(strsplit(names(ESTIMATE$correct), 'COV_'), length))
            lengthsplit <- lengthsplit + do.call(c, lapply(strsplit(names(ESTIMATE$correct), 'MEAN_'), length))
            is.latent <- lengthsplit > 2L
            if(!dontrun){
                if(ESTIMATE$cycles <= 10L)
                    if(opts$message)
                        message('Very few EM cycles performed. Consider decreasing TOL further to
                            increase EM iteration count or starting farther away from ML estimates by
                            passing the \'GenRandomPars = TRUE\' argument')
                estmat <- matrix(FALSE, length(ESTIMATE$correction), length(ESTIMATE$correction))
                DM <- estmat + 0
                diag(estmat) <- TRUE
                if(!opts$technical$parallel){
                    ncores <- .mirtClusterEnv$ncores
                    .mirtClusterEnv$ncores <- 1L
                }
                DM <- myLapply(1L:ncol(estmat), FUN=SE.SEM, estmat=estmat, pars=ESTIMATE$pars, constrain=constrain, Data=Data,
                               list = list(NCYCLES=opts$NCYCLES, TOL=opts$SEtol, MSTEPTOL=opts$MSTEPTOL,
                                           nfactNames=PrepList[[1L]]$nfactNames, theta=theta,
                                           itemloc=PrepList[[1L]]$itemloc, keep_vcov_PD=opts$keep_vcov_PD,
                                           sitems=sitems, specific=specific, NULL.MODEL=opts$NULL.MODEL,
                                           nfact=nfact, constrain=constrain, verbose=opts$verbose,
                                           CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND, Moptim=ESTIMATE$Moptim,
                                           EHPrior=ESTIMATE$Prior, warn=opts$warn, dentype=opts$dentype,
                                           message=opts$message, full=opts$full, lrPars=lrPars, method=opts$method),
                               Theta=Theta, theta=theta, ESTIMATE=ESTIMATE, from=from, to=to,
                               DERIV=DERIV, is.latent=is.latent, Ls=Ls, PrepList=PrepList,
                               solnp_args=opts$solnp_args, control=control)
                SEMconv <- sapply(DM, function(x) all(attr(x, 'converged')))
                if(!all(SEMconv)){
                    warning(sprintf(c('%i parameters did not converge in numerical SEM derivative.\n',
                                      'Try using different starting values or passing GenRandomPars=TRUE'),
                                    sum(!SEMconv)),
                            call.=FALSE)
                    SEMconv <- FALSE
                } else SEMconv <- TRUE
                DM <- do.call(rbind, DM)
                if(!opts$technical$parallel)
                    .mirtClusterEnv$ncores <- ncores
                ESTIMATE$pars <- reloadPars(longpars=ESTIMATE$longpars, pars=ESTIMATE$pars,
                                            ngroups=Data$ngroups, J=Data$nitems)
                DM[, is.latent] <- DM[is.latent, ]
                DM[is.latent, is.latent] <- 0
                info <- try(solve(-solve(ESTIMATE$hess) %*% solve(diag(ncol(DM)) - DM)), silent=TRUE)
                info[,is.latent] <- t(info[is.latent, ,drop=FALSE])
                if(opts$technical$symmetric) info <- (info + t(info)) / 2
                if(is(info, 'try-error')){
                    if(opts$warn)
                        warning('Information matrix in SEM could not be computed due to instability',
                                call.=FALSE)
                } else ESTIMATE <- loadESTIMATEinfo(info=info, ESTIMATE=ESTIMATE, constrain=constrain,
                                                    warn=opts$warn)
            }
        } else if(opts$SE.type == 'numerical' && opts$method == 'BL'){
            ESTIMATE <- loadESTIMATEinfo(info=-ESTIMATE$hess, ESTIMATE=ESTIMATE, constrain=constrain,
                                         warn=opts$warn)
        } else if(opts$SE.type %in% c('Richardson', 'forward', 'central') &&
                  !(opts$method %in% c('MHRM', 'SEM', 'MIXED'))){
            ESTIMATE <- SE.Numerical(pars=ESTIMATE$pars, Theta=ESTIMATE$Theta, theta=theta, PrepList=PrepList, Data=Data,
                                     dentype=opts$dentype, itemloc=PrepList[[1L]]$itemloc, ESTIMATE=ESTIMATE,
                                     constrain=constrain, Ls=Ls, specific=oldmodel, sitems=sitems,
                                     CUSTOM.IND=CUSTOM.IND, EHPrior=ESTIMATE$Prior, warn=opts$warn, type=opts$SE.type,
                                     delta=opts$delta, lrPars=ESTIMATE$lrPars)
        } else if(opts$SE.type == 'MHRM' && opts$method == 'EM'){
            if(opts$dentype %in% c('EH', 'EHW'))
                stop('MHRM standard error methods not available when using empirical histograms', call.=FALSE)
            ESTIMATE <- MHRM.group(pars=pars, constrain=constrain, Ls=Ls, PrepList=PrepList, Data=Data,
                                   list = list(NCYCLES=1000L, BURNIN=1L, SEMCYCLES=opts$SEMCYCLES,
                                               KDRAWS=opts$KDRAWS, MHDRAWS=opts$MHDRAWS,
                                               TOL=opts$SEtol, SE=TRUE, SE.type=opts$SE.type,
                                               gain=opts$gain, nfactNames=PrepList[[1L]]$nfactNames,
                                               itemloc=PrepList[[1L]]$itemloc,
                                               nfact=nfact, constrain=constrain, verbose=FALSE, expl=FALSE,
                                               CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND, message=opts$message,
                                               startlongpars=startlongpars, SE=opts$SE, warn=opts$warn,
                                               plausible.draws=0L, MSTEPTOL=opts$MSTEPTOL, Moptim='NR1',
                                               keep_vcov_PD=opts$keep_vcov_PD),
                                   DERIV=DERIV, solnp_args=opts$solnp_args, control=control)
        } else if(any(opts$SE.type %in% c('crossprod', 'Louis', 'sandwich.Louis', 'sandwich')) &&
                  !(opts$method %in% c('MHRM', 'SEM', 'MIXED'))){
            if(logPrior != 0 && opts$warn)
                warning('Information matrix with the crossprod, Louis, and sandwich method
                        do not account for prior parameter distribution information')
            ESTIMATE <- SE.simple(PrepList=PrepList, ESTIMATE=ESTIMATE, Theta=Theta, Data=Data,
                                  constrain=constrain, Ls=Ls, N=nrow(data), type=opts$SE.type,
                                  CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND, warn=opts$warn,
                                  message=opts$message, complete=ESTIMATE$hess)
        } else if(opts$SE.type == 'Fisher' && !(opts$method %in% c('MHRM', 'SEM', 'MIXED'))){
            if(logPrior != 0 && opts$warn)
                warning('Information matrix with the Fisher method does not
                        account for prior parameter distribution information')
            ESTIMATE <- SE.Fisher(PrepList=PrepList, ESTIMATE=ESTIMATE, Theta=Theta, Data=Data,
                                  constrain=constrain, Ls=Ls, N=nrow(data), full=opts$full,
                                  CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND, warn=opts$warn)
        }
        ESTIMATE$cycles <- tmp$cycles
        ESTIMATE$Prior <- tmp$Prior
    }
    opts$times$end.time.SE <- proc.time()[3L]
    opts$times$start.time.post <- proc.time()[3L]
    cmods <- vector('list', Data$ngroups)
    if(is.null(opts$theta_lim))
        opts$theta_lim <- numeric(1)
    lrPars <- ESTIMATE$lrPars
    class(lrPars) <- 'S4'
    for(g in seq_len(Data$ngroups)){
        if(opts$method == 'MIXED' || opts$dentype == "discrete"){
            F <- matrix(NA)
            h2 <- numeric(1)
        } else {
            F <- Lambdas(ESTIMATE$pars[[g]], Names=colnames(data))
            colnames(F) <- PrepList[[1L]]$factorNames
            h2 <- rowSums(F^2)
        }
        cmods[[g]] <- new('SingleGroupClass', ParObjects=list(pars=ESTIMATE$pars[[g]], lrPars=lrPars,
                                                              random=ESTIMATE$random,
                                                              lr.random=ESTIMATE$lr.random),
                          Data = list(K=Data$K, nitems=Data$nitems),
                          Model = list(itemloc=PrepList[[1L]]$itemloc, nfact=nfact, constrain=constrain,
                                       factorNames=PrepList[[1L]]$factorNames,
                                       itemtype=PrepList[[1L]]$itemtype,
                                       prodlist=PrepList[[1L]]$prodlist),
                          Options = list(method = 'MHRM', exploratory=PrepList[[1L]]$exploratory,
                                         theta_lim=opts$theta_lim, dentype=opts$dentype),
                          Fit = list(G2=G2group[g], F=F, h2=h2),
                          Internals = list(Pl = rlist[[g]]$expected, CUSTOM.IND=CUSTOM.IND,
                                           SLOW.IND=SLOW.IND))
        if(opts$dentype %in% c("discrete", 'EH', 'EHW', 'Davidian')){
            cmods[[g]]@Model$Theta <- Theta
            cmods[[g]]@Internals$Prior <- list(ESTIMATE$Prior[[g]])
        }
    }
    #missing stats for MHRM
    if(opts$method %in% c('MHRM', 'MIXED', 'SEM')){
        Pl <- vector('list', Data$ngroups)
        logPrior <- logLik <- SElogLik <- G2 <- 0
        if(opts$draws > 0L){
            if(opts$verbose) cat("\nCalculating log-likelihood...\n")
            flush.console()
            if(!opts$technical$parallel){
                ncores <- .mirtClusterEnv$ncores
                .mirtClusterEnv$ncores <- 1L
            }
            for(g in seq_len(Data$ngroups)){
                cmods[[g]]@Data <- list(data=Data$data[Data$group == Data$groupName[g], ],
                                        fulldata=Data$fulldata[[g]], tabdata=Data$tabdata,
                                        Freq=list(Data$Freq[[g]]), K=Data$K)
                cmods[[g]] <- calcLogLik(cmods[[g]], opts$draws, G2 = 'return',
                                         lrPars=ESTIMATE$lrPars)
                cmods[[g]]@Data <- list(K=Data$K, nitems=Data$nitems)
                logLik <- logLik + cmods[[g]]@Fit$logLik
                logPrior <- logPrior + cmods[[g]]@Fit$logPrior
                SElogLik <- SElogLik + cmods[[g]]@Fit$SElogLik
                G2 <- G2 + cmods[[g]]@Fit$G2
                Pl[[g]] <- cmods[[g]]@Internals$Pl
            }
            if(!opts$technical$parallel)
                .mirtClusterEnv$ncores <- ncores
        }
    }

    ####post estimation stats
    if(opts$Moptim %in% c('solnp', 'nloptr')){
        if(!is.null(opts$solnp_args$eqfun))
            df <- df + length(opts$solnp_args$eqfun(ESTIMATE$shortpars, list()))
        if(!is.null(opts$solnp_args$eval_g_eq))
            df <- df + length(opts$solnp_args$eval_g_eq(ESTIMATE$shortpars, list()))
    }
    r <- rr
    N <- sum(r)
    tmp <- dfsubtr
    AIC <- (-2) * logLik + 2 * tmp
    AICc <- AIC + 2 * tmp * (tmp + 1) / (N - tmp - 1L)
    BIC <- (-2) * logLik + tmp*log(N)
    SABIC <- (-2) * logLik + tmp*log((N+2)/24)
    DIC <- AIC
    HQ <- (-2) * logLik + 2*tmp*log(log(N))
    if(logPrior != 0) DIC <- -2 * (logLik + logPrior) + 2 * tmp
    p.G2 <- 1 - pchisq(G2,df)
    RMSEA.G2 <- rmsea(X2=G2, df=df, N=N)
    null.mod <- unclass(new('SingleGroupClass'))
    TLI.G2 <- CFI.G2 <- NaN
    if(opts$calcNull && length(r) * 3L < prod(Data$K) && opts$warn)
        warning(c('Full table of responses is very sparse. ',
                  'Goodness-of-fit statistics may be very inaccurate'), call.=FALSE)
    if(!opts$NULL.MODEL && opts$method != 'MIXED' && opts$calcNull && nmissingtabdata == 0L){
        null.mod <- try(unclass(computeNullModel(data=data, itemtype=itemtype, key=key,
                                                 group=if(length(pars) > 1L) group else NULL)))
        if(is(null.mod, 'try-error')){
            if(opts$warn)
                warning('Null model calculation did not converge.')
            null.mod <- unclass(new('SingleGroupClass'))
        } else if(!is.nan(G2)) {
            TLI.G2 <- tli(X2=G2, X2.null=null.mod@Fit$G2, df=df, df.null=null.mod@Fit$df)
            CFI.G2 <- cfi(X2=G2, X2.null=null.mod@Fit$G2, df=df, df.null=null.mod@Fit$df)
        }
    }
    if(nmissingtabdata > 0L)
        p.G2 <- RMSEA.G2 <- G2 <- TLI.G2 <- CFI.G2 <-  NaN
    if(is.null(parprior)) parprior <- list()
    if(is.null(opts$quadpts)) opts$quadpts <- NaN
    opts$times$end.time.post <- proc.time()[3L]
    time <- opts$times
    opts$times <- NULL
    # set macro objects
    Options <- opts
    Options$exploratory <- PrepList[[1L]]$exploratory
    Fit <- list(G2=G2, p=p.G2, TLI=TLI.G2, CFI=CFI.G2, RMSEA=RMSEA.G2, df=df,
                AIC=AIC, AICc=AICc, BIC=BIC, SABIC=SABIC, DIC=DIC, HQ=HQ, logLik=logLik,
                logPrior=logPrior, SElogLik=SElogLik, F=F, h2=h2)
    pis <- if(opts$dentype == 'mixture')
        ExtractMixtures(lapply(cmods, function(x) x@ParObjects$pars)) else NULL
    Model <- list(model=oldmodel, factorNames=PrepList[[1L]]$factorNames, itemtype=PrepList[[1L]]$itemtype,
                  itemloc=PrepList[[1L]]$itemloc, nfact=nfact, pis=pis,
                  Theta=Theta, constrain=constrain, parprior=parprior, nest=as.integer(dfsubtr),
                  invariance=invariance, lrPars=lrPars, formulas=attr(mixed.design, 'formula'),
                  prodlist=PrepList[[1L]]$prodlist, nestpars=nestpars)
    if(!is.null(opts$technical$Etable)){
        Model$Etable <- ESTIMATE$rlist
        Model$Etable$Theta <- Theta
    }
    Data$covdata <- if(length(lrPars)) lrPars@df else attr(mixed.design, 'covdata')
    Data$itemdesign <- attr(mixed.design, 'itemdesign')
    ParObjects <- list(pars=cmods, lrPars=lrPars, random=ESTIMATE$random, lr.random=ESTIMATE$lr.random)
    OptimInfo <- list(iter=ESTIMATE$cycles, converged=ESTIMATE$converge, cand.t.var=ESTIMATE$cand.t.var,
                      condnum=NA, secondordertest=NA, SEMconv=SEMconv)
    vcov <- matrix(NA, 1, 1)
    if(Options$SE){
        information <- ESTIMATE$info
        if(!is.null(opts$technical$infoAsVcov)){
            vcov <- information
        } else {
            if(!ESTIMATE$fail_invert_info){
                isna <- is.na(diag(information))
                info <- information[!isna, !isna]
                vcov <- matrix(NA, ncol(information), ncol(information))
                rownames(vcov) <- colnames(vcov) <- colnames(information)
                vcov2 <- try(solve(info), silent=TRUE)
                vcov[!isna, !isna] <- vcov2
                if(!is(vcov2, 'try-error')){
                    OptimInfo$condnum <- kappa(info, exact=TRUE)
                    OptimInfo$secondordertest <- all(eigen(info)$values > 0)
                } else OptimInfo$secondordertest <- FALSE
            } else {
                OptimInfo$secondordertest <- FALSE
            }
        }
    }
    Internals <- list(collectLL=ESTIMATE$collectLL, Prior=ESTIMATE$Prior, Pl=Pl,
                      shortpars=as.numeric(ESTIMATE$shortpars), key=key,
                      bfactor=list(), CUSTOM.IND=CUSTOM.IND, SLOW.IND=SLOW.IND,
                      survey.weights=survey.weights)
    if(opts$storeEtable)
        Internals$Etable <- ESTIMATE$Etable
    if(opts$method == 'SEM') Options$TOL <- NA
    if(opts$odentype == "discrete"){
        Fit$F <- Fit$h2 <- NULL
        mod <- new('DiscreteClass',
                   Data=Data,
                   Options=Options,
                   Fit=Fit,
                   Model=Model,
                   ParObjects=ParObjects,
                   OptimInfo=OptimInfo,
                   Internals=Internals,
                   vcov=vcov)
    } else {
        if(Data$ngroups == 1L){
            ParObjects$pars <- cmods[[1L]]@ParObjects$pars
            Internals$Pl <- Internals$Pl[[1L]]
            if(opts$method == 'MIXED'){
                Fit$p <- NaN
                mod <- new('MixedClass',
                           Data=Data,
                           Options=Options,
                           Fit=Fit,
                           Model=Model,
                           ParObjects=ParObjects,
                           OptimInfo=OptimInfo,
                           Internals=Internals,
                           vcov=vcov)
            } else {
                if(Options$exploratory){
                    FF <- F %*% t(F)
                    V <- eigen(FF)$vector[ ,1L:nfact]
                    L <- eigen(FF)$values[1L:nfact]
                    if (nfact == 1L) F <- as.matrix(V * sqrt(L))
                    else F <- V %*% sqrt(diag(L))
                    if (sum(F[ ,1L] < 0)) F <- (-1) * F
                    colnames(F) <- paste("F", 1L:ncol(F), sep="")
                    h2 <- rowSums(F^2)
                } else {
                    if(opts$method == 'EM')
                        Internals$bfactor <- list(prior=ESTIMATE$prior,
                                                  Priorbetween=ESTIMATE$Priorbetween,
                                                  sitems=ESTIMATE$sitems, specific=specific)
                }
                mod <- new('SingleGroupClass',
                           Data=Data,
                           Options=Options,
                           Fit=Fit,
                           Model=Model,
                           ParObjects=ParObjects,
                           OptimInfo=OptimInfo,
                           Internals=Internals,
                           vcov=vcov)
            }
        } else {
            if(opts$method == 'EM')
                Internals$bfactor <- list(prior=ESTIMATE$prior,
                                          Priorbetween=ESTIMATE$Priorbetween,
                                          sitems=ESTIMATE$sitems, specific=specific)
            cls <- ifelse(opts$odentype == 'mixture', 'MixtureClass', 'MultipleGroupClass')
            mod <- new(cls,
                       Data=Data,
                       Options=Options,
                       Fit=Fit,
                       Model=Model,
                       ParObjects=ParObjects,
                       OptimInfo=OptimInfo,
                       Internals=Internals,
                       vcov=vcov)
        }
    }
    mod@time <- c("TOTAL:" = as.numeric(proc.time()[3L] - time$start.time),
                  Data = as.numeric(time$end.time.Data - time$start.time.Data),
                  ESTIMATE$time,
                  SE = as.numeric(time$end.time.SE - time$start.time.SE),
                  Post = as.numeric(time$end.time.post - time$start.time.post))
    return(mod)
}

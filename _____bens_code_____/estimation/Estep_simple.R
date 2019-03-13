Estep_simple <- function(pars, Data, gTheta, prior, Prior, Priorbetween, specific, sitems, itemloc, CUSTOM.IND, dentype, ngroups, rlist, full, Etable = TRUE){

    LL <- 0
    tabdata <- if(full) Data$fulldata[[1L]] else Data$tabdatalong

    for(g in seq_len(ngroups)){
        freq <- if(full) 1 else Data$Freq[[g]]

        rlist[[g]] <- Estep.mirt(pars=pars[[g]], tabdata=tabdata, freq=Data$Freq[[g]], CUSTOM.IND=CUSTOM.IND, Theta=gTheta[[g]], prior=Prior[[g]], itemloc=itemloc, full=full, Etable=Etable)

        LL <- LL + sum(freq * log(rlist[[g]]$expected), na.rm = TRUE)

        rlist[[g]]$r1[is.nan(rlist[[g]]$r1)] <- 0
    }

    list(rlist = rlist, LL = LL)
}
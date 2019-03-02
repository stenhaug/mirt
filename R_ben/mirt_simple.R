mirt_simple <- function(data, model, itemtype = NULL, guess = 0, upper = 1, SE = FALSE,
                 covdata = NULL, formula = NULL, SE.type = 'Oakes', method = 'EM',
                 optimizer = NULL, dentype = 'Gaussian',
                 pars = NULL, constrain = NULL, parprior = NULL,
                 calcNull = FALSE, draws = 5000, survey.weights = NULL,
                 quadpts = NULL, TOL = NULL, gpcm_mats = list(), grsm.block = NULL,
                 rsm.block = NULL, monopoly.k = 1L, key = NULL,
                 large = FALSE, GenRandomPars = FALSE, accelerate = 'Ramsay', verbose = TRUE,
                 solnp_args = list(), nloptr_args = list(), spline_args = list(),
                 control = list(), technical = list(), ...)
{
    Call <- match.call()
    latent.regression <- latentRegression_obj(data=data, covdata=covdata,
                                              dentype=dentype, formula=formula, method=method)
    mod <- estimation_simple(data=data, model=model, group=rep('all', nrow(data)),
                      itemtype=itemtype, guess=guess, upper=upper, grsm.block=grsm.block,
                      pars=pars, method=method, constrain=constrain, SE=SE, TOL=TOL,
                      parprior=parprior, quadpts=quadpts, monopoly.k=monopoly.k,
                      technical=technical, verbose=verbose, survey.weights=survey.weights,
                      calcNull=calcNull, SE.type=SE.type, large=large, key=key,
                      accelerate=accelerate, draws=draws, rsm.block=rsm.block,
                      GenRandomPars=GenRandomPars, optimizer=optimizer,
                      solnp_args=solnp_args, nloptr_args=nloptr_args,
                      latent.regression=latent.regression, gpcm_mats=gpcm_mats,
                      control=control, spline_args=spline_args, dentype=dentype, ...)
    if(is(mod, 'SingleGroupClass'))
        mod@Call <- Call
    return(mod)
}

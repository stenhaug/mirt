# set parameters
dim <- 1
n_items <- 10
n_students <- 100

# simulate data
data <-
    mirt::simdata(
        a = matrix(rlnorm(n_items * dim, 0.1, .5), ncol = dim),
        d = matrix(rnorm(n_items), ncol = 1),
        N = n_students,
        itemtype = rep("2PL", n_items),
        mu = rep(0, dim),
        sigma = diag(dim)
    )

# fit models
mirt(data, model = 1, itemtype = "Rasch", method = "EM")
mirt_simple(data, model = 2, itemtype = "2PL", method = "EM", technical = list(NCYCLES = 50))

# debug on and off
debug(mirt)
debug(ESTIMATION)

debug(estimation_simple)
undebug(estimation_simple)


deb



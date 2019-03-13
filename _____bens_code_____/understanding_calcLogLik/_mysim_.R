# first run all other functions in this folder.

# set parameters
dim <- 1
n_items <- 3
n_students <- 10

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

# fit model
object <- mirt::mirt(data, model = 1, itemtype = "2PL")
object@Fit$logLik
calcLogLik(object)


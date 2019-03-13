library(tidyverse)

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

# fit models
mirt(data, model = 1, itemtype = "Rasch", method = "EM")


debug(Estep_simple)
mirt_simple(data, model = 1, itemtype = "Rasch", method = "EM", technical = list(NCYCLES = 3))

# debug on and off
debug(mirt_simple)
debug(estimation_simple)
debug(EM_group_simple)
debug(Estep_simple)

debug(computeItemtrace)

undebug(computeItemtrace)
undebug(mirt_simple)
undebug(estimation_simple)
undebug(EM_group_simple)
undebug(Estep_simple)

# replicate em log likelihood estimation
full_data <- Data$fulldata[[1]]
theta <- seq(-6, 6, length.out = 61)
my_prior <- dnorm(theta) / sum(dnorm(theta))

item_trace <- computeItemtrace(pars[[1]], matrix(theta), itemloc, CUSTOM.IND = integer(0))

list_of_item_trace <- item_trace %>% plyr::alply(1, function(x){x}) %>% map(~ .)

ll_at_each_theta <-
    list_of_item_trace %>%
    map_dbl(~ sum(log(matrix(., ncol = ncol(full_data), nrow = nrow(full_data), byrow = TRUE)) * full_data))


sum(log(matrix(list_of_item_trace[[1]], ncol = ncol(full_data), nrow = nrow(full_data), byrow = TRUE)) * full_data)

sum(ll_at_each_theta * my_prior)

# lowest theta gets the first item right
diff <- 1:n_items %>% map_dbl(~ pars[[1]][[.]]@par["d"])
boot::inv.logit(theta[1] + diff[1]) == item_trace[1, 2]



sum(log() * full_data)

tabdata

pars[[1]][[1]]@par
pars[[1]][[2]]@par
pars[[1]][[3]]@par
pars[[1]][[4]]@par

Prior[[1]] == my_prior

near(my_prior[1], Prior[[1]][1])

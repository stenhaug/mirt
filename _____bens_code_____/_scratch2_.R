tabdata

item_trace

f <- function(th){
    boot::inv.logit(th + diff[1]) * (1 - boot::inv.logit(th + diff[2])) * (1 - boot::inv.logit(th + diff[3])) * dnorm(th)
}

integrate(f, -6, 6)


f2 <- function(th){
    boot::inv.logit(th + diff[1]) * (1 - boot::inv.logit(th + diff[2])) * (1 - boot::inv.logit(th + diff[3]))
}

tibble(
    theta = theta,
    prior = my_prior
) %>%
    mutate(
        p = map_dbl(theta, f2),
        weighted_p = p * prior
    ) %>%
    pull(weighted_p) %>%
    sum()

a <- item_trace * matrix(tabdata[1, ], ncol = 6, nrow = 61, byrow = TRUE)
b <- a[ , c(2, 3, 5)]
c <- apply(b, 1, prod)
sum(c * my_prior)
my_prior

ben <- c()

for (i in 1:61){
    ben[i] <- sum(log(matrix(item_trace[i, ], nrow = 10, ncol = 6, byrow = TRUE)) * full_data)
}


out




full_data

# load package with control shift l

library(tidyverse)

data <- read_rds("~/Desktop/data.rds")

model <- mirt(data, model = 1, itemtype = "2PL", quadpts = 1000, technical = list(theta_lim = c(-6, 6), NCYCLES = 1))
model@Fit$logLik

model_simple <- mirt_simple(data, model = 1, itemtype = "Rasch", quadpts = 10, technical = list(theta_lim = c(-3, 3), NCYCLES = 3))
model_simple@Fit$logLik

calc_log_lik_ghq(model_simple, data, theta_span = 3, quad_points = 10)

undebug(mirt_simple)
undebug(estimation_simple)
undebug(EM_group_simple)
undebug(Estep_simple)
undebug(updatePrior)

anova(model, model_simple)

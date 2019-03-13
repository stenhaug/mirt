fulldata

library(glue)

first <- 1 - data
second <- data

colnames(first) <- glue("Item.{1:ncol(data)}_1")
colnames(second) <- glue("Item.{1:ncol(data)}_2")

first
second

gdata::interleave(first, second)

?gdata::interleave


weave = function(...) {
    l = list(...)
    matrix(do.call(rbind, l), nrow = nrow(l[[1]]))
}

weave(first, second)

alternate.cols <- function(m1, m2) {
    cbind(m1, m2)[, order(c(seq(ncol(m1)), seq(ncol(m2))))]
}

identical(alternate.cols(first, second), fulldata)


a <- alternate.cols(first, second)

fulldata %>% class()

identical(a, fulldata)

library(compare)

install.packages("compare")

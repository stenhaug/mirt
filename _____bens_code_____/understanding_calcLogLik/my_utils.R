coefs_model_mirt <- function(model_mirt){
    mirt::coef(model_mirt) %>%
        magrittr::extract(-length(.)) %>%
        do.call(rbind, .) %>%
        as_tibble()
}

Theta_and_object_to_proba_matrix_response <- function(Theta, object){
    disc_matrix <-
        coefs_model_mirt(object) %>%
        select(starts_with("a")) %>%
        as.matrix()

    diff_matrix <-
        matrix(
            coefs_model_mirt(object)$d,
            nrow = nrow(Theta),
            ncol = nrow(disc_matrix),
            byrow = TRUE
        )

    proba_matrix <- boot::inv.logit(Theta %*% t(disc_matrix) + diff_matrix)
    proba_matrix_response <- ifelse(as.matrix(object@Data$data) == 1, proba_matrix, 1 - proba_matrix)
    proba_matrix_response[is.nan(proba_matrix_response)] <- 0
    proba_matrix_response
}

parse_na_types <- function(s) {
    
    positions <- purrr::map(
        stringr::str_split(s, pattern = ""), 
        match,
        table = names(na_types_dict)
    ) %>%
        unlist()
    
    na_types_dict[positions] %>%
        unlist() %>%
        unname()
}

na_types_dict <- list("r" = NA_real_,
                      "i" = rlang::na_int,
                      "c" = NA_character_,
                      "l" = rlang::na_lgl)

as_glance_tibble <- function(..., na_types) {
    
    cols <- list(...)
    
    if (length(cols) != stringr::str_length(na_types)) {
        stop(
            "The number of columns provided does not match the number of ",
            "column types provided."
        )
    }
    
    na_types_long <- parse_na_types(na_types)
    
    entries <- purrr::map2(cols, 
                           na_types_long, 
                           function(.x, .y) {if (length(.x) == 0) .y else .x})
    
    tibble::as_tibble_row(entries)
    
}

tidy.pgmm <- function(x,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      ...) {
    result <- summary(x)$coefficients %>%
        tibble::as_tibble(rownames = "term") %>%
        dplyr::rename(
            estimate = Estimate,
            std.error = `Std. Error`,
            statistic = `z-value`,
            p.value = `Pr(>|z|)`
        )
    
    if (conf.int) {
        ci <- confint(x, level = conf.level) %>% 
            as.data.frame() %>% 
            rownames_to_column(var = "term") %>% 
            dplyr::rename(
                conf.low = `2.5 %`,
                conf.high = `97.5 %`
            )
        result <- dplyr::left_join(result, ci, by = "term")
    }
    
    result
}


glance.pgmm <- function(x, ...) {
    s <- summary(x)
    as_glance_tibble(
        nobs = stats::nobs(x),
        n.country = pdim(x)$nT$n,
        na_types = "ii"
    )
}

glance.plm <- function(x, ...) {
    s <- summary(x)
    as_glance_tibble(
        nobs = stats::nobs(x),
        n.country = pdim(x)$nT$n,
        na_types = "ii"
    )
}

vcovBK_se <- function(x) {
    plm::vcovBK(x, cluster = "time") %>% 
        diag() %>% 
        sqrt()
}

vcovHC_se <-  function(x) {
    plm::vcovHC(x, method="arellano", cluster="group") %>%  #default setting
        diag() %>% 
        sqrt()
}

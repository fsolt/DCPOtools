#' Check the Mean Correlations of Survey Items
#'
#' \code{mean_corr} summarizes the correlations across items.
#'
#' @param dcpo_input_raw a data frame output by \code{dcpo_setup}
#
#' @details  \code{mean_corr}, when passed a data frame of survey item
#' observations, calculates the correlations between all pairs of items
#' and cutpoints and returns, for each item-cutpoint, its mean correlation
#' with all other included item-cutpoints.  This can be useful for
#' identifying data-entry problems as well as possible multidimensionality
#' issues.
#'
#' @return a data frame
#'
#' @import dplyr
#' @importFrom psych pairwiseCount
#'
#' @export

mean_corr <- function(dcpo_input_raw) {
    df <- dcpo_input_raw %>%
        format_dcpo(scale_q = "problem4", scale_cp = 2) %>%
        pluck("data") %>%
        group_by(country, year, item) %>%
        summarize(mean_perc = y_r/n_r*100) %>%
        pivot_wider(names_from = "item", values_from = "mean_perc") %>%
        ungroup() %>%
        select(ends_with("higher"))

    corr <- cor(df, use = "pairwise.complete.obs") %>%
        round(3) %>%
        as.data.frame() %>%
        rownames_to_column(var = "item") %>%
        pivot_longer(cols = ends_with("higher"),
                     names_to = "var2",
                     values_to = "corr") %>%
        filter(!is.na(corr)) %>%
        left_join({psych::pairwiseCount(df) %>%
                as.data.frame() %>%
                rownames_to_column(var = "item") %>%
                pivot_longer(cols = ends_with("higher"), names_to = "var2", values_to = "count") %>%
                filter(!is.na(count))}, by = c("item", "var2")) %>%
        filter(count > 5 & !(item == var2)) %>%
        group_by(item) %>%
        summarize(mean_corr = mean(corr),
                  mean_obs = mean(count),
                  n_other_items = n()) %>%
        arrange(mean_corr)

    return(corr)
}

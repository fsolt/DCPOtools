#' Select Items with Observations in a Minimum Number of Distinct Country-Years
#'
#' \code{with_min_cy} is a convenience function for filtering data to be input to \code{dcpo} by the number of country-year-items observed
#'
#' @param x a data frame of survey items and marginals generated by \code{dcpo_setup}
#' @param min_yrs the minimum number of distinct country-years needed for an item included in x to be retained
#'
#' @return a data frame
#'
#' @importFrom dplyr "%>%" filter mutate group_by ungroup
#'

#' @export
with_min_cy <- function(x, min_cys) {
    if (!is.na(min_cys)) {
        country <- cy_obs <- NULL

        x <- x %>%
            group_by(item) %>%
            mutate(cy_obs = length(unique(paste(country, year)))) %>%
            filter(cy_obs >= min_cys) %>%
            select(-cy_obs) %>%
            ungroup()
    }
    return(x)
}

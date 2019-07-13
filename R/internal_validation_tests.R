#' Internal Validation Tests for DCPO Models
#'
#' @param dcpo_input the data object created by \code{dcpo_setup} used as input for \code{DCPO::dcpo}
#' @param dcpo_output the output of \code{DCPO::dcpo}
#'
#' @details
#'
#' @return A data frame of validation test statistics, including the model's mean absolute error,
#' the percentage improvement of this mean absolute error over that of the baseline provided by
#' the country means in the available survey data, and the leave-one-out information criterion
#' proposed by Ventari, Gelman, Gabry (2017).  The country-means mean absolute error is also
#' reported for reference.
#'
#' @importFrom loo extract_log_lik loo
#' @importFrom tibble tibble rownames_to_column
#' @importFrom dplyr filter pull first mutate group_by ungroup
#'
#' @export

internal_validation_tests <- function(dcpo_input, dcpo_output) {
    loo_ic <- suppressWarnings(
        dcpo_output %>%
        loo::extract_log_lik() %>%
        loo::loo() %>%
        `[[`("estimates") %>%
        as.data.frame() %>%
        tibble::rownames_to_column("var") %>%
        dplyr::filter(var == "looic") %>%
        dplyr::pull(Estimate)
    )

    y_r_pred <- rstan::extract(dcpo_output, pars = c("y_r_pred")) %>%
        dplyr::first() %>%
        colMeans()
    model_mae <- mean(abs((dcpo_input$y_r/dcpo_input$n_r) - (y_r_pred/gm_data$n_r))) %>%
        round(3)

    country_mean <- dcpo_input$data %>%
        dplyr::group_by(country) %>%
        dplyr::mutate(country_mean = mean(y_r/n_r)) %>%
        dplyr::ungroup()
    country_mean_mae <- mean(abs((country_mean$y_r/country_mean$n_r - country_mean$country_mean))) %>%
        round(3)

    improv_vs_cmmae <- round((country_mean_mae - model_mae)/country_mean_mae * 100, 1)

    model_name <- match.call()$dcpo_output %>%
        as.character()
    ivt_results <- tibble::tibble(model = c(model_name, "country means"),
                   mae = c(model_mae, country_mean_mae),
                   improv_over_cmmae = c(improv_vs_cmmae, NA),
                   loo_ic = c(loo_ic, NA))

    return(ivt_results)
}

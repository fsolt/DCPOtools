#' Internal Validation Tests for DCPO Models
#'
#' @param input_data the data object created by \code{dcpo_setup} used as input for \code{DCPO::dcpo}
#' @param output the output of \code{DCPO::dcpo} or \code{rstan::stan} using a Claassen (2019) Stan file
#' @param model the model employed to estimate the results, either "dcpo" or "claassen".  The default is "dcpo".
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

internal_validation_tests <- function(dcpo_input, dcpo_output, model = c("dcpo", "claassen")) {
    model <- match.arg(model)

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

    if (model == "dcpo") {
        y_r_pred <- rstan::extract(dcpo_output, pars = c("y_r_pred")) %>%
            dplyr::first() %>%
            colMeans()
        model_mae <- mean(abs((dcpo_input$y_r/dcpo_input$n_r) - (y_r_pred/dcpo_input$n_r))) %>%
            round(3)

        country_mean <- dcpo_input$data %>%
            dplyr::group_by(country) %>%
            dplyr::mutate(country_mean = mean(y_r/n_r)) %>%
            dplyr::ungroup()
        country_mean_mae <- mean(abs((country_mean$y_r/country_mean$n_r - country_mean$country_mean))) %>%
            round(3)
    } else if (model == "claassen") {
        x_pred <- rstan::extract(dcpo_output, pars = c("x_pred")) %>%
            dplyr::first() %>%
            colMeans()
        model_mae <- mean(abs((dcpo_input$x/dcpo_input$samp) - (x_pred/dcpo_input$samp))) %>%
            round(3)

        country_mean <- dcpo_input$data %>%
            dplyr::group_by(country) %>%
            dplyr::mutate(country_mean = mean(x/samp)) %>%
            dplyr::ungroup()
        country_mean_mae <- mean(abs((country_mean$x/country_mean$samp - country_mean$country_mean))) %>%
            round(3)
    }

    improv_vs_cmmae <- round((country_mean_mae - model_mae)/country_mean_mae * 100, 1)

    model_name <- match.call()$dcpo_output %>%
        as.character()
    ivt_results <- tibble::tibble(model = c(model_name, "country means"),
                   mae = c(model_mae, country_mean_mae),
                   improv_over_cmmae = c(improv_vs_cmmae, NA),
                   loo_ic = c(loo_ic, NA))

    return(ivt_results)
}

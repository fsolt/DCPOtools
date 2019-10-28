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

internal_validation_tests <- function(dcpo_input, dcpo_output, model = c("dcpo", "claassen", "dgirt")) {
    model <- match.arg(model)

    if (!(model == "dgirt")) {
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
    } else {
        loo_ic <- NA
    }

    if (model == "dcpo") {
        y_r_pred <- rstan::extract(dcpo_output, pars = "y_r_pred") %>%
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
        x_pred <- rstan::extract(dcpo_output, pars = "x_pred") %>%
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
    } else {
        if (!("stanfit" %in% class(dcpo_output))) {
            dcpo_output1 <- dcpo_output@stanfit
        } else {
            dcpo_output1 <- dcpo_output
        }
        pi_pred <- rstan::extract(dcpo_output1, pars = "PI") %>%
            dplyr::first() %>%
            colMeans() %>%
            `dimnames<-`(dimnames(dcpo_input@stan_data$SSSS)) %>%
            reshape::melt(varnames=c("year", "country", "item", "r"))
        pi <- suppressWarnings(
            dcpo_input@stan_data$data %>%
            group_by(country, year, item) %>%
            mutate(pi = n/sum(n)) %>%
            ungroup() %>%
            left_join(pi_pred, by = c("country", "year", "item", "r"))
            )
        model_mae <- mean(abs((pi$pi - pi$value))) %>%
            round(3)

        country_mean <- dcpo_input@stan_data$data %>%
            dplyr::group_by(country, year, item) %>%
            dplyr::arrange(desc(r), .by_group = TRUE) %>%
            dplyr::mutate(y_r = round(cumsum(n)),
                   n_r = round(sum(n))) %>%
            dplyr::arrange(r, .by_group = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::filter(r > 1) %>%
            dplyr::group_by(country) %>%
            dplyr::mutate(country_mean = mean(y_r/n_r)) %>%
            dplyr::ungroup()
        country_mean_mae <- mean(abs((country_mean$y_r/country_mean$n_r - country_mean$country_mean))) %>%
            round(3)
    }

    improv_vs_cmmae <- round((country_mean_mae - model_mae)/country_mean_mae * 100, 1)

    ivt_results <- tibble::tibble(model = model,
                                  cmmae = country_mean_mae,
                                  mae = model_mae,
                                  improv_over_cmmae = improv_vs_cmmae,
                                  loo_ic = loo_ic)

    return(ivt_results)
}

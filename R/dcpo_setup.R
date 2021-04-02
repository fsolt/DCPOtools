#' Prepare Data for DCPO
#'
#' \code{dcpo_setup} prepares survey data for use with the \code{dcpo} function.
#'
#' @param vars a data frame (or, optionally, a .csv file) of survey items
#' @param datapath path to the directory that houses raw survey datasets
#' @param file a file path to save output to (in comma-separated format)
#' @param chime play chime when complete?
#
#' @details \code{dcpo_setup}, when passed a data frame of survey items, collects the
#' responses and formats them for use with the \code{dcpo} function.
#'
#' @return a data frame
#'
#' @import dplyr
#' @import countrycode
#' @importFrom beepr beep
#' @importFrom readr read_csv
#' @importFrom rio import
#' @importFrom forcats fct_relabel
#' @importFrom labelled labelled to_character to_factor
#' @importFrom lubridate year
#' @importFrom stats na.omit setNames
#' @importFrom stringr str_detect str_subset str_extract str_replace str_to_lower str_trim
#' @importFrom utils head
#'
#' @export

dcpo_setup <- function(vars,
                       datapath = "../data/dcpo_surveys",
                       file = "",
                       chime = TRUE) {
  if ("data.frame" %in% class(vars)) {
    vars_table <- vars
  } else {
    vars_table <- readr::read_csv(vars, col_types = "cccc")
  }

  # loop rather than purrr to avoid reloading datasets (some are big and slow)
  all_sets <- list()
  for (i in seq(nrow(vars_table))) {
    cat(i, " ")
    v <- vars_table[i, ]
    ds <- surveys_data[surveys_data$survey==v$survey, ]

    # Import dataset (if necessary)
    if (vars_table[["survey"]][i] != c(0, head(vars_table[["survey"]], -1))[i]) {
      dataset_path <- file.path(datapath,
                                paste0(ds$archive, "_files"),
                                paste0(ds$surv_program, "_files"),
                                ds$file_id)
      dataset_file <- list.files(path = dataset_path) %>% stringr::str_subset(".RData") %>% last()
      if (!is.na(ds$subfile)) dataset_file <- paste0(ds$subfile, ".RData")
      t_data <- rio::import(file.path(dataset_path, dataset_file))

      # Fix column names and make lowercase
      valid_column_names <- stringr::str_to_lower(names(t_data)) %>%
        make.names(names = ., unique = TRUE, allow_ = TRUE)
      names(t_data) <- valid_column_names

      # Get countries
      suppressWarnings(
        t_data$c_dcpo <- if (ds$country_var %in% names(t_data)) {
          if (is.null(attr(t_data[[ds$country_var]], "labels"))) {
            if (!is.null(attr(t_data[[ds$country_var]], "value.labels"))) {
              attr(t_data[[ds$country_var]], "labels") <- attr(t_data[[ds$country_var]],
                                                               "value.labels") %>% as.numeric()
              attr(attr(t_data[[ds$country_var]], "labels"), "names") <- attr(attr(t_data[[ds$country_var]],
                                                                                   "value.labels"), "names")
            }
          }
          t_data[[ds$country_var]] %>%
          {if (!is.null(attr(t_data[[ds$country_var]], "labels")))
            labelled::labelled(., attr(., "labels")) %>%
              labelled::to_factor(levels = "prefixed") %>%
              forcats::fct_relabel(., function(x) stringr::str_replace(x, "\\[\\d+\\]\\s+", ""))
            else .} %>%
            as.character() %>%
            stringr::str_replace("Haití", "Haiti") %>%
            stringr::str_replace("República Dominicana", "Dominican Republic") %>%
            {if (!is.na(ds$cc_dict))
              countrycode(., "orig", "dest", custom_dict = eval(parse(text = ds$cc_dict)))
              else if (!is.na(ds$cc_origin) & !is.na(ds$cc_match))
                  countrycode(., ds$cc_origin, "country.name", custom_match = eval(parse(text = ds$cc_match)))
              else if (!is.na(ds$cc_origin))
                  countrycode(., ds$cc_origin, "country.name")
              else if (!is.na(ds$cc_match))
                countrycode(., "country.name", "country.name",
                            custom_match = eval(parse(text = ds$cc_match)))
              else countrycode(., "country.name", "country.name")}
        } else ds$country_var %>%
          countrycode(., "country.name", "country.name")
      )
      if (ds$survey == "wvs4_swe") {
        t_data <- labelled::remove_labels(t_data)
        t_data <- t_data %>%
          dplyr::filter(c_dcpo == "Sweden")
      }
      if (ds$survey == "tcmeg2004") {
        t_data <- t_data %>%
          dplyr::filter(msurvey == 2)
      }
      if (ds$survey == "cdcee") {
        t_data <- t_data %>%
          dplyr::filter(!v3 == 16)
      }
      if (ds$country_var %in% names(t_data)) {
        t_data <- t_data %>%
          mutate(c_dcpo = if_else(!is.na(c_dcpo), c_dcpo, as.character(.data[[ds$country_var]])))
      }

      # Get years
      t_data$y_dcpo <- if (!is.na(ds$year_dict)) { # if there's a year dictionary...
        t_data[[ds$country_var]] %>%
          labelled::labelled(., attr(., "labels")) %>%
          labelled::to_factor(levels = "labels") %>%
          as.character() %>%
          countrycode("orig", "year", custom_dict = eval(parse(text = ds$year_dict)))
      } else if (!is.na(ds$year_var)) { # if there's a year variable...
        if (length(unique(t_data$c_dcpo))==1) { # single-country study
          t_data[[ds$year_var]]
        } else if (stringr::str_detect(ds$survey, "ess")) {
          if ("inwyr" %in% names(t_data)) {
            t_data %>%
              group_by(c_dcpo) %>%
              mutate(year = ifelse(!is.na(inwyr) & !inwyr==9999, inwyr, 2000 + essround * 2),
                     y_dcpo = round(mean(year, na.rm = TRUE))) %>%
              ungroup() %>%
              .[["y_dcpo"]]
          } else if ("inwyys" %in% names(t_data)) {
            t_data %>%
              group_by(c_dcpo) %>%
              mutate(year = ifelse(!is.na(inwyys) & !inwyys==9999, inwyys, 2000 + essround * 2),
                     y_dcpo = round(mean(year, na.rm = TRUE))) %>%
              ungroup() %>%
              .[["y_dcpo"]]
          }
        } else if (ds$survey == "cdcee" | ds$survey == "eqls") {
          suppressWarnings(
            t_data[[ds$year_var]] %>%
              labelled::labelled(., attr(., "labels")) %>%
              labelled::to_character(levels = "prefixed") %>%
              stringr::str_extract("\\d{4}")
          )
        } else if (ds$survey == "amb_combo") {
          t_data[[ds$year_var]]
        } else if (ds$survey == "neb_combo") {
          floor(t_data[[ds$year_var]])
        } else if (ds$surv_program == "afrob" |   # single-wave cross-national surveys
                   ds$survey == "arabb3" |        # with interviews bleeding over years
                   ds$survey == "asianb3") {
          t_data %>%
            mutate(year = lubridate::year(t_data[[ds$year_var]]),
                   modal_year = as.integer(names(table(year)[table(year)==max(table(year))])),
                   year = if_else(is.na(year) | year < 1950,
                                  modal_year,
                                  as.integer(year)),
                   group_dcpo = c_dcpo) %>%
            group_by(c_dcpo) %>%
            mutate(y_dcpo = round(mean(year))) %>%
            ungroup() %>%
            .[["y_dcpo"]]
        } else { # cross-national surveys with interviews bleeding over years
          t_data %>%
            mutate(modal_year = as.integer(names(table(t_data[[ds$year_var]])[table(t_data[[ds$year_var]])==max(table(t_data[[ds$year_var]]))])),
                   year = if_else(between(as.numeric(t_data[[ds$year_var]]),
                                 1950, as.numeric(lubridate::year(Sys.Date()))),
                         as.integer(t_data[[ds$year_var]]),
                         modal_year),
                   group_dcpo = c_dcpo) %>%
                   {if (!is.na(ds$cy_var))
                     mutate(., group_dcpo = t_data[[ds$cy_var]])
                     else .} %>%
            group_by(group_dcpo) %>%
            mutate(y_dcpo = round(mean(year))) %>%
            ungroup() %>%
            .[["y_dcpo"]]
        }
      } else as.numeric(ds$year)
      t_data$y_dcpo <- as.numeric(t_data$y_dcpo)
      t_data <- t_data %>%
        filter(!is.na(y_dcpo))

      # Get weights
      if (!is.na(ds$wt) & !all(is.na(ds$wt))) {
        if (length(unlist(strsplit(ds$wt, split = " "))) == 1) {
          wt <- as.numeric(with(t_data, get(ds$wt)))
        } else eval(parse(text = ds$wt))
        t_data$wt_dcpo <- wt
        t_data$wt_dcpo[t_data$wt_dcpo > 10] <- 10
        t_data$wt_dcpo <- t_data$wt_dcpo/mean(t_data$wt_dcpo, na.rm = TRUE)
        t_data$wt_dcpo[is.na(t_data$wt_dcpo)] <- 1
        rm(wt)
      } else t_data$wt_dcpo <- 1
    }

    # Get variable of interest
    if (length(unlist(strsplit(v$variable, split = " "))) == 1) {
      t_data$target <- with(t_data, as.numeric(get(v$variable) %>% stringr::str_trim()))
    } else {
      t_data <- t_data %>%
        mutate(target = eval(parse(text = v$variable)))
    }
    vals <- eval(parse(text = v$values))
    t_data$target <- if_else(t_data$target %in% vals, t_data$target, NA_real_)
    options(warn = 2)
    t_data$target <- do.call(dplyr::recode, c(list(t_data$target), setNames(1:length(vals), vals)))
    options(warn = 0)

    # Summarize by country and year
    vars1 <- t_data %>%
      dplyr::select(c_dcpo, y_dcpo, wt_dcpo, target) %>%
      filter(!is.na(target)) %>%
      group_by(c_dcpo, y_dcpo, target) %>%
      summarise(survey = v$survey,
                item = v$item,
                n = mean(wt_dcpo) * length(na.omit(target))) %>%
      ungroup() %>%
      rename(country = c_dcpo,
             year = y_dcpo,
             survey = survey,
             item = item,
             r = target,
             n = n)

    all_sets[[i]] <- vars1
    rm(vars1)
  }

  suppressWarnings(
  all_data <- bind_rows(all_sets)
  )
  rm(list = c("t_data", "ds", "v"))

  all_data2 <- all_data %>%
    group_by(country, year, item, r) %>%
    summarize(n = sum(n),     # When two surveys ask the same question in
              survey = paste0(survey, collapse = ", ")) %>% # the same country-year, add samples together
    ungroup() %>%
    group_by(country) %>%
    mutate(cc_rank = n(),         # number of country-year-items (data-richness)
           year = as.integer(year)) %>%
    ungroup() %>%
    arrange(desc(cc_rank), country, year)

  # Chime
  if(chime) {
    beepr::beep()
  }

  if(file!="") {
    write_csv(all_data2, file)
  }

  return(all_data2)
}


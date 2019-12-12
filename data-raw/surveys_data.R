library(readr)

detach("package:DCPOtools", unload=TRUE)

surveys_data <- read_csv("data/surveys_data.csv",
                         col_type = cols(year = "i", .default = "c"))
save(surveys_data, file = "data/surveys_data.rda")
usethis::use_data(surveys_data, overwrite = TRUE)

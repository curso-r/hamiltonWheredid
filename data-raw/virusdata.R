virusdata <- read.csv("data-raw/csv/virusdata_app.csv")

virusdata <- virusdata %>% 
  dplyr::mutate(
    country = as.character(country),
    country = dplyr::case_when(
      country == "Côte d’Ivoire" ~ "Cote dIvoire",
      country == "Curaçao" ~ "Curacao",
      country == "São Tomé & Príncipe" ~ "Sao Tome & Principe",
      TRUE ~ country
    ),
    country = as.factor(country)
  )

usethis::use_data(virusdata, overwrite = TRUE)
# Côte d’Ivoire
# Curaçao
# São Tomé & Príncipe
# 
# Encoding(levels(english_monarchs$name)) <- "latin1"
# levels(english_monarchs$name) <- iconv(
#   levels(english_monarchs$name), 
#   "latin1", 
#   "UTF-8"
# )
## code to prepare `my_dataset` dataset goes here
merged3 <- read.csv("data-raw/csv/merged3.csv")
usethis::use_data(merged3, overwrite = TRUE)


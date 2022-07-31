## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

testdata = readr::read_file("data-raw/testdata.m")
usethis::use_data(testdata, internal = T)

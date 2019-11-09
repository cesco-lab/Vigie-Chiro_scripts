# Transform as package
# setwd("..")
# usethis::create_package("vigiechiro")

# Hide files
usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("misc")
usethis::use_build_ignore("AppShiny")
usethis::use_build_ignore("functions")
usethis::use_build_ignore("IO")
usethis::use_build_ignore(".git")

# Clean DESCRIPTION
usethis::use_tidy_description()
usethis::use_roxygen_md()

# License
usethis::use_gpl3_license("Yves Bas")

# Unit tests
usethis::use_testthat()
usethis::use_test("f_biodiv_modgis")

# Vignettes
usethis::use_vignette("f_biodiv_modgis")

# CI
# usetihs::install_github("ropenscilabs/tic")
tic::use_tic()

# Development
attachment::att_amend_desc()
devtools::check()

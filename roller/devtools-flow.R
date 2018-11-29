# =====================================================
# Devtools workflow
#library(devtools)
# =====================================================

usethis::create_package("roller")
devtools::document("roller")          # generate documentation
devtools::check_man("roller")         # check documentation
devtools::test("roller")              # run tests
devtools::build_vignettes("roller")   # build vignettes
devtools::build("roller")             # build bundle
devtools::install("roller")           # install package
